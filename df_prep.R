library(tidyverse)
library(aws.s3)
library(qs2)

on_ec2 <- file.exists("/sys/hypervisor/uuid") ||
  file.exists("/sys/devices/virtual/dmi/id/product_uuid")

if (!on_ec2) {
  # Running locally (RStudio laptop)
  Sys.setenv(
    AWS_PROFILE = "brian-hurler",
    AWS_DEFAULT_REGION = "us-west-1"
  )
} else {
  # Running on EC2
  # DO NOT set AWS_PROFILE
  Sys.setenv(
    AWS_DEFAULT_REGION = "us-west-1"
  )
}

# =============================================================================
# Load source data
# =============================================================================

save_object(
  object = "v25_structure/v25_matches.rda",
  bucket = "usavbeach",
  file = "v25_matches.rda"
)
load("v25_matches.rda")
matches <- data
rm(data)

save_object(
  object = "rallies_with_off_def_elo.rda",
  bucket = "usavbeach",
  file = "rallies_with_off_def_elo.rda"
)
load("rallies_with_off_def_elo.rda")

save_object(
  object = "elo/long_matches_k_factor_30.rda",
  bucket = "usavbeach",
  file = "long_matches_k_factor_30.rda"
)
load("long_matches_k_factor_30.rda")

min_date = "2023-01-01"

long_matches <- long_matches %>% filter(date > min_date)
rallies_with_off_def_elo <- rallies_with_off_def_elo %>% filter(date >= min_date)
matches <- matches %>% filter(date_from > min_date)

# =============================================================================
# Helpers
# =============================================================================

# Canonical two-player team key.
# Player IDs are placed in TRUE numeric order so the same partnership always
# receives the same key in BeachData and FIVB/VIS, regardless of player order.
pair_key <- function(player1, player2) {
  p1 <- suppressWarnings(as.numeric(player1))
  p2 <- suppressWarnings(as.numeric(player2))

  if_else(
    is.na(p1) | is.na(p2),
    NA_character_,
    paste0(pmin(p1, p2), "|", pmax(p1, p2))
  )
}

# Canonical matchup key. Team orientation does not matter here; the two already
# canonicalized partnership keys are sorted so A-v-B == B-v-A.
matchup_key <- function(pair1, pair2) {
  if_else(
    is.na(pair1) | is.na(pair2),
    NA_character_,
    map2_chr(
      pair1,
      pair2,
      ~ paste(sort(c(.x, .y)), collapse = "__VS__")
    )
  )
}

normalize_gender <- function(x) {
  case_when(
    str_to_lower(as.character(x)) %in% c("female", "women", "woman", "w") ~ "W",
    str_to_lower(as.character(x)) %in% c("male", "men", "man", "m") ~ "M",
    TRUE ~ as.character(x)
  )
}

# Tournament names are retained only for QA. BeachData tournament labels are
# intentionally user-friendly and therefore are NOT used as a required
# cross-system match key.
normalize_tournament <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]", "")
}

first_non_missing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else x[[1]]
}

# =============================================================================
# 1. Build one row per BeachData match that has rally Elo data
#
# Source relationship:
# - matches$match_id and rallies_with_off_def_elo$match_id are the SAME
#   BeachData match ID.
# - matches contains mirrored +/- match IDs. Rally data uses the positive ID,
#   so we retain positive matches only.
# =============================================================================

rally_match_ids <- rallies_with_off_def_elo %>%
  distinct(match_id)

match_base <- matches %>%
  filter(match_id > 0) %>%
  semi_join(rally_match_ids, by = "match_id") %>%
  mutate(
    date = as.Date(match_datetime),
    gender_key = normalize_gender(gender),
    tournament_key = normalize_tournament(tournament_name),
    team_a_pair_key = pair_key(player_id_11, player_id_12),
    team_b_pair_key = pair_key(player_id_21, player_id_22),
    matchup_key = matchup_key(team_a_pair_key, team_b_pair_key),
    team_a_win = case_when(
      team1_score > team2_score ~ 1L,
      team1_score < team2_score ~ 0L,
      TRUE ~ NA_integer_
    )
  )

if (anyDuplicated(match_base$match_id) > 0) {
  stop("match_base is not unique by positive BeachData match_id.")
}

# Within an otherwise identical same-day matchup, BeachData provides an exact
# match time. Tournament is deliberately excluded from the identity because
# BeachData tournament names are presentation-friendly rather than raw FIVB
# labels.
match_base <- match_base %>%
  arrange(date, gender_key, matchup_key, match_datetime, match_id) %>%
  group_by(date, gender_key, matchup_key) %>%
  mutate(matchup_occurrence = row_number()) %>%
  ungroup()

# =============================================================================
# 2. Collapse rally Elo to pre-match team offense / defense Elo
# =============================================================================

rally_team_elos <- bind_rows(
  rallies_with_off_def_elo %>%
    transmute(
      match_id,
      date = as.Date(date),
      set_num,
      rally_num,
      team_num = if_else(serving_team_num == 1L, 2L, 1L),
      offense_elo,
      defense_elo = NA_real_
    ),
  rallies_with_off_def_elo %>%
    transmute(
      match_id,
      date = as.Date(date),
      set_num,
      rally_num,
      team_num = as.integer(serving_team_num),
      offense_elo = NA_real_,
      defense_elo
    )
) %>%
  arrange(match_id, set_num, rally_num) %>%
  group_by(match_id, date, team_num) %>%
  summarise(
    offense_elo_pre = first_non_missing(offense_elo),
    defense_elo_pre = first_non_missing(defense_elo),
    .groups = "drop"
  )

rally_team_elo_wide <- rally_team_elos %>%
  filter(team_num %in% c(1L, 2L)) %>%
  select(match_id, team_num, offense_elo_pre, defense_elo_pre) %>%
  pivot_wider(
    names_from = team_num,
    values_from = c(offense_elo_pre, defense_elo_pre),
    names_glue = "{.value}_team{team_num}"
  ) %>%
  rename(
    team_a_offense_elo_pre = offense_elo_pre_team1,
    team_b_offense_elo_pre = offense_elo_pre_team2,
    team_a_defense_elo_pre = defense_elo_pre_team1,
    team_b_defense_elo_pre = defense_elo_pre_team2
  )

# =============================================================================
# 3. Calculate total point differential from rally winners
# =============================================================================

rally_points <- rallies_with_off_def_elo %>%
  filter(scoring_team_num %in% c(1L, 2L)) %>%
  group_by(match_id) %>%
  summarise(
    team_a_points = sum(scoring_team_num == 1L, na.rm = TRUE),
    team_b_points = sum(scoring_team_num == 2L, na.rm = TRUE),
    point_differential = team_a_points - team_b_points,
    .groups = "drop"
  )

# =============================================================================
# 4. Collapse long_matches to exact FIVB/VIS match + team pre-match Elo
#
# FIVB/VIS match_id does NOT equal BeachData match_id.
# Primary cross-system identity:
#   date + gender + canonical four-player matchup + same-day occurrence
#
# Tournament is retained for QA only and is not required to match.
# =============================================================================

long_matches_keyed <- long_matches %>%
  mutate(
    source_row = row_number(),
    date = as.Date(date),
    gender_key = normalize_gender(gender),
    tournament_key = normalize_tournament(tourn),
    team_pair_key = pair_key(athlete, partner),
    opponent_pair_key = pair_key(opponent1, opponent2),
    matchup_key = matchup_key(team_pair_key, opponent_pair_key)
  )

# One row per FIVB/VIS match, retaining chronological first-row position.
long_match_index <- long_matches_keyed %>%
  group_by(match_id) %>%
  summarise(
    date = first(date),
    gender_key = first(gender_key),
    long_tournament_key = first(tournament_key),
    matchup_key = first(matchup_key),
    long_source_order = min(source_row),
    .groups = "drop"
  ) %>%
  arrange(date, gender_key, matchup_key, long_source_order, match_id) %>%
  group_by(date, gender_key, matchup_key) %>%
  mutate(matchup_occurrence = row_number()) %>%
  ungroup() %>%
  rename(long_match_id = match_id)

# Exact BeachData -> FIVB/VIS match crosswalk.
match_crosswalk <- match_base %>%
  select(
    match_id,
    date,
    gender_key,
    tournament_key,
    matchup_key,
    matchup_occurrence
  ) %>%
  left_join(
    long_match_index,
    by = c(
      "date",
      "gender_key",
      "matchup_key",
      "matchup_occurrence"
    )
  )

if (anyDuplicated(match_crosswalk$match_id) > 0) {
  stop("match_crosswalk is not unique by BeachData match_id.")
}

# Reduce the four athlete rows to one team Elo per team within each exact FIVB
# match. athlete_elo_before is used deliberately to avoid post-match leakage.
long_team_elos <- long_matches_keyed %>%
  group_by(
    long_match_id = match_id,
    team_pair_key
  ) %>%
  summarise(
    team_match_elo_pre = mean(athlete_elo_before, na.rm = TRUE),
    athletes_in_team = n_distinct(athlete),
    .groups = "drop"
  )

bad_long_team_rows <- long_team_elos %>%
  filter(athletes_in_team != 2L)

if (nrow(bad_long_team_rows) > 0) {
  warning(
    nrow(bad_long_team_rows),
    " long_matches match/team groups do not contain exactly two athletes."
  )
}

# =============================================================================
# 5. Attach exact pre-match match Elo to BeachData Team A and Team B
# =============================================================================

match_elos <- match_base %>%
  select(match_id, team_a_pair_key, team_b_pair_key) %>%
  left_join(
    match_crosswalk %>% select(match_id, long_match_id),
    by = "match_id"
  ) %>%
  left_join(
    long_team_elos %>%
      transmute(
        long_match_id,
        team_a_pair_key = team_pair_key,
        team_a_elo_pre = team_match_elo_pre
      ),
    by = c("long_match_id", "team_a_pair_key")
  ) %>%
  left_join(
    long_team_elos %>%
      transmute(
        long_match_id,
        team_b_pair_key = team_pair_key,
        team_b_elo_pre = team_match_elo_pre
      ),
    by = c("long_match_id", "team_b_pair_key")
  ) %>%
  select(match_id, long_match_id, team_a_elo_pre, team_b_elo_pre)

# =============================================================================
# 6. Assemble final modeling dataframe
# =============================================================================

model_df <- match_base %>%
  left_join(rally_points, by = "match_id") %>%
  left_join(rally_team_elo_wide, by = "match_id") %>%
  left_join(match_elos, by = "match_id") %>%
  left_join(
    rallies_with_off_def_elo %>%
      arrange(match_id, set_num, rally_num) %>%
      group_by(match_id) %>%
      summarise(
        team_a = first(team1_name),
        team_b = first(team2_name),
        .groups = "drop"
      ),
    by = "match_id"
  ) %>%
  mutate(
    match_winner = case_when(
      team_a_win == 1L ~ team_a,
      team_a_win == 0L ~ team_b,
      TRUE ~ NA_character_
    )
  ) %>%
  transmute(
    date,
    match_id,
    long_match_id,
    gender,
    tournament_name,
    team_a,
    team_b,
    match_winner,
    team_a_win,
    point_differential,
    team_a_elo_pre,
    team_b_elo_pre,
    team_a_offense_elo_pre,
    team_b_offense_elo_pre,
    team_a_defense_elo_pre,
    team_b_defense_elo_pre
  ) %>%
  arrange(date, match_id)

# =============================================================================
# 7. QA
# =============================================================================

if (anyDuplicated(model_df$match_id) > 0) {
  stop("model_df is not one row per match_id.")
}

cat("\nLA28 model dataframe QA\n")
cat("=======================\n")
cat("Rows: ", format(nrow(model_df), big.mark = ","), "\n", sep = "")
cat("Date range: ", format(min(model_df$date, na.rm = TRUE)), " to ", format(max(model_df$date, na.rm = TRUE)), "\n", sep = "")
cat("Matched to exact FIVB/VIS match: ", sum(!is.na(model_df$long_match_id)), " / ", nrow(model_df), "\n", sep = "")
cat("Missing match Elo A: ", sum(is.na(model_df$team_a_elo_pre)), "\n", sep = "")
cat("Missing match Elo B: ", sum(is.na(model_df$team_b_elo_pre)), "\n", sep = "")
cat("Missing offense Elo A: ", sum(is.na(model_df$team_a_offense_elo_pre)), "\n", sep = "")
cat("Missing offense Elo B: ", sum(is.na(model_df$team_b_offense_elo_pre)), "\n", sep = "")
cat("Missing defense Elo A: ", sum(is.na(model_df$team_a_defense_elo_pre)), "\n", sep = "")
cat("Missing defense Elo B: ", sum(is.na(model_df$team_b_defense_elo_pre)), "\n", sep = "")
cat("Missing point differential: ", sum(is.na(model_df$point_differential)), "\n", sep = "")

# Useful diagnostics if exact-match linkage is incomplete.
unmatched_crosswalk <- match_base %>%
  select(
    match_id,
    match_datetime,
    date,
    gender,
    tournament_name,
    team_a_pair_key,
    team_b_pair_key,
    matchup_key,
    matchup_occurrence
  ) %>%
  anti_join(
    match_crosswalk %>% filter(!is.na(long_match_id)) %>% select(match_id),
    by = "match_id"
  )

cat("Unmatched crosswalk rows: ", nrow(unmatched_crosswalk), "\n", sep = "")

# Core-key duplicate diagnostics. These are worth inspecting if occurrence-based
# matching remains incomplete, because multiple BeachData records can sometimes
# represent the same underlying sporting match.
beachdata_core_duplicates <- match_base %>%
  count(date, gender_key, matchup_key, name = "n_beachdata") %>%
  filter(n_beachdata > 1L)

fivb_core_duplicates <- long_match_index %>%
  count(date, gender_key, matchup_key, name = "n_fivb") %>%
  filter(n_fivb > 1L)

cat("BeachData core keys with >1 match: ", nrow(beachdata_core_duplicates), "\n", sep = "")
cat("FIVB core keys with >1 match: ", nrow(fivb_core_duplicates), "\n", sep = "")

print(head(model_df, 10))
