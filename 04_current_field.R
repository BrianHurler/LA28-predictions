library(tidyverse)
library(aws.s3)
library(qs2)

on_ec2 <- file.exists("/sys/hypervisor/uuid") ||
  file.exists("/sys/devices/virtual/dmi/id/product_uuid")

# =============================================================================
# AWS setup
# =============================================================================

if (!on_ec2) {
  Sys.setenv(
    AWS_PROFILE = "brian-hurler",
    AWS_DEFAULT_REGION = "us-west-1"
  )
} else {
  Sys.setenv(AWS_DEFAULT_REGION = "us-west-1")
}

# =============================================================================
# Load current production data
# =============================================================================

if (!exists("performance_data")) {
  if (on_ec2) {
    tmp_performance <- tempfile(fileext = ".qs")
    save_object(
      object = "performance_data.qs",
      bucket = "usavbeach",
      file = tmp_performance
    )
    performance_data <- qs_read(tmp_performance)
    unlink(tmp_performance)
  } else {
    performance_data <- qs_read("/Users/brianhurler/Desktop/performance_data.qs")
  }
} else {
  message("performance_data already loaded; skipping reload.")
}

tmp_ratings <- tempfile(fileext = ".rda")
save_object(
  object = "off_def_elo_ratings.rda",
  bucket = "usavbeach",
  file = tmp_ratings
)
load(tmp_ratings)
unlink(tmp_ratings)

if (!exists("ratings")) {
  stop("off_def_elo_ratings.rda did not contain an object named ratings.")
}

# =============================================================================
# Helpers
# =============================================================================

last_non_missing_chr <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) NA_character_ else x[[length(x)]]
}

last_non_missing_num <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else x[[length(x)]]
}

normalize_team_label <- function(x) {
  x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]", "")
}

# Greedy duplicate-athlete resolver. Input must already be ordered by priority.
# A partnership is kept only if neither athlete ID has already been used.
remove_duplicate_athletes <- function(df, used_player_ids = character()) {
  if (nrow(df) == 0) return(df)

  keep <- logical(nrow(df))
  used <- used_player_ids[!is.na(used_player_ids) & used_player_ids != ""]

  for (i in seq_len(nrow(df))) {
    ids <- c(df$player1_id[[i]], df$player2_id[[i]])
    ids <- ids[!is.na(ids) & ids != ""]

    if (!any(ids %in% used)) {
      keep[[i]] <- TRUE
      used <- unique(c(used, ids))
    }
  }

  df[keep, , drop = FALSE]
}

# =============================================================================
# 1. Latest observed overall Elo, federation, and partnership IDs
# =============================================================================

service_rows <- performance_data %>%
  filter(touch_type == "service") %>%
  mutate(date = as.Date(date)) %>%
  arrange(date, match_id, set_num, rally_num)

if (nrow(service_rows) == 0) {
  stop("No service rows found in performance_data.qs.")
}

latest_data_date <- max(service_rows$date, na.rm = TRUE)
activity_window_start <- latest_data_date - 364
active_since_date <- as.Date("2026-01-01")

team_service_rows <- service_rows %>%
  mutate(
    team = as.character(touch_team_name),
    team_id = case_when(
      team == team1_name ~ as.character(team1_id),
      team == team2_name ~ as.character(team2_id),
      TRUE ~ NA_character_
    ),
    player1_id = case_when(
      team == team1_name ~ as.character(player_id_11),
      team == team2_name ~ as.character(player_id_21),
      TRUE ~ NA_character_
    ),
    player2_id = case_when(
      team == team1_name ~ as.character(player_id_12),
      team == team2_name ~ as.character(player_id_22),
      TRUE ~ NA_character_
    ),
    federation = as.character(federation),
    overall_elo = as.numeric(team_elo_on_date)
  ) %>%
  filter(!is.na(team), team != "")

current_federation_conflicts <- team_service_rows %>%
  filter(!is.na(federation), federation != "") %>%
  distinct(team, federation) %>%
  group_by(team) %>%
  summarise(
    n_federations = n_distinct(federation),
    federations = paste(sort(unique(federation)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_federations > 1L) %>%
  arrange(desc(n_federations), team)

current_team_state <- team_service_rows %>%
  group_by(gender, team) %>%
  summarise(
    last_observed_date = max(date, na.rm = TRUE),
    team_id = last_non_missing_chr(team_id),
    player1_id = last_non_missing_chr(player1_id),
    player2_id = last_non_missing_chr(player2_id),
    federation = last_non_missing_chr(federation),
    overall_elo = last_non_missing_num(overall_elo),
    .groups = "drop"
  )

# =============================================================================
# 2. Count recent partnership matches and define activity eligibility
# =============================================================================
# General field eligibility requires BOTH:
#   - at least 15 distinct matches in the latest 365-day window; and
#   - at least one observed match on or after 2026-01-01.
# Known continental-berth teams bypass both activity requirements.
# =============================================================================

team_match_history <- service_rows %>%
  distinct(match_id, date, gender, team1_name, team2_name) %>%
  transmute(
    match_id,
    date,
    gender,
    team_a = as.character(team1_name),
    team_b = as.character(team2_name)
  ) %>%
  pivot_longer(
    cols = c(team_a, team_b),
    names_to = "side",
    values_to = "team"
  ) %>%
  filter(!is.na(team), team != "") %>%
  distinct(match_id, date, gender, team)

recent_match_counts <- team_match_history %>%
  filter(
    date >= activity_window_start,
    date <= latest_data_date
  ) %>%
  count(gender, team, name = "matches_last_365")

current_team_state <- current_team_state %>%
  left_join(recent_match_counts, by = c("gender", "team")) %>%
  mutate(
    matches_last_365 = replace_na(matches_last_365, 0L),
    played_in_2026 = last_observed_date >= active_since_date,
    activity_eligible = matches_last_365 >= 15L & played_in_2026
  )

# =============================================================================
# 3. Attach CURRENT offense and defense Elo
# =============================================================================

player_ratings <- ratings %>%
  filter(kind == "player") %>%
  transmute(
    player_id = as.character(id),
    offensive_elo = as.numeric(offensive_elo),
    defensive_elo = as.numeric(defensive_elo)
  )

team_ratings <- ratings %>%
  filter(kind == "team") %>%
  transmute(
    team_id = as.character(id),
    team_offensive_elo = as.numeric(offensive_elo),
    team_defensive_elo = as.numeric(defensive_elo)
  )

current_team_state <- current_team_state %>%
  left_join(
    player_ratings %>%
      rename(
        player1_id = player_id,
        player1_offensive_elo = offensive_elo,
        player1_defensive_elo = defensive_elo
      ),
    by = "player1_id"
  ) %>%
  left_join(
    player_ratings %>%
      rename(
        player2_id = player_id,
        player2_offensive_elo = offensive_elo,
        player2_defensive_elo = defensive_elo
      ),
    by = "player2_id"
  ) %>%
  left_join(team_ratings, by = "team_id") %>%
  mutate(
    offense_elo = case_when(
      !is.na(player1_offensive_elo) & !is.na(player2_offensive_elo) ~
        (player1_offensive_elo + player2_offensive_elo) / 2,
      !is.na(team_offensive_elo) ~ team_offensive_elo,
      TRUE ~ NA_real_
    ),
    defense_elo = case_when(
      !is.na(player1_defensive_elo) & !is.na(player2_defensive_elo) ~
        (player1_defensive_elo + player2_defensive_elo) / 2,
      !is.na(team_defensive_elo) ~ team_defensive_elo,
      TRUE ~ NA_real_
    ),
    rally_elo_source = case_when(
      !is.na(player1_offensive_elo) & !is.na(player2_offensive_elo) &
        !is.na(player1_defensive_elo) & !is.na(player2_defensive_elo) ~
        "player average",
      !is.na(team_offensive_elo) & !is.na(team_defensive_elo) ~
        "team fallback",
      TRUE ~ "missing"
    )
  ) %>%
  select(
    gender,
    team,
    federation,
    last_observed_date,
    matches_last_365,
    played_in_2026,
    activity_eligible,
    team_id,
    player1_id,
    player2_id,
    overall_elo,
    offense_elo,
    defense_elo,
    rally_elo_source
  )

# =============================================================================
# 4. Known continental berths
# =============================================================================

continental_berths_existing <- tribble(
  ~berth_team,                     ~expected_gender, ~expected_federation,
  "Andre/Renato",                 "male",          "BRA",
  "Victoria/Thamela",             "female",        "BRA",
  "Stam/Schoon",                  "female",        "NED",
  "Andersson, E/Hölting Nilsson", "male",          "SWE",
  "Nicolaidis/Carracher",         "male",          "AUS",
  "Clancy/Fejes",                 "female",        "AUS",
  "Pamela/Esther M",              "female",        "NGR"
) %>%
  mutate(team_key = normalize_team_label(berth_team))

continental_lookup <- current_team_state %>%
  mutate(team_key = normalize_team_label(team)) %>%
  select(team_key, everything())

continental_berth_qa <- continental_berths_existing %>%
  left_join(continental_lookup, by = "team_key") %>%
  mutate(
    found = !is.na(team),
    federation_source = case_when(
      !is.na(federation) & federation != "" ~ "data",
      found ~ "known berth override",
      TRUE ~ NA_character_
    ),
    federation = if_else(
      found & (is.na(federation) | federation == ""),
      expected_federation,
      federation
    ),
    gender_ok = found & gender == expected_gender,
    federation_ok = found & federation == expected_federation,
    overall_elo_ok = found & !is.na(overall_elo),
    offense_elo_ok = found & !is.na(offense_elo),
    defense_elo_ok = found & !is.na(defense_elo),
    qa_pass = found & gender_ok & federation_ok & overall_elo_ok &
      offense_elo_ok & defense_elo_ok
  )

if (anyDuplicated(continental_berth_qa$berth_team) > 0) {
  stop("Continental berth lookup produced duplicate rows for a requested team.")
}

if (any(!continental_berth_qa$qa_pass)) {
  cat("\nContinental berth QA FAILED\n")
  print(
    continental_berth_qa %>%
      select(
        berth_team,
        expected_gender,
        team,
        gender,
        federation,
        expected_federation,
        federation_source,
        overall_elo,
        offense_elo,
        defense_elo,
        found,
        gender_ok,
        federation_ok,
        overall_elo_ok,
        offense_elo_ok,
        defense_elo_ok,
        qa_pass
      ),
    n = Inf
  )
  stop("At least one known continental-berth team failed dataset/Elo/federation QA.")
}

locked_existing <- continental_berth_qa %>%
  transmute(
    gender,
    team,
    federation,
    last_observed_date,
    matches_last_365,
    played_in_2026,
    activity_eligible,
    team_id,
    player1_id,
    player2_id,
    overall_elo,
    offense_elo,
    defense_elo,
    rally_elo_source,
    continental_berth = TRUE,
    berth_source = if_else(
      federation_source == "known berth override",
      "known continental berth - federation override",
      "known continental berth"
    )
  )

locked_morocco <- tibble(
  gender = "male",
  team = "Elgraoui/El Gharouti",
  federation = "MAR",
  last_observed_date = as.Date(NA),
  matches_last_365 = 0L,
  played_in_2026 = FALSE,
  activity_eligible = FALSE,
  team_id = NA_character_,
  player1_id = "150074",
  player2_id = "161541",
  overall_elo = 1500,
  offense_elo = 1500,
  defense_elo = 1500,
  rally_elo_source = "hard-coded 1500",
  continental_berth = TRUE,
  berth_source = "known continental berth - manual team"
)

locked_berths <- bind_rows(locked_existing, locked_morocco)

locked_athletes <- locked_berths %>%
  select(player1_id, player2_id) %>%
  unlist(use.names = FALSE) %>%
  as.character() %>%
  discard(~ is.na(.x) || .x == "") %>%
  unique()

locked_federation_counts <- locked_berths %>%
  count(gender, federation, name = "locked_federation_teams")

if (any(locked_federation_counts$locked_federation_teams > 2L)) {
  warning("Known continental berths exceed the two-team federation cap for at least one gender/federation.")
}

# =============================================================================
# 5. Build provisional 24-team Olympic field by gender
# =============================================================================
# Field rule:
#   1. Lock all known continental berths into the field.
#   2. For all other teams, require >=15 matches in the last 365 days.
#   3. For all other teams, require at least one match on/after 2026-01-01.
#   4. Remove duplicate-athlete partnerships, keeping the more recently active
#      partnership by last_observed_date. Locked berth athletes are protected.
#   5. Known berths count toward the maximum of two teams per federation.
#   6. Fill remaining slots by latest overall Elo.
#   7. Seed the completed field by overall Elo.
# =============================================================================

locked_keys <- locked_existing %>%
  transmute(gender, team_key = normalize_team_label(team))

regular_candidates_pre_dedupe <- current_team_state %>%
  mutate(team_key = normalize_team_label(team)) %>%
  anti_join(locked_keys, by = c("gender", "team_key")) %>%
  filter(
    activity_eligible,
    !is.na(overall_elo),
    !is.na(federation),
    federation != ""
  ) %>%
  arrange(gender, desc(last_observed_date), desc(overall_elo), team)

regular_candidates_deduped <- map_dfr(c("female", "male"), function(g) {
  regular_candidates_pre_dedupe %>%
    filter(gender == g) %>%
    remove_duplicate_athletes(used_player_ids = locked_athletes)
})

duplicate_athlete_drops <- regular_candidates_pre_dedupe %>%
  anti_join(
    regular_candidates_deduped %>% select(gender, team),
    by = c("gender", "team")
  ) %>%
  arrange(gender, desc(last_observed_date), team)

regular_candidates <- regular_candidates_deduped %>%
  left_join(locked_federation_counts, by = c("gender", "federation")) %>%
  mutate(
    locked_federation_teams = replace_na(locked_federation_teams, 0L),
    federation_slots_remaining = pmax(0L, 2L - locked_federation_teams)
  ) %>%
  arrange(gender, federation, desc(overall_elo), team) %>%
  group_by(gender, federation) %>%
  mutate(federation_candidate_rank = row_number()) %>%
  ungroup() %>%
  filter(federation_candidate_rank <= federation_slots_remaining) %>%
  transmute(
    gender,
    team,
    federation,
    last_observed_date,
    matches_last_365,
    played_in_2026,
    activity_eligible,
    team_id,
    player1_id,
    player2_id,
    overall_elo,
    offense_elo,
    defense_elo,
    rally_elo_source,
    continental_berth = FALSE,
    berth_source = NA_character_
  )

field_by_gender <- map_dfr(c("female", "male"), function(g) {
  locked_g <- locked_berths %>% filter(gender == g)
  regular_g <- regular_candidates %>%
    filter(gender == g) %>%
    arrange(desc(overall_elo), team)

  n_regular_needed <- 24L - nrow(locked_g)

  if (n_regular_needed < 0L) {
    stop("More than 24 locked berth teams found for gender: ", g)
  }

  bind_rows(
    locked_g,
    regular_g %>% slice_head(n = n_regular_needed)
  )
})

current_field <- field_by_gender %>%
  arrange(gender, desc(overall_elo), team) %>%
  group_by(gender) %>%
  mutate(seed = row_number()) %>%
  ungroup() %>%
  select(
    gender,
    seed,
    team,
    federation,
    continental_berth,
    berth_source,
    last_observed_date,
    matches_last_365,
    played_in_2026,
    activity_eligible,
    overall_elo,
    offense_elo,
    defense_elo,
    rally_elo_source,
    team_id,
    player1_id,
    player2_id
  )

field_counts <- current_field %>%
  count(gender, name = "n_teams")

final_field_athletes <- current_field %>%
  select(gender, team, player1_id, player2_id) %>%
  pivot_longer(
    cols = c(player1_id, player2_id),
    names_to = "player_slot",
    values_to = "player_id"
  ) %>%
  filter(!is.na(player_id), player_id != "")

final_duplicate_athletes <- final_field_athletes %>%
  count(gender, player_id, name = "n_teams") %>%
  filter(n_teams > 1L)

if (nrow(final_duplicate_athletes) > 0) {
  stop("Duplicate athlete IDs remain in the final field. Inspect final_duplicate_athletes.")
}

if (any(field_counts$n_teams < 24L)) {
  warning("At least one gender has fewer than 24 teams after berth, activity, duplicate-athlete, and federation rules.")
}

if (any(is.na(current_field$offense_elo)) || any(is.na(current_field$defense_elo))) {
  warning("At least one selected field team is missing current offense/defense Elo.")
}

# =============================================================================
# 6. Save downstream inputs
# =============================================================================

dir.create("data", showWarnings = FALSE, recursive = TRUE)
qs_save(current_team_state, "data/current_team_state.qs")
qs_save(current_field, "data/current_field.qs")

# =============================================================================
# 7. QA / inspection
# =============================================================================

cat("\nLA28 provisional current field\n")
cat("==============================\n")
cat("Latest data date: ", format(latest_data_date), "\n", sep = "")
cat("365-day activity window: ", format(activity_window_start), " to ", format(latest_data_date), "\n", sep = "")
cat("Minimum matches for non-berth field eligibility: 15\n")
cat("Non-berth teams must have played on/after: ", format(active_since_date), "\n", sep = "")
cat("Current team states: ", format(nrow(current_team_state), big.mark = ","), "\n", sep = "")
cat("15-match eligible teams: ", sum(current_team_state$matches_last_365 >= 15L), "\n", sep = "")
cat("Teams with a 2026 match: ", sum(current_team_state$played_in_2026, na.rm = TRUE), "\n", sep = "")
cat("Fully activity-eligible teams: ", sum(current_team_state$activity_eligible, na.rm = TRUE), "\n", sep = "")
cat("Known continental berths locked: ", nrow(locked_berths), "\n", sep = "")
cat("Teams dropped for duplicate athlete ID: ", nrow(duplicate_athlete_drops), "\n", sep = "")
cat("Teams with historical federation conflicts: ", nrow(current_federation_conflicts), "\n", sep = "")

cat("\nKnown continental berth QA\n")
print(
  continental_berth_qa %>%
    select(
      berth_team,
      team,
      gender,
      federation,
      expected_federation,
      federation_source,
      matches_last_365,
      last_observed_date,
      overall_elo,
      offense_elo,
      defense_elo,
      qa_pass
    ),
  n = Inf
)

cat("\nManual continental berth\n")
print(
  locked_morocco %>%
    select(gender, team, federation, overall_elo, offense_elo, defense_elo)
)

cat("\nDuplicate-athlete partnership drops\n")
if (nrow(duplicate_athlete_drops) == 0) {
  cat("None\n")
} else {
  print(
    duplicate_athlete_drops %>%
      select(
        gender,
        team,
        federation,
        last_observed_date,
        matches_last_365,
        overall_elo,
        player1_id,
        player2_id
      ),
    n = Inf
  )
}

cat("\nFinal duplicate-athlete QA\n")
cat("Duplicate athlete IDs remaining: ", nrow(final_duplicate_athletes), "\n", sep = "")

cat("\nField counts\n")
print(field_counts)

if (nrow(current_federation_conflicts) > 0) {
  cat("\nFederation conflicts (first 20)\n")
  print(head(current_federation_conflicts, 20))
}

cat("\nSelected provisional field\n")
print(
  current_field %>%
    select(
      gender,
      seed,
      team,
      federation,
      continental_berth,
      last_observed_date,
      matches_last_365,
      overall_elo,
      offense_elo,
      defense_elo,
      rally_elo_source
    ),
  n = Inf
)

cat("\nSaved: data/current_team_state.qs\n")
cat("Saved: data/current_field.qs\n")
