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
# For future-match prediction we want each team's LATEST known state, not the
# leakage-safe prior-day values used to train and validate the historical model.
#
# Overall Elo comes from the latest team_elo_on_date in performance_data.qs.
# Current offense / defense Elo comes from the current off_def_elo_ratings.rda
# snapshot and is calculated as the mean of the two partners' player ratings.
# =============================================================================

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

# =============================================================================
# 1. Latest observed overall Elo, federation, and partnership IDs
# =============================================================================
# Every team serves during a normal match, so service rows provide a clean way
# to identify the team represented by federation and team_elo_on_date.
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

# Federation QA remains visible here because the Olympic field enforces a
# maximum of two teams per federation.
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
# 2. Count recent partnership matches
# =============================================================================
# Field eligibility requires at least 15 matches in the most recent 365-day
# window available in the dataset. Count distinct match_id values so each match
# counts once regardless of number of sets or rallies.
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
    activity_eligible = matches_last_365 >= 15L
  )

# =============================================================================
# 3. Attach CURRENT offense and defense Elo
# =============================================================================
# The rally Elo system tracks player-level offense and defense ratings whenever
# player IDs are available. The team rating used by the model is the average of
# the two partners. If player ratings are unavailable, retain the team-level
# rating snapshot as a fallback rather than silently inventing a value.
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
# 4. Build provisional 24-team Olympic field by gender
# =============================================================================
# V0 field rule:
#   1. Team must have played at least 15 matches in the last 365 days.
#   2. Rank eligible teams by most recent overall Elo.
#   3. Maximum two teams per federation.
#   4. Take the top 24 remaining teams for each gender.
# =============================================================================

field_candidates <- current_team_state %>%
  filter(
    activity_eligible,
    !is.na(overall_elo),
    !is.na(federation),
    federation != ""
  ) %>%
  arrange(gender, desc(overall_elo), team) %>%
  group_by(gender, federation) %>%
  mutate(federation_rank = row_number()) %>%
  ungroup() %>%
  filter(federation_rank <= 2L) %>%
  arrange(gender, desc(overall_elo), team)

current_field <- field_candidates %>%
  group_by(gender) %>%
  slice_head(n = 24) %>%
  mutate(seed = row_number()) %>%
  ungroup() %>%
  select(
    gender,
    seed,
    team,
    federation,
    last_observed_date,
    matches_last_365,
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

if (any(field_counts$n_teams < 24L)) {
  warning("At least one gender has fewer than 24 eligible teams after activity and federation filtering.")
}

if (any(is.na(current_field$offense_elo)) || any(is.na(current_field$defense_elo))) {
  warning(
    "At least one selected field team is missing current offense/defense Elo. ",
    "Inspect current_field before simulation."
  )
}

# =============================================================================
# 5. Save downstream inputs
# =============================================================================

dir.create("data", showWarnings = FALSE, recursive = TRUE)
qs_save(current_team_state, "data/current_team_state.qs")
qs_save(current_field, "data/current_field.qs")

# =============================================================================
# 6. QA / inspection
# =============================================================================

cat("\nLA28 provisional current field\n")
cat("==============================\n")
cat("Latest data date: ", format(latest_data_date), "\n", sep = "")
cat("365-day activity window: ", format(activity_window_start), " to ", format(latest_data_date), "\n", sep = "")
cat("Minimum matches for field eligibility: 15\n")
cat("Current team states: ", format(nrow(current_team_state), big.mark = ","), "\n", sep = "")
cat("Activity-eligible teams: ", sum(current_team_state$activity_eligible), "\n", sep = "")
cat("Teams excluded for <15 matches: ", sum(!current_team_state$activity_eligible), "\n", sep = "")
cat("Teams with historical federation conflicts: ", nrow(current_federation_conflicts), "\n", sep = "")
cat("Missing latest federation: ", sum(is.na(current_team_state$federation)), "\n", sep = "")
cat("Missing latest overall Elo: ", sum(is.na(current_team_state$overall_elo)), "\n", sep = "")
cat("Missing current offense Elo: ", sum(is.na(current_team_state$offense_elo)), "\n", sep = "")
cat("Missing current defense Elo: ", sum(is.na(current_team_state$defense_elo)), "\n", sep = "")

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
