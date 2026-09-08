library(tidyverse)
library(aws.s3)
library(qs2)

on_ec2 <- file.exists("/sys/hypervisor/uuid") ||
  file.exists("/sys/devices/virtual/dmi/id/product_uuid")

# =============================================================================
# AWS setup
# =============================================================================

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
#
# performance_data.qs is the canonical cleaned analytical dataset produced by
# the beach pipeline. It determines which matches enter this model and supplies
# match metadata, outcomes, points, and daily match Elo.
#
# rallies_with_off_def_elo.rda is used ONLY for rally-based offense / defense
# Elo. It shares BeachData match_id with performance_data, so no cross-system
# match mapping is required.
#
# Match-Elo convention:
# - performance_data$team_elo_on_date / opp_elo_on_date represent the final
#   daily Elo state attached by the production pipeline.
# - To avoid same-day leakage, match Elo below uses the latest rating from a
#   STRICTLY EARLIER DATE.
# - Therefore all matches played by a team on the same date intentionally use
#   the same pre-day Elo. Later matches that day are slightly stale, but never
#   contain information from that day's outcomes.
# - On a team's first observed playing date in performance_data, there is no
#   earlier rating available. Those matches receive the Elo system's starting
#   value of 1500.
# =============================================================================

tmp_performance <- tempfile(fileext = ".qs")
save_object(
  object = "performance_data.qs",
  bucket = "usavbeach",
  file = tmp_performance
)
performance_data <- qs_read(tmp_performance)
unlink(tmp_performance)

tmp_rally_elo <- tempfile(fileext = ".rda")
save_object(
  object = "rallies_with_off_def_elo.rda",
  bucket = "usavbeach",
  file = tmp_rally_elo
)
load(tmp_rally_elo)
unlink(tmp_rally_elo)

min_date <- as.Date("2023-01-01")

# =============================================================================
# Helpers
# =============================================================================

first_non_missing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else x[[1]]
}

# =============================================================================
# 1. Keep one row per rally from performance_data: the service row
# =============================================================================
#
# performance_data is already cleaned upstream. One service row corresponds to
# one rally, which lets us collapse outcomes and points without weighting rallies
# by their number of touches.
# =============================================================================

service_rows_all <- performance_data %>%
  filter(touch_type == "service") %>%
  mutate(date = as.Date(date)) %>%
  arrange(date, match_id, set_num, rally_num)

if (nrow(service_rows_all) == 0) {
  stop("No service rows found in performance_data.qs.")
}

# =============================================================================
# 2. Build leakage-free prior-day match Elo
# =============================================================================
#
# performance_data contains both sides' daily Elo on every service row:
# - touch_team_name / team_elo_on_date = serving side
# - opponent / opp_elo_on_date         = receiving side
#
# First create one team rating per team/date. Then lag within team. Because the
# lag is across DISTINCT dates, every match on a given day receives the rating
# from that team's most recent earlier playing date.
#
# On the first observed playing date for a team, elo_pre_day is set to 1500.
# This is intentionally a first-DAY flag: if the same team plays twice on its
# first observed date, both matches use 1500 because our leakage-free convention
# ignores all same-day Elo movement.
# =============================================================================

team_day_elos <- bind_rows(
  service_rows_all %>%
    transmute(
      date,
      team = as.character(touch_team_name),
      elo_on_date = as.numeric(team_elo_on_date)
    ),
  service_rows_all %>%
    transmute(
      date,
      team = as.character(opponent),
      elo_on_date = as.numeric(opp_elo_on_date)
    )
) %>%
  filter(!is.na(team), team != "", !is.na(elo_on_date)) %>%
  group_by(team, date) %>%
  summarise(
    elo_on_date = first(elo_on_date),
    n_distinct_elos = n_distinct(elo_on_date),
    .groups = "drop"
  )

inconsistent_team_day_elos <- team_day_elos %>%
  filter(n_distinct_elos > 1L)

if (nrow(inconsistent_team_day_elos) > 0) {
  warning(
    nrow(inconsistent_team_day_elos),
    " team/date combinations contain multiple Elo values in performance_data."
  )
}

team_day_elos <- team_day_elos %>%
  arrange(team, date) %>%
  group_by(team) %>%
  mutate(
    first_observed_date = first(date),
    first_observed_day = date == first_observed_date,
    prior_date = lag(date),
    elo_pre_day = lag(elo_on_date),
    elo_pre_day = if_else(first_observed_day, 1500, elo_pre_day)
  ) %>%
  ungroup()

# =============================================================================
# 3. Build one row per cleaned match
# =============================================================================

match_base <- service_rows_all %>%
  filter(date >= min_date) %>%
  group_by(match_id) %>%
  summarise(
    date = first(date),
    gender = first(gender),
    tournament_name = first(tournament_name),
    team_a = first(team1_name),
    team_b = first(team2_name),
    match_winner = first(match_winner),
    .groups = "drop"
  ) %>%
  mutate(
    team_a_win = case_when(
      match_winner == team_a ~ 1L,
      match_winner == team_b ~ 0L,
      TRUE ~ NA_integer_
    )
  )

if (anyDuplicated(match_base$match_id) > 0) {
  stop("match_base is not one row per match_id.")
}

# =============================================================================
# 4. Point differential from cleaned performance_data rally winners
# =============================================================================
#
# Because service_rows_all has exactly one row per rally, counting rally winners
# is equivalent to counting points won.
# =============================================================================

rally_points <- service_rows_all %>%
  filter(date >= min_date) %>%
  group_by(match_id) %>%
  summarise(
    team_a_points = sum(rally_winner == first(team1_name), na.rm = TRUE),
    team_b_points = sum(rally_winner == first(team2_name), na.rm = TRUE),
    point_differential = team_a_points - team_b_points,
    .groups = "drop"
  )

# =============================================================================
# 5. Exact pre-match rally offense / defense Elo from original rally table
# =============================================================================
#
# rallies_with_off_def_elo stores one row per rally:
# - offense_elo = receiving side's offense Elo BEFORE that rally
# - defense_elo = serving side's defense Elo BEFORE that rally
#
# Offense Elo changes only when a team receives; defense Elo changes only when a
# team serves. Therefore the first observed value for each team/role inside a
# match is still that team's true pre-match rating for that component.
#
# performance_data determines the match universe. The raw rally-Elo table only
# contributes these four predictor fields through the shared BeachData match_id.
# =============================================================================

raw_rally_elos <- rallies_with_off_def_elo %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= min_date) %>%
  semi_join(match_base %>% select(match_id), by = "match_id") %>%
  bind_rows(
    . %>%
      transmute(
        match_id,
        set_num,
        rally_num,
        team_num = if_else(serving_team_num == 1L, 2L, 1L),
        offense_elo,
        defense_elo = NA_real_
      ),
    . %>%
      transmute(
        match_id,
        set_num,
        rally_num,
        team_num = as.integer(serving_team_num),
        offense_elo = NA_real_,
        defense_elo
      )
  ) %>%
  arrange(match_id, set_num, rally_num) %>%
  group_by(match_id, team_num) %>%
  summarise(
    offense_elo_pre = first_non_missing(offense_elo),
    defense_elo_pre = first_non_missing(defense_elo),
    .groups = "drop"
  ) %>%
  filter(team_num %in% c(1L, 2L)) %>%
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
# 6. Attach prior-day team Elo
# =============================================================================

team_a_prior_elo <- team_day_elos %>%
  transmute(
    date,
    team_a = team,
    team_a_elo_pre = elo_pre_day,
    team_a_elo_source_date = prior_date,
    team_a_first_observed_day = first_observed_day
  )

team_b_prior_elo <- team_day_elos %>%
  transmute(
    date,
    team_b = team,
    team_b_elo_pre = elo_pre_day,
    team_b_elo_source_date = prior_date,
    team_b_first_observed_day = first_observed_day
  )

# =============================================================================
# 7. Assemble final modeling dataframe
# =============================================================================

model_df <- match_base %>%
  left_join(rally_points, by = "match_id") %>%
  left_join(raw_rally_elos, by = "match_id") %>%
  left_join(
    team_a_prior_elo,
    by = c("date", "team_a")
  ) %>%
  left_join(
    team_b_prior_elo,
    by = c("date", "team_b")
  ) %>%
  transmute(
    date,
    match_id,
    gender,
    tournament_name,
    team_a,
    team_b,
    match_winner,
    team_a_win,
    point_differential,
    team_a_elo_pre,
    team_b_elo_pre,
    team_a_elo_source_date,
    team_b_elo_source_date,
    team_a_first_observed_day,
    team_b_first_observed_day,
    team_a_offense_elo_pre,
    team_b_offense_elo_pre,
    team_a_defense_elo_pre,
    team_b_defense_elo_pre
  ) %>%
  arrange(date, match_id)

# =============================================================================
# 8. QA
# =============================================================================

if (anyDuplicated(model_df$match_id) > 0) {
  stop("model_df is not one row per match_id.")
}

cat("\nLA28 model dataframe QA\n")
cat("=======================\n")
cat("Canonical match source: performance_data.qs\n")
cat("Rally Elo source: rallies_with_off_def_elo.rda\n")
cat("Rows: ", format(nrow(model_df), big.mark = ","), "\n", sep = "")
cat(
  "Date range: ",
  format(min(model_df$date, na.rm = TRUE)),
  " to ",
  format(max(model_df$date, na.rm = TRUE)),
  "\n",
  sep = ""
)
cat("Missing outcome: ", sum(is.na(model_df$team_a_win)), "\n", sep = "")
cat("Team A first-observed-day rows (Elo = 1500): ", sum(model_df$team_a_first_observed_day, na.rm = TRUE), "\n", sep = "")
cat("Team B first-observed-day rows (Elo = 1500): ", sum(model_df$team_b_first_observed_day, na.rm = TRUE), "\n", sep = "")
cat("Missing prior-day match Elo A after first-day initialization: ", sum(is.na(model_df$team_a_elo_pre)), "\n", sep = "")
cat("Missing prior-day match Elo B after first-day initialization: ", sum(is.na(model_df$team_b_elo_pre)), "\n", sep = "")
cat("Missing offense Elo A: ", sum(is.na(model_df$team_a_offense_elo_pre)), "\n", sep = "")
cat("Missing offense Elo B: ", sum(is.na(model_df$team_b_offense_elo_pre)), "\n", sep = "")
cat("Missing defense Elo A: ", sum(is.na(model_df$team_a_defense_elo_pre)), "\n", sep = "")
cat("Missing defense Elo B: ", sum(is.na(model_df$team_b_defense_elo_pre)), "\n", sep = "")
cat("Missing point differential: ", sum(is.na(model_df$point_differential)), "\n", sep = "")
cat("Team/date Elo inconsistencies: ", nrow(inconsistent_team_day_elos), "\n", sep = "")

cat("\nPrior-day Elo staleness (days since source rating)\n")
cat("Team A median: ", median(as.integer(model_df$date - model_df$team_a_elo_source_date), na.rm = TRUE), "\n", sep = "")
cat("Team B median: ", median(as.integer(model_df$date - model_df$team_b_elo_source_date), na.rm = TRUE), "\n", sep = "")

print(head(model_df, 10))
