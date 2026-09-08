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
# Load canonical production dataset
# =============================================================================
#
# performance_data.qs is the canonical cleaned analytical dataset produced by
# the beach pipeline. Upstream cleaning therefore stays in one place rather
# than being reimplemented independently in this repo.
#
# Important Elo convention for this model:
# - performance_data$team_elo_on_date / opp_elo_on_date represent the final
#   daily Elo state attached by the production pipeline.
# - To avoid same-day leakage, match Elo below uses the latest rating from a
#   STRICTLY EARLIER DATE.
# - Therefore all matches played by a team on the same date intentionally use
#   the same pre-day Elo. A second match that day is slightly stale, but never
#   contains information from that day's outcomes.
# =============================================================================

tmp <- tempfile(fileext = ".qs")

save_object(
  object = "performance_data.qs",
  bucket = "usavbeach",
  file = tmp
)

performance_data <- qs_read(tmp)
unlink(tmp)

min_date <- as.Date("2023-01-01")

# =============================================================================
# 1. Keep one row per rally: the service row
# =============================================================================
#
# One service row corresponds to one rally and carries:
# - match / tournament metadata
# - rally winner
# - serving team in touch_team_name
# - receiving team in opponent
# - team_elo_on_date for the serving team
# - opp_elo_on_date for the receiving team
# - offense_elo for the receiving side BEFORE the rally
# - defense_elo for the serving side BEFORE the rally
#
# Using service rows makes the match-level collapse simple and avoids weighting
# rallies by their number of touches.
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
    prior_date = lag(date),
    elo_pre_day = lag(elo_on_date)
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
# 4. Point differential from rally winners
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
# 5. Exact pre-match rally offense / defense Elo
# =============================================================================
#
# On a service row:
# - defense_elo belongs to touch_team_name (the serving team)
# - offense_elo belongs to opponent        (the receiving team)
#
# The first time a team serves in a match gives its pre-match defense Elo.
# The first time a team receives in a match gives its pre-match offense Elo.
# =============================================================================

rally_elos <- service_rows_all %>%
  filter(date >= min_date) %>%
  group_by(match_id) %>%
  summarise(
    team_a = first(team1_name),
    team_b = first(team2_name),

    team_a_offense_elo_pre = first(
      offense_elo[opponent == first(team1_name) & !is.na(offense_elo)],
      default = NA_real_
    ),
    team_b_offense_elo_pre = first(
      offense_elo[opponent == first(team2_name) & !is.na(offense_elo)],
      default = NA_real_
    ),

    team_a_defense_elo_pre = first(
      defense_elo[touch_team_name == first(team1_name) & !is.na(defense_elo)],
      default = NA_real_
    ),
    team_b_defense_elo_pre = first(
      defense_elo[touch_team_name == first(team2_name) & !is.na(defense_elo)],
      default = NA_real_
    ),

    .groups = "drop"
  ) %>%
  select(-team_a, -team_b)

# =============================================================================
# 6. Attach prior-day team Elo
# =============================================================================

team_a_prior_elo <- team_day_elos %>%
  transmute(
    date,
    team_a = team,
    team_a_elo_pre = elo_pre_day,
    team_a_elo_source_date = prior_date
  )

team_b_prior_elo <- team_day_elos %>%
  transmute(
    date,
    team_b = team,
    team_b_elo_pre = elo_pre_day,
    team_b_elo_source_date = prior_date
  )

# =============================================================================
# 7. Assemble final modeling dataframe
# =============================================================================

model_df <- match_base %>%
  left_join(rally_points, by = "match_id") %>%
  left_join(rally_elos, by = "match_id") %>%
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
cat("Source: performance_data.qs\n")
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
cat("Missing prior-day match Elo A: ", sum(is.na(model_df$team_a_elo_pre)), "\n", sep = "")
cat("Missing prior-day match Elo B: ", sum(is.na(model_df$team_b_elo_pre)), "\n", sep = "")
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
