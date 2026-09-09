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
# Load source data
# =============================================================================
# performance_data.qs defines the cleaned match universe and supplies outcomes,
# points, metadata, federation, gender, and daily match Elo.
#
# Local laptop (Mac or Windows): read performance_data.qs from the current
# user's Desktop folder. EC2: download the current performance_data.qs
# object from S3.
#
# rallies_with_off_def_elo.rda is used only for exact pre-match offense/defense
# Elo. It shares BeachData match_id with performance_data.
#
# Path resolution assumes the project was opened via LA28-predictions.Rproj
# (RStudio sets the working directory to the project root on open) and works
# unchanged on both Mac and Windows -- USERPROFILE is used on Windows since
# HOME is not reliably set there.
# =============================================================================

local_performance_data_path <- file.path(
  if (.Platform$OS.type == "windows") Sys.getenv("USERPROFILE") else Sys.getenv("HOME"),
  "Desktop",
  "performance_data.qs"
)

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
    if (!file.exists(local_performance_data_path)) {
      stop(
        "performance_data.qs not found at: ", local_performance_data_path,
        ". Place performance_data.qs on your Desktop, or set on_ec2 workflow instead."
      )
    }
    performance_data <- qs_read(local_performance_data_path)
  }
} else {
  message("performance_data already loaded; skipping reload.")
}

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

first_non_missing_chr <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) NA_character_ else x[[1]]
}

# =============================================================================
# 1. One cleaned performance_data row per rally
# =============================================================================

service_rows_all <- performance_data %>%
  filter(touch_type == "service") %>%
  mutate(date = as.Date(date)) %>%
  arrange(date, match_id, set_num, rally_num)

if (nrow(service_rows_all) == 0) {
  stop("No service rows found in performance_data.qs.")
}

# =============================================================================
# 2. Leakage-free prior-day match Elo
# =============================================================================
# performance_data stores a final daily Elo state. To avoid same-day leakage,
# each match uses the team's rating from its latest STRICTLY EARLIER playing
# date. All matches on the same date therefore share the same pre-day Elo.
#
# On a team's first observed playing date, the Elo system starting value of
# 1500 is used. If a team plays twice on that first observed date, both matches
# use 1500 because same-day updates are intentionally ignored.
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
# 3. Federation QA from cleaned service rows
# =============================================================================

team_federation_history <- service_rows_all %>%
  transmute(
    date,
    team = as.character(touch_team_name),
    federation = as.character(federation)
  ) %>%
  filter(
    !is.na(team), team != "",
    !is.na(federation), federation != ""
  ) %>%
  distinct(team, federation)

team_federation_conflicts <- team_federation_history %>%
  group_by(team) %>%
  summarise(
    n_federations = n_distinct(federation),
    federations = paste(sort(unique(federation)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_federations > 1L) %>%
  arrange(desc(n_federations), team)

if (nrow(team_federation_conflicts) > 0) {
  warning(
    nrow(team_federation_conflicts),
    " teams are associated with multiple federations in performance_data. ",
    "See team_federation_conflicts."
  )
}

# =============================================================================
# 4. One row per cleaned match
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
    team_a_federation = first_non_missing_chr(
      federation[touch_team_name == first(team1_name)]
    ),
    team_b_federation = first_non_missing_chr(
      federation[touch_team_name == first(team2_name)]
    ),
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
# 5. Point differential from cleaned performance_data rally winners
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
# 6. Exact pre-match offense / defense Elo from original rally table
# =============================================================================

rally_elo_base <- rallies_with_off_def_elo %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= min_date) %>%
  semi_join(match_base %>% select(match_id), by = "match_id")

rally_offense_rows <- rally_elo_base %>%
  transmute(
    match_id,
    set_num,
    rally_num,
    team_num = if_else(serving_team_num == 1L, 2L, 1L),
    offense_elo,
    defense_elo = NA_real_
  )

rally_defense_rows <- rally_elo_base %>%
  transmute(
    match_id,
    set_num,
    rally_num,
    team_num = as.integer(serving_team_num),
    offense_elo = NA_real_,
    defense_elo
  )

raw_rally_elos <- bind_rows(
  rally_offense_rows,
  rally_defense_rows
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
# 7. Attach prior-day team Elo
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
# 8. Assemble full match dataframe
# =============================================================================

model_df <- match_base %>%
  left_join(rally_points, by = "match_id") %>%
  left_join(raw_rally_elos, by = "match_id") %>%
  left_join(team_a_prior_elo, by = c("date", "team_a")) %>%
  left_join(team_b_prior_elo, by = c("date", "team_b")) %>%
  transmute(
    date,
    match_id,
    gender,
    tournament_name,
    team_a,
    team_b,
    team_a_federation,
    team_b_federation,
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

if (anyDuplicated(model_df$match_id) > 0) {
  stop("model_df is not one row per match_id.")
}

model_team_federations <- bind_rows(
  model_df %>% transmute(team = team_a, federation = team_a_federation),
  model_df %>% transmute(team = team_b, federation = team_b_federation)
) %>%
  filter(
    !is.na(team), team != "",
    !is.na(federation), federation != ""
  ) %>%
  distinct(team, federation)

model_team_federation_conflicts <- model_team_federations %>%
  group_by(team) %>%
  summarise(
    n_federations = n_distinct(federation),
    federations = paste(sort(unique(federation)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_federations > 1L) %>%
  arrange(desc(n_federations), team)

# =============================================================================
# 9. Temporary model-ready population
# =============================================================================

model_df_ready <- model_df %>%
  filter(
    !is.na(team_a_win),
    !is.na(team_a_elo_pre),
    !is.na(team_b_elo_pre),
    !is.na(team_a_offense_elo_pre),
    !is.na(team_b_offense_elo_pre),
    !is.na(team_a_defense_elo_pre),
    !is.na(team_b_defense_elo_pre)
  )

dir.create("data", showWarnings = FALSE, recursive = TRUE)
qs_save(model_df_ready, "data/model_df_ready.qs")

# =============================================================================
# 10. QA
# =============================================================================

cat("\nLA28 model dataframe QA\n")
cat("=======================\n")
cat("Canonical match source: performance_data.qs\n")
cat("Rally Elo source: rallies_with_off_def_elo.rda\n")
cat("Rows in model_df: ", format(nrow(model_df), big.mark = ","), "\n", sep = "")
cat("Rows in model_df_ready: ", format(nrow(model_df_ready), big.mark = ","), "\n", sep = "")
cat("Rows excluded from temporary model population: ", format(nrow(model_df) - nrow(model_df_ready), big.mark = ","), "\n", sep = "")
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
cat("Missing federation A: ", sum(is.na(model_df$team_a_federation)), "\n", sep = "")
cat("Missing federation B: ", sum(is.na(model_df$team_b_federation)), "\n", sep = "")
cat("Teams with multiple federations (service-row QA): ", nrow(team_federation_conflicts), "\n", sep = "")
cat("Teams with multiple federations (model_df QA): ", nrow(model_team_federation_conflicts), "\n", sep = "")
cat("Missing point differential: ", sum(is.na(model_df$point_differential)), "\n", sep = "")
cat("Team/date Elo inconsistencies: ", nrow(inconsistent_team_day_elos), "\n", sep = "")

if (nrow(model_team_federation_conflicts) > 0) {
  cat("\nFederation conflicts (first 20)\n")
  print(head(model_team_federation_conflicts, 20))
}

cat("\nPrior-day Elo staleness (days since source rating)\n")
cat("Team A median: ", median(as.integer(model_df$date - model_df$team_a_elo_source_date), na.rm = TRUE), "\n", sep = "")
cat("Team B median: ", median(as.integer(model_df$date - model_df$team_b_elo_source_date), na.rm = TRUE), "\n", sep = "")
cat("\nSaved: data/model_df_ready.qs\n")

print(head(model_df_ready, 10))