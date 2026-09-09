library(tidyverse)
library(aws.s3)
library(qs2)

on_ec2 <- file.exists("/sys/hypervisor/uuid") ||
  file.exists("/sys/devices/virtual/dmi/id/product_uuid")

if (!on_ec2) {
  Sys.setenv(
    AWS_PROFILE = "brian-hurler",
    AWS_DEFAULT_REGION = "us-west-1"
  )
} else {
  Sys.setenv(AWS_DEFAULT_REGION = "us-west-1")
}

# =============================================================================
# Load source data and production model
# =============================================================================
# Local laptop (Mac or Windows): read performance_data.qs from the current
# user's Desktop folder. Assumes the project was opened via
# LA28-predictions.Rproj (RStudio sets the working directory to the project
# root on open). EC2: download the current performance_data.qs object from S3.
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

model_df <- qs_read("data/model_df_ready.qs")
production_model <- qs_read("data/production_model.qs")

# =============================================================================
# 1. Reconstruct historical set scores from rally winners
# =============================================================================
# One service row represents one rally. Counting rally_winner within each set
# therefore reconstructs the observed rally-point score without relying on a
# separate score field.
# =============================================================================

set_scores <- performance_data %>%
  filter(touch_type == "service") %>%
  mutate(date = as.Date(date)) %>%
  semi_join(model_df %>% select(match_id), by = "match_id") %>%
  group_by(match_id, set_num) %>%
  summarise(
    team_a = first(team1_name),
    team_b = first(team2_name),
    team_a_points = sum(rally_winner == first(team1_name), na.rm = TRUE),
    team_b_points = sum(rally_winner == first(team2_name), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    set_winner = case_when(
      team_a_points > team_b_points ~ team_a,
      team_b_points > team_a_points ~ team_b,
      TRUE ~ NA_character_
    )
  )

# =============================================================================
# 2. Collapse to match scoreline
# =============================================================================

match_scorelines <- set_scores %>%
  group_by(match_id) %>%
  summarise(
    team_a = first(team_a),
    team_b = first(team_b),
    team_a_sets = sum(set_winner == first(team_a), na.rm = TRUE),
    team_b_sets = sum(set_winner == first(team_b), na.rm = TRUE),
    team_a_points = sum(team_a_points, na.rm = TRUE),
    team_b_points = sum(team_b_points, na.rm = TRUE),
    n_sets = n(),
    .groups = "drop"
  )

# =============================================================================
# 3. Attach historical neutral match probabilities
# =============================================================================
# As in 05_pairwise_probabilities.R, predict each historical matchup in both
# orientations and average the two directions so the non-zero Team-A intercept
# does not affect the strength variable used for scoreline matching.
# =============================================================================

historical_scores <- model_df %>%
  transmute(
    match_id,
    date,
    gender,
    team_a,
    team_b,
    match_winner,
    elo_diff = team_a_elo_pre - team_b_elo_pre,
    off_elo_diff = team_a_offense_elo_pre - team_b_offense_elo_pre,
    def_elo_diff = team_a_defense_elo_pre - team_b_defense_elo_pre
  )

historical_scores$p_team_a_raw <- predict(
  production_model,
  newdata = historical_scores %>% select(elo_diff, off_elo_diff, def_elo_diff),
  type = "response"
)

reverse_inputs <- historical_scores %>%
  transmute(
    elo_diff = -elo_diff,
    off_elo_diff = -off_elo_diff,
    def_elo_diff = -def_elo_diff
  )

historical_scores$p_team_b_raw_as_a <- predict(
  production_model,
  newdata = reverse_inputs,
  type = "response"
)

historical_scores <- historical_scores %>%
  mutate(
    p_team_a = (p_team_a_raw + (1 - p_team_b_raw_as_a)) / 2,
    p_team_b = 1 - p_team_a
  ) %>%
  left_join(match_scorelines, by = c("match_id", "team_a", "team_b")) %>%
  mutate(
    winner_probability = case_when(
      match_winner == team_a ~ p_team_a,
      match_winner == team_b ~ p_team_b,
      TRUE ~ NA_real_
    ),
    winner_sets = case_when(
      match_winner == team_a ~ team_a_sets,
      match_winner == team_b ~ team_b_sets,
      TRUE ~ NA_integer_
    ),
    loser_sets = case_when(
      match_winner == team_a ~ team_b_sets,
      match_winner == team_b ~ team_a_sets,
      TRUE ~ NA_integer_
    ),
    winner_points = case_when(
      match_winner == team_a ~ team_a_points,
      match_winner == team_b ~ team_b_points,
      TRUE ~ NA_integer_
    ),
    loser_points = case_when(
      match_winner == team_a ~ team_b_points,
      match_winner == team_b ~ team_a_points,
      TRUE ~ NA_integer_
    ),
    scoreline = paste0(winner_sets, "-", loser_sets),
    winner_probability_bin = cut(
      winner_probability,
      breaks = c(0, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 1),
      labels = c(
        "0.00-0.35",
        "0.35-0.45",
        "0.45-0.55",
        "0.55-0.65",
        "0.65-0.75",
        "0.75-0.85",
        "0.85-1.00"
      ),
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  filter(
    !is.na(winner_probability),
    !is.na(winner_probability_bin),
    winner_sets == 2L,
    loser_sets %in% c(0L, 1L),
    n_sets %in% c(2L, 3L),
    winner_points > loser_points
  ) %>%
  select(
    match_id,
    date,
    gender,
    team_a,
    team_b,
    match_winner,
    winner_probability,
    winner_probability_bin,
    winner_sets,
    loser_sets,
    scoreline,
    winner_points,
    loser_points,
    n_sets
  )

# =============================================================================
# 4. Summary table for QA and simulation fallback
# =============================================================================
# 07_tournament_simulation.R will sample an observed scoreline from the same
# gender and winner-probability band. This preserves the real joint relationship
# between 2-0 / 2-1 outcomes and total rally-point margins.
# =============================================================================

scoreline_summary <- historical_scores %>%
  group_by(gender, winner_probability_bin) %>%
  summarise(
    matches = n(),
    pct_2_0 = mean(loser_sets == 0L),
    pct_2_1 = mean(loser_sets == 1L),
    mean_winner_points = mean(winner_points),
    mean_loser_points = mean(loser_points),
    mean_point_ratio = mean(winner_points / loser_points),
    .groups = "drop"
  )

if (nrow(historical_scores) == 0) {
  stop("No usable historical scorelines were reconstructed.")
}

if (any(historical_scores$winner_sets != 2L)) {
  stop("Historical scoreline library contains a match winner with != 2 sets won.")
}

if (any(!historical_scores$loser_sets %in% c(0L, 1L))) {
  stop("Historical scoreline library contains an invalid loser set count.")
}

if (any(scoreline_summary$matches < 25L)) {
  warning(
    "At least one gender/probability band has fewer than 25 historical matches. ",
    "Inspect scoreline_summary before tournament simulation."
  )
}

# =============================================================================
# Save downstream simulation inputs
# =============================================================================

dir.create("data", showWarnings = FALSE, recursive = TRUE)
qs_save(historical_scores, "data/scoreline_library.qs")
qs_save(scoreline_summary, "data/scoreline_summary.qs")

# =============================================================================
# Inspection output
# =============================================================================

cat("\nLA28 historical scoreline model\n")
cat("===============================\n")
cat("Usable historical scorelines: ", format(nrow(historical_scores), big.mark = ","), "\n", sep = "")
cat("Date range: ", format(min(historical_scores$date)), " to ", format(max(historical_scores$date)), "\n", sep = "")
cat("Scoreline generation method: empirical resampling by gender and neutral winner-probability band\n")

cat("\nOverall scoreline frequencies\n")
print(
  historical_scores %>%
    count(gender, scoreline) %>%
    group_by(gender) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup()
)

cat("\nScoreline library by winner-probability band\n")
print(scoreline_summary, n = Inf)

cat("\nSaved: data/scoreline_library.qs\n")
cat("Saved: data/scoreline_summary.qs\n")
