library(tidyverse)
library(qs2)

# =============================================================================
# Load model-ready historical matches
# =============================================================================

model_df <- qs_read("data/model_df_ready.qs") %>%
  mutate(
    elo_diff = team_a_elo_pre - team_b_elo_pre,
    off_elo_diff = team_a_offense_elo_pre - team_b_offense_elo_pre,
    def_elo_diff = team_a_defense_elo_pre - team_b_defense_elo_pre,
    year = lubridate::year(date)
  ) %>%
  arrange(date, match_id)

# =============================================================================
# Scoring helper
# =============================================================================

score_predictions <- function(actual, probability) {
  probability <- pmin(pmax(probability, 1e-15), 1 - 1e-15)

  tibble(
    log_loss = -mean(
      actual * log(probability) +
        (1 - actual) * log(1 - probability)
    ),
    brier = mean((probability - actual)^2)
  )
}

# =============================================================================
# Chronological 80 / 20 train-test split
# =============================================================================
# V0 first uses the first 80% of observed dates for training and the final 20%
# for out-of-time testing. We split by unique date so matches from the same date
# do not land on opposite sides of the validation boundary.
# =============================================================================

unique_dates <- sort(unique(model_df$date))
split_index <- max(1L, floor(length(unique_dates) * 0.80))
split_date <- unique_dates[[split_index]]

train_df <- model_df %>%
  filter(date <= split_date)

test_df <- model_df %>%
  filter(date > split_date)

cat("\nLA28 model split\n")
cat("================\n")
cat("Split date: ", format(split_date), "\n", sep = "")
cat("Training matches: ", format(nrow(train_df), big.mark = ","), "\n", sep = "")
cat("Testing matches: ", format(nrow(test_df), big.mark = ","), "\n", sep = "")

# =============================================================================
# Four V0 candidate models
# =============================================================================

model_0 <- glm(
  team_a_win ~ elo_diff,
  data = train_df,
  family = binomial()
)

model_off <- glm(
  team_a_win ~ elo_diff + off_elo_diff,
  data = train_df,
  family = binomial()
)

model_def <- glm(
  team_a_win ~ elo_diff + def_elo_diff,
  data = train_df,
  family = binomial()
)

model_1 <- glm(
  team_a_win ~ elo_diff + off_elo_diff + def_elo_diff,
  data = train_df,
  family = binomial()
)

# =============================================================================
# 80 / 20 out-of-time predictions and scoring
# =============================================================================

test_predictions <- test_df %>%
  mutate(
    p_model_0 = predict(model_0, newdata = test_df, type = "response"),
    p_model_off = predict(model_off, newdata = test_df, type = "response"),
    p_model_def = predict(model_def, newdata = test_df, type = "response"),
    p_model_1 = predict(model_1, newdata = test_df, type = "response")
  )

component_scores <- bind_rows(
  score_predictions(test_predictions$team_a_win, test_predictions$p_model_0) %>%
    mutate(model = "Overall Elo"),
  score_predictions(test_predictions$team_a_win, test_predictions$p_model_off) %>%
    mutate(model = "Overall + offense Elo"),
  score_predictions(test_predictions$team_a_win, test_predictions$p_model_def) %>%
    mutate(model = "Overall + defense Elo"),
  score_predictions(test_predictions$team_a_win, test_predictions$p_model_1) %>%
    mutate(model = "Overall + offense + defense Elo")
) %>%
  select(model, everything())

cat("\n80/20 out-of-time performance\n")
cat("=============================\n")
print(component_scores)

# =============================================================================
# Calibration tables for baseline and full model
# =============================================================================

calibration_0 <- test_predictions %>%
  mutate(probability_bin = ntile(p_model_0, 10)) %>%
  group_by(probability_bin) %>%
  summarise(
    n = n(),
    mean_predicted = mean(p_model_0),
    actual_win_rate = mean(team_a_win),
    .groups = "drop"
  )

calibration_1 <- test_predictions %>%
  mutate(probability_bin = ntile(p_model_1, 10)) %>%
  group_by(probability_bin) %>%
  summarise(
    n = n(),
    mean_predicted = mean(p_model_1),
    actual_win_rate = mean(team_a_win),
    .groups = "drop"
  )

cat("\nModel 0 calibration\n")
print(calibration_0)

cat("\nModel 1 calibration\n")
print(calibration_1)

# =============================================================================
# Rolling yearly out-of-time validation
# =============================================================================
# Expanding-window validation:
# - 2024 holdout: train on all matches before 2024, test on 2024
# - 2025 holdout: train on all matches before 2025, test on 2025
# - 2026 holdout: train on all matches before 2026, test on available 2026 data
#
# This mimics real deployment: at each point in time, only prior matches are
# available to fit the model.
# =============================================================================

fit_and_score_year <- function(test_year, data) {

  train_year_df <- data %>%
    filter(year < test_year)

  test_year_df <- data %>%
    filter(year == test_year)

  if (nrow(train_year_df) == 0 || nrow(test_year_df) == 0) {
    return(NULL)
  }

  m0 <- glm(
    team_a_win ~ elo_diff,
    data = train_year_df,
    family = binomial()
  )

  moff <- glm(
    team_a_win ~ elo_diff + off_elo_diff,
    data = train_year_df,
    family = binomial()
  )

  mdef <- glm(
    team_a_win ~ elo_diff + def_elo_diff,
    data = train_year_df,
    family = binomial()
  )

  mfull <- glm(
    team_a_win ~ elo_diff + off_elo_diff + def_elo_diff,
    data = train_year_df,
    family = binomial()
  )

  preds <- test_year_df %>%
    mutate(
      p_overall = predict(m0, newdata = test_year_df, type = "response"),
      p_offense = predict(moff, newdata = test_year_df, type = "response"),
      p_defense = predict(mdef, newdata = test_year_df, type = "response"),
      p_full = predict(mfull, newdata = test_year_df, type = "response")
    )

  bind_rows(
    score_predictions(preds$team_a_win, preds$p_overall) %>%
      mutate(model = "Overall Elo"),
    score_predictions(preds$team_a_win, preds$p_offense) %>%
      mutate(model = "Overall + offense Elo"),
    score_predictions(preds$team_a_win, preds$p_defense) %>%
      mutate(model = "Overall + defense Elo"),
    score_predictions(preds$team_a_win, preds$p_full) %>%
      mutate(model = "Overall + offense + defense Elo")
  ) %>%
    mutate(
      test_year = test_year,
      train_matches = nrow(train_year_df),
      test_matches = nrow(test_year_df),
      train_through = max(train_year_df$date),
      test_start = min(test_year_df$date),
      test_end = max(test_year_df$date)
    ) %>%
    select(
      test_year,
      model,
      train_matches,
      test_matches,
      train_through,
      test_start,
      test_end,
      log_loss,
      brier
    )
}

rolling_scores <- map_dfr(
  c(2024L, 2025L, 2026L),
  fit_and_score_year,
  data = model_df
)

cat("\nRolling yearly out-of-time validation\n")
cat("=====================================\n")
print(rolling_scores)

# =============================================================================
# Improvement versus overall-Elo baseline within each holdout year
# =============================================================================

rolling_lift <- rolling_scores %>%
  group_by(test_year) %>%
  mutate(
    baseline_log_loss = log_loss[model == "Overall Elo"],
    baseline_brier = brier[model == "Overall Elo"],
    log_loss_improvement = baseline_log_loss - log_loss,
    brier_improvement = baseline_brier - brier
  ) %>%
  ungroup() %>%
  select(
    test_year,
    model,
    log_loss,
    log_loss_improvement,
    brier,
    brier_improvement
  )

cat("\nYearly improvement versus overall Elo\n")
cat("=====================================\n")
print(rolling_lift)
