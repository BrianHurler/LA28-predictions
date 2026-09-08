library(tidyverse)
library(qs2)

# =============================================================================
# Load model-ready historical matches
# =============================================================================

model_df <- qs_read("data/model_df_ready.qs") %>%
  mutate(
    elo_diff = team_a_elo_pre - team_b_elo_pre,
    off_elo_diff = team_a_offense_elo_pre - team_b_offense_elo_pre,
    def_elo_diff = team_a_defense_elo_pre - team_b_defense_elo_pre
  ) %>%
  arrange(date, match_id)

# =============================================================================
# Chronological train / test split
# =============================================================================
# V0 uses the first 80% of observed dates for training and the final 20% for
# out-of-time testing. We split by unique date so matches from the same date do
# not land on opposite sides of the validation boundary.
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
# Model 0: overall Elo only
# =============================================================================

model_0 <- glm(
  team_a_win ~ elo_diff,
  data = train_df,
  family = binomial()
)

# =============================================================================
# Model 1: overall + offense + defense Elo
# =============================================================================
# Primary V0 research question:
# Does rally-derived offense / defense Elo improve out-of-time prediction beyond
# overall match Elo alone?
# =============================================================================

model_1 <- glm(
  team_a_win ~ elo_diff + off_elo_diff + def_elo_diff,
  data = train_df,
  family = binomial()
)

# =============================================================================
# Out-of-time predictions and scoring
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

test_predictions <- test_df %>%
  mutate(
    p_model_0 = predict(model_0, newdata = test_df, type = "response"),
    p_model_1 = predict(model_1, newdata = test_df, type = "response")
  )

model_scores <- bind_rows(
  score_predictions(test_predictions$team_a_win, test_predictions$p_model_0) %>%
    mutate(model = "Model 0: overall Elo"),
  score_predictions(test_predictions$team_a_win, test_predictions$p_model_1) %>%
    mutate(model = "Model 1: overall + offense + defense Elo")
) %>%
  select(model, everything())

cat("\nOut-of-time performance\n")
cat("=======================\n")
print(model_scores)

# =============================================================================
# Calibration tables
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
