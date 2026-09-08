library(tidyverse)
library(qs2)

# =============================================================================
# Load complete historical modeling population
# =============================================================================

model_df <- qs_read("data/model_df_ready.qs") %>%
  mutate(
    elo_diff = team_a_elo_pre - team_b_elo_pre,
    off_elo_diff = team_a_offense_elo_pre - team_b_offense_elo_pre,
    def_elo_diff = team_a_defense_elo_pre - team_b_defense_elo_pre
  ) %>%
  arrange(date, match_id)

# =============================================================================
# Fit production V0 match-win model
# =============================================================================
# Selected after chronological 80/20 validation and expanding-window yearly
# holdouts showed consistent out-of-time improvement over overall Elo alone.
# =============================================================================

production_model <- glm(
  team_a_win ~ elo_diff + off_elo_diff + def_elo_diff,
  data = model_df,
  family = binomial()
)

cat("\nLA28 production V0 model\n")
cat("========================\n")
cat("Training matches: ", format(nrow(model_df), big.mark = ","), "\n", sep = "")
cat("Training date range: ", format(min(model_df$date)), " to ", format(max(model_df$date)), "\n", sep = "")
cat("\nCoefficients\n")
print(coef(summary(production_model)))

# =============================================================================
# Save fitted model for downstream field and tournament simulation scripts
# =============================================================================

dir.create("data", showWarnings = FALSE, recursive = TRUE)
qs_save(production_model, "data/production_model.qs")

cat("\nSaved: data/production_model.qs\n")
