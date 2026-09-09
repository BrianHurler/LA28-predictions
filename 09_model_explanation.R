library(tidyverse)
library(qs2)

# =============================================================================
# Purpose
# =============================================================================
# Run after 05_pairwise_probabilities.R and, ideally, after 08_simulation_qa.R.
#
# This script explains WHY the production model ranks current LA28 teams the way
# it does. In particular, it decomposes each team's model strength into the
# contribution from:
#   1. overall Elo
#   2. offense Elo
#   3. defense Elo
#
# The production model is fit on DIFFERENCES between Team A and Team B. Because
# only rating differences matter, raw beta * Elo values are not very meaningful
# on their own. We therefore center each rating within gender and report each
# component's contribution relative to the average team in the current field.
#
# Positive contribution = pushes the team ABOVE the field-average model strength.
# Negative contribution = pushes the team BELOW the field-average model strength.
# =============================================================================

# =============================================================================
# Load inputs
# =============================================================================

current_field <- qs_read("data/current_field.qs")
production_model <- qs_read("data/production_model.qs")
pairwise_probabilities <- qs_read("data/pairwise_probabilities.qs")

simulation_summary <- if (file.exists("data/simulation_summary.qs")) {
  qs_read("data/simulation_summary.qs")
} else {
  NULL
}

required_field_cols <- c(
  "gender", "seed", "team", "federation",
  "overall_elo", "offense_elo", "defense_elo"
)

missing_field_cols <- setdiff(required_field_cols, names(current_field))
if (length(missing_field_cols) > 0L) {
  stop(
    "current_field is missing required columns: ",
    paste(missing_field_cols, collapse = ", ")
  )
}

# =============================================================================
# 1. Extract model coefficients
# =============================================================================

beta <- coef(production_model)
required_coefs <- c("(Intercept)", "elo_diff", "off_elo_diff", "def_elo_diff")
missing_coefs <- setdiff(required_coefs, names(beta))

if (length(missing_coefs) > 0L) {
  stop(
    "production_model is missing expected coefficients: ",
    paste(missing_coefs, collapse = ", ")
  )
}

b_intercept <- unname(beta[["(Intercept)"]])
b_overall <- unname(beta[["elo_diff"]])
b_offense <- unname(beta[["off_elo_diff"]])
b_defense <- unname(beta[["def_elo_diff"]])

coefficient_explanation <- tibble(
  component = c("overall_elo", "offense_elo", "defense_elo"),
  coefficient = c(b_overall, b_offense, b_defense),
  odds_multiplier_per_10_points = exp(10 * coefficient),
  odds_multiplier_per_50_points = exp(50 * coefficient),
  odds_multiplier_per_100_points = exp(100 * coefficient)
)

# =============================================================================
# 2. Current-field rating ranks and centered model contributions
# =============================================================================
# For a matchup A vs B, the non-intercept linear predictor is:
#
#   beta_overall * (overall_A - overall_B) +
#   beta_offense * (offense_A - offense_B) +
#   beta_defense * (defense_A - defense_B)
#
# Thus each team can be represented by a latent score:
#
#   beta_overall * overall_elo +
#   beta_offense * offense_elo +
#   beta_defense * defense_elo
#
# Centering within gender leaves all pairwise score differences unchanged while
# making the individual component contributions easy to interpret.
# =============================================================================

model_explanation <- current_field %>%
  group_by(gender) %>%
  mutate(
    overall_elo_rank = min_rank(desc(overall_elo)),
    offense_elo_rank = min_rank(desc(offense_elo)),
    defense_elo_rank = min_rank(desc(defense_elo)),

    overall_elo_centered = overall_elo - mean(overall_elo),
    offense_elo_centered = offense_elo - mean(offense_elo),
    defense_elo_centered = defense_elo - mean(defense_elo),

    overall_contribution = b_overall * overall_elo_centered,
    offense_contribution = b_offense * offense_elo_centered,
    defense_contribution = b_defense * defense_elo_centered,

    centered_model_score =
      overall_contribution + offense_contribution + defense_contribution,

    centered_model_score_rank = min_rank(desc(centered_model_score))
  ) %>%
  ungroup()

# =============================================================================
# 3. Bracket-free pairwise strength
# =============================================================================

pairwise_strength <- pairwise_probabilities %>%
  group_by(gender, team = team_a) %>%
  summarise(
    mean_field_win_probability = mean(win_probability),
    .groups = "drop"
  ) %>%
  group_by(gender) %>%
  mutate(
    pairwise_strength_rank = min_rank(desc(mean_field_win_probability))
  ) %>%
  ungroup()

model_explanation <- model_explanation %>%
  left_join(pairwise_strength, by = c("gender", "team"))

# The centered latent score and pairwise ranking need not be perfectly identical
# because 05_pairwise_probabilities.R neutralizes the historical Team-A intercept
# by averaging predictions in both orientations. They should nevertheless tell
# essentially the same strength story.

# =============================================================================
# 4. Optional simulation results
# =============================================================================

if (!is.null(simulation_summary)) {
  simulation_ranks <- simulation_summary %>%
    group_by(gender) %>%
    mutate(
      gold_rank = min_rank(desc(gold_probability)),
      medal_rank = min_rank(desc(medal_probability))
    ) %>%
    ungroup() %>%
    select(
      gender, team,
      gold_probability, medal_probability, expected_finish,
      gold_rank, medal_rank
    )

  model_explanation <- model_explanation %>%
    left_join(simulation_ranks, by = c("gender", "team"))
}

# =============================================================================
# 5. Explain movement away from overall Elo
# =============================================================================
# Positive rank_change_vs_overall means the full production model ranks a team
# BETTER than overall Elo alone. The component columns show what is causing that
# movement.
# =============================================================================

model_explanation <- model_explanation %>%
  mutate(
    rank_change_vs_overall = overall_elo_rank - pairwise_strength_rank,
    non_overall_contribution = offense_contribution + defense_contribution,
    primary_non_overall_driver = case_when(
      abs(offense_contribution) > abs(defense_contribution) ~ "offense",
      abs(defense_contribution) > abs(offense_contribution) ~ "defense",
      TRUE ~ "equal"
    )
  ) %>%
  arrange(gender, pairwise_strength_rank)

# =============================================================================
# 6. Contender-focused explanation tables
# =============================================================================

female_model_explanation <- model_explanation %>%
  filter(gender == "female")

male_model_explanation <- model_explanation %>%
  filter(gender == "male")

# Teams moving at least 2 places relative to overall Elo are especially useful
# to inspect when validating whether offense/defense Elo is behaving sensibly.
rank_movers <- model_explanation %>%
  filter(abs(rank_change_vs_overall) >= 2L) %>%
  arrange(gender, desc(abs(rank_change_vs_overall)))

# =============================================================================
# 7. Direct matchup explanation among top women's contenders
# =============================================================================
# This table shows the exact rating differences and log-odds contribution of each
# feature for every directed matchup among the top 8 women by pairwise strength.
# It lets us answer questions such as:
#   "Why does the model like Stam/Schoon against Melissa/Brandie?"
# =============================================================================

female_top8 <- female_model_explanation %>%
  filter(pairwise_strength_rank <= 8L) %>%
  select(team)

female_contender_matchups <- pairwise_probabilities %>%
  filter(gender == "female") %>%
  semi_join(female_top8, by = c("team_a" = "team")) %>%
  semi_join(female_top8, by = c("team_b" = "team")) %>%
  transmute(
    team_a,
    team_b,
    team_a_elo,
    team_b_elo,
    team_a_offense_elo,
    team_b_offense_elo,
    team_a_defense_elo,
    team_b_defense_elo,
    elo_diff,
    off_elo_diff,
    def_elo_diff,
    overall_logodds_contribution = b_overall * elo_diff,
    offense_logodds_contribution = b_offense * off_elo_diff,
    defense_logodds_contribution = b_defense * def_elo_diff,
    total_nonintercept_logodds =
      overall_logodds_contribution +
      offense_logodds_contribution +
      defense_logodds_contribution,
    win_probability
  ) %>%
  arrange(team_a, desc(win_probability))

# =============================================================================
# 8. Save outputs
# =============================================================================

dir.create("data", showWarnings = FALSE, recursive = TRUE)
qs_save(model_explanation, "data/model_explanation.qs")
qs_save(rank_movers, "data/model_explanation_rank_movers.qs")
qs_save(female_contender_matchups, "data/model_explanation_female_h2h.qs")

# =============================================================================
# 9. Console output
# =============================================================================

cat("\nLA28 production model explanation\n")
cat("=================================\n")
cat("\nIntercept: ", round(b_intercept, 6), "\n", sep = "")

cat("\nCoefficient interpretation\n")
print(coefficient_explanation)

for (g in c("female", "male")) {
  cat("\n", toupper(g), " — rating and model-strength decomposition\n", sep = "")

  x <- model_explanation %>%
    filter(gender == g) %>%
    select(
      seed,
      team,
      overall_elo,
      overall_elo_rank,
      offense_elo,
      offense_elo_rank,
      defense_elo,
      defense_elo_rank,
      overall_contribution,
      offense_contribution,
      defense_contribution,
      centered_model_score,
      pairwise_strength_rank,
      rank_change_vs_overall,
      any_of(c("gold_rank", "medal_rank", "gold_probability", "medal_probability"))
    ) %>%
    arrange(pairwise_strength_rank)

  print(x, n = 24, width = Inf)
}

cat("\nRANK MOVERS — production model vs overall Elo\n")
cat("----------------------------------------------\n")
if (nrow(rank_movers) == 0L) {
  cat("No teams moved by 2 or more places.\n")
} else {
  print(
    rank_movers %>%
      select(
        gender,
        team,
        overall_elo_rank,
        offense_elo_rank,
        defense_elo_rank,
        pairwise_strength_rank,
        rank_change_vs_overall,
        overall_contribution,
        offense_contribution,
        defense_contribution,
        non_overall_contribution,
        primary_non_overall_driver
      ),
    n = Inf,
    width = Inf
  )
}

cat("\nObjects available in memory:\n")
cat("  model_explanation\n")
cat("  female_model_explanation\n")
cat("  male_model_explanation\n")
cat("  rank_movers\n")
cat("  coefficient_explanation\n")
cat("  female_contender_matchups\n")

cat("\nSaved:\n")
cat("  data/model_explanation.qs\n")
cat("  data/model_explanation_rank_movers.qs\n")
cat("  data/model_explanation_female_h2h.qs\n")
