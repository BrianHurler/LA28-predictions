library(tidyverse)
library(qs2)

# =============================================================================
# Purpose
# =============================================================================
# Run AFTER 07_tournament_simulation.R.
#
# This script is designed to answer a very specific question:
#
#   "Do the Monte Carlo results make sense given the underlying team strength,
#    pairwise matchup probabilities, seeding, and pool path?"
#
# It does not refit the model or rerun the 10,000 simulations. Instead, it QA's
# the saved outputs and highlights teams whose tournament results look unusually
# strong or weak relative to the inputs that generated them.
# =============================================================================

# =============================================================================
# Configuration
# =============================================================================

EXPECTED_SIMS <- 10000L
RANK_FLAG_THRESHOLD <- 3L
TOP_CONTENDERS <- 8L

# =============================================================================
# Load saved inputs and simulation outputs
# =============================================================================

current_field <- qs_read("data/current_field.qs")
pairwise_probabilities <- qs_read("data/pairwise_probabilities.qs")
simulation_summary <- qs_read("data/simulation_summary.qs")
finish_distribution <- qs_read("data/finish_distribution.qs")

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
# 1. Hard integrity checks
# =============================================================================

field_counts <- current_field %>%
  count(gender, name = "n_teams")

if (any(field_counts$n_teams != 24L)) {
  stop("Expected exactly 24 teams per gender in current_field.")
}

simulation_counts <- simulation_summary %>%
  select(gender, team, simulations)

if (any(simulation_counts$simulations != EXPECTED_SIMS)) {
  warning(
    "At least one team does not have exactly ",
    format(EXPECTED_SIMS, big.mark = ","),
    " simulations."
  )
}

qa_probability_totals <- simulation_summary %>%
  group_by(gender) %>%
  summarise(
    gold_total = sum(gold_probability),
    silver_total = sum(silver_probability),
    bronze_total = sum(bronze_probability),
    medal_total = sum(medal_probability),
    .groups = "drop"
  )

if (any(abs(qa_probability_totals$gold_total - 1) > 1e-10)) {
  stop("Gold probabilities do not sum to 1 within gender.")
}
if (any(abs(qa_probability_totals$silver_total - 1) > 1e-10)) {
  stop("Silver probabilities do not sum to 1 within gender.")
}
if (any(abs(qa_probability_totals$bronze_total - 1) > 1e-10)) {
  stop("Bronze probabilities do not sum to 1 within gender.")
}
if (any(abs(qa_probability_totals$medal_total - 3) > 1e-10)) {
  stop("Medal probabilities do not sum to 3 within gender.")
}

medal_identity_qa <- simulation_summary %>%
  mutate(
    medal_identity_error = abs(
      medal_probability -
        (gold_probability + silver_probability + bronze_probability)
    )
  )

if (max(medal_identity_qa$medal_identity_error) > 1e-12) {
  stop("Medal probability != gold + silver + bronze for at least one team.")
}

finish_team_totals <- finish_distribution %>%
  group_by(gender, team) %>%
  summarise(probability_total = sum(probability), .groups = "drop")

if (any(abs(finish_team_totals$probability_total - 1) > 1e-10)) {
  stop("Finish distribution does not sum to 1 for at least one team.")
}

# Every simulated tournament contains exactly:
# 1 champion, 1 silver, 1 bronze, 1 fourth,
# 4 QF losers, 8 R16 losers, 2 lucky-loser playoff losers, 6 pool fourths.
expected_finish_slots <- tribble(
  ~finish, ~expected_teams,
  1L, 1,
  2L, 1,
  3L, 1,
  4L, 1,
  5L, 4,
  9L, 8,
  17L, 2,
  19L, 6
)

qa_finish_slots <- finish_distribution %>%
  group_by(gender, finish) %>%
  summarise(expected_team_count = sum(probability), .groups = "drop") %>%
  left_join(expected_finish_slots, by = "finish") %>%
  mutate(error = expected_team_count - expected_teams)

if (any(is.na(qa_finish_slots$expected_teams)) ||
    any(abs(qa_finish_slots$error) > 1e-10)) {
  stop("Tournament finish-slot totals are inconsistent with the tournament format.")
}

# Pairwise complementarity: P(A beats B) + P(B beats A) must equal 1.
reverse_lookup <- pairwise_probabilities %>%
  transmute(
    gender,
    team_a_reverse = team_b,
    team_b_reverse = team_a,
    reverse_probability = win_probability
  )

qa_pairwise_symmetry <- pairwise_probabilities %>%
  left_join(
    reverse_lookup,
    by = c(
      "gender" = "gender",
      "team_a" = "team_a_reverse",
      "team_b" = "team_b_reverse"
    )
  ) %>%
  mutate(symmetry_error = abs(win_probability + reverse_probability - 1))

if (any(is.na(qa_pairwise_symmetry$reverse_probability)) ||
    max(qa_pairwise_symmetry$symmetry_error) > 1e-12) {
  stop("Pairwise probability symmetry QA failed.")
}

# =============================================================================
# 2. Independent strength measures
# =============================================================================
# Mean pairwise win probability asks a clean question:
# "If this team played each of the other 23 teams once, how often would the
# production model expect it to win?"
#
# This is useful because it removes the Olympic bracket entirely. If a team is
# 2nd in gold probability AND 2nd in average pairwise strength, the tournament
# simulator is probably not the source of the surprise. If it is 2nd in gold
# but 6th in pairwise strength, path/seeding deserves much closer inspection.
# =============================================================================

pairwise_strength <- pairwise_probabilities %>%
  group_by(gender, team = team_a) %>%
  summarise(
    mean_field_win_probability = mean(win_probability),
    median_field_win_probability = median(win_probability),
    weakest_matchup_probability = min(win_probability),
    strongest_matchup_probability = max(win_probability),
    .groups = "drop"
  ) %>%
  group_by(gender) %>%
  mutate(
    pairwise_strength_rank = min_rank(desc(mean_field_win_probability))
  ) %>%
  ungroup()

field_strength <- current_field %>%
  group_by(gender) %>%
  mutate(
    overall_elo_rank = min_rank(desc(overall_elo)),
    offense_elo_rank = min_rank(desc(offense_elo)),
    defense_elo_rank = min_rank(desc(defense_elo))
  ) %>%
  ungroup() %>%
  select(
    gender, seed, team, federation,
    overall_elo, offense_elo, defense_elo,
    overall_elo_rank, offense_elo_rank, defense_elo_rank
  )

# =============================================================================
# 3. Pool/path diagnostics
# =============================================================================

pool_seed_map <- tribble(
  ~pool, ~seed,
  "A", 1L, "A", 12L, "A", 13L, "A", 24L,
  "B", 2L, "B", 11L, "B", 14L, "B", 23L,
  "C", 3L, "C", 10L, "C", 15L, "C", 22L,
  "D", 4L, "D", 9L,  "D", 16L, "D", 21L,
  "E", 5L, "E", 8L,  "E", 17L, "E", 20L,
  "F", 6L, "F", 7L,  "F", 18L, "F", 19L
)

field_with_pool <- current_field %>%
  left_join(pool_seed_map, by = "seed")

pool_opponents <- field_with_pool %>%
  select(gender, pool, team) %>%
  inner_join(
    field_with_pool %>%
      select(gender, pool, opponent = team),
    by = c("gender", "pool")
  ) %>%
  filter(team != opponent) %>%
  left_join(
    pairwise_probabilities %>%
      select(gender, team_a, team_b, win_probability),
    by = c(
      "gender" = "gender",
      "team" = "team_a",
      "opponent" = "team_b"
    )
  )

if (any(is.na(pool_opponents$win_probability))) {
  stop("Could not match every pool opponent to a pairwise probability.")
}

pool_difficulty <- pool_opponents %>%
  group_by(gender, pool, team) %>%
  summarise(
    expected_pool_wins = sum(win_probability),
    mean_pool_match_win_probability = mean(win_probability),
    hardest_pool_match = min(win_probability),
    easiest_pool_match = max(win_probability),
    .groups = "drop"
  )

# =============================================================================
# 4. Strength vs simulated outcome
# =============================================================================

strength_vs_results <- simulation_summary %>%
  left_join(field_strength, by = c("gender", "team", "seed", "federation")) %>%
  left_join(pairwise_strength, by = c("gender", "team")) %>%
  left_join(pool_difficulty, by = c("gender", "team")) %>%
  group_by(gender) %>%
  mutate(
    gold_rank = min_rank(desc(gold_probability)),
    medal_rank = min_rank(desc(medal_probability)),
    expected_finish_rank = min_rank(expected_finish),

    # Positive = tournament result rank is BETTER than the comparison rank.
    gold_rank_advantage_vs_elo = overall_elo_rank - gold_rank,
    gold_rank_advantage_vs_pairwise = pairwise_strength_rank - gold_rank,
    medal_rank_advantage_vs_elo = overall_elo_rank - medal_rank,
    medal_rank_advantage_vs_pairwise = pairwise_strength_rank - medal_rank,

    gold_mc_se = sqrt(gold_probability * (1 - gold_probability) / simulations),
    medal_mc_se = sqrt(medal_probability * (1 - medal_probability) / simulations),
    gold_mc_95_margin = 1.96 * gold_mc_se,
    medal_mc_95_margin = 1.96 * medal_mc_se
  ) %>%
  ungroup() %>%
  arrange(gender, gold_rank)

qa_flags <- strength_vs_results %>%
  filter(
    abs(gold_rank_advantage_vs_pairwise) >= RANK_FLAG_THRESHOLD |
      abs(medal_rank_advantage_vs_pairwise) >= RANK_FLAG_THRESHOLD |
      abs(gold_rank_advantage_vs_elo) >= RANK_FLAG_THRESHOLD |
      abs(medal_rank_advantage_vs_elo) >= RANK_FLAG_THRESHOLD
  ) %>%
  mutate(
    flag_reason = case_when(
      abs(gold_rank_advantage_vs_pairwise) >= RANK_FLAG_THRESHOLD ~
        "Gold rank differs materially from bracket-free pairwise strength",
      abs(medal_rank_advantage_vs_pairwise) >= RANK_FLAG_THRESHOLD ~
        "Medal rank differs materially from bracket-free pairwise strength",
      abs(gold_rank_advantage_vs_elo) >= RANK_FLAG_THRESHOLD ~
        "Gold rank differs materially from overall Elo rank",
      TRUE ~ "Medal rank differs materially from overall Elo rank"
    )
  )

# =============================================================================
# 5. Head-to-head contender matrices
# =============================================================================
# These are often the fastest way to explain a surprising team. If its overall
# Elo rank is modest but the production model gives it strong probabilities
# against the other contenders, offense/defense Elo is likely driving the result.
# =============================================================================

top_contenders <- strength_vs_results %>%
  group_by(gender) %>%
  arrange(gold_rank, .by_group = TRUE) %>%
  slice_head(n = TOP_CONTENDERS) %>%
  ungroup() %>%
  select(gender, team)

contender_head_to_head <- pairwise_probabilities %>%
  semi_join(top_contenders, by = c("gender", "team_a" = "team")) %>%
  semi_join(top_contenders, by = c("gender", "team_b" = "team")) %>%
  select(gender, team_a, team_b, win_probability) %>%
  arrange(gender, team_a, desc(win_probability))

# Wide matrices for easy viewing in RStudio.
contender_matrix_female <- contender_head_to_head %>%
  filter(gender == "female") %>%
  select(team_a, team_b, win_probability) %>%
  pivot_wider(names_from = team_b, values_from = win_probability)

contender_matrix_male <- contender_head_to_head %>%
  filter(gender == "male") %>%
  select(team_a, team_b, win_probability) %>%
  pivot_wider(names_from = team_b, values_from = win_probability)

# =============================================================================
# 6. Compact review tables
# =============================================================================

qa_review <- strength_vs_results %>%
  select(
    gender,
    seed,
    pool,
    team,
    federation,
    overall_elo,
    overall_elo_rank,
    offense_elo_rank,
    defense_elo_rank,
    mean_field_win_probability,
    pairwise_strength_rank,
    expected_pool_wins,
    gold_probability,
    gold_rank,
    medal_probability,
    medal_rank,
    expected_finish,
    expected_finish_rank,
    gold_rank_advantage_vs_elo,
    gold_rank_advantage_vs_pairwise,
    medal_rank_advantage_vs_pairwise,
    gold_mc_95_margin,
    medal_mc_95_margin
  )

# =============================================================================
# Save QA objects
# =============================================================================

dir.create("data", showWarnings = FALSE, recursive = TRUE)
qs_save(qa_review, "data/simulation_qa_review.qs")
qs_save(qa_flags, "data/simulation_qa_flags.qs")
qs_save(contender_head_to_head, "data/simulation_qa_contender_h2h.qs")

# =============================================================================
# Console output
# =============================================================================

cat("\nLA28 post-simulation QA\n")
cat("=======================\n")
cat("Expected simulations per team: ", format(EXPECTED_SIMS, big.mark = ","), "\n", sep = "")
cat("Rank-difference flag threshold: ", RANK_FLAG_THRESHOLD, " places\n", sep = "")

cat("\nProbability totals\n")
print(qa_probability_totals)

cat("\nFinish-slot totals\n")
print(qa_finish_slots)

for (g in c("female", "male")) {
  cat("\n", toupper(g), " — strength vs tournament results\n", sep = "")
  print(
    qa_review %>%
      filter(gender == g) %>%
      select(
        seed, pool, team,
        overall_elo_rank,
        pairwise_strength_rank,
        gold_rank,
        medal_rank,
        expected_finish,
        mean_field_win_probability,
        expected_pool_wins,
        gold_probability,
        medal_probability,
        gold_rank_advantage_vs_pairwise
      ) %>%
      arrange(gold_rank),
    n = 24
  )
}

cat("\nFLAGGED TEAMS\n")
cat("-------------\n")
if (nrow(qa_flags) == 0L) {
  cat("No teams exceeded the configured rank-difference threshold.\n")
} else {
  print(
    qa_flags %>%
      select(
        gender, team, seed, pool,
        overall_elo_rank,
        pairwise_strength_rank,
        gold_rank,
        medal_rank,
        gold_rank_advantage_vs_elo,
        gold_rank_advantage_vs_pairwise,
        medal_rank_advantage_vs_pairwise,
        expected_pool_wins,
        flag_reason
      ) %>%
      arrange(gender, gold_rank),
    n = Inf
  )
}

cat("\nInterpretation guide\n")
cat("--------------------\n")
cat("1. overall_elo_rank = ranking from current overall Elo only.\n")
cat("2. pairwise_strength_rank = ranking from the production model with the bracket removed.\n")
cat("3. gold_rank = ranking from the 10,000 Olympic simulations.\n")
cat("4. If gold_rank ~= pairwise_strength_rank but differs from overall_elo_rank,\n")
cat("   the production model (likely offense/defense Elo) is driving the surprise.\n")
cat("5. If gold_rank is much better/worse than pairwise_strength_rank, inspect\n")
cat("   seed, pool, expected_pool_wins, and bracket path.\n")
cat("6. Monte Carlo 95% margins show whether a difference could plausibly be\n")
cat("   simulation noise; large rank gaps usually will not be.\n")

cat("\nSaved: data/simulation_qa_review.qs\n")
cat("Saved: data/simulation_qa_flags.qs\n")
cat("Saved: data/simulation_qa_contender_h2h.qs\n")
cat("Objects available in memory:\n")
cat("  qa_review\n")
cat("  qa_flags\n")
cat("  contender_matrix_female\n")
cat("  contender_matrix_male\n")