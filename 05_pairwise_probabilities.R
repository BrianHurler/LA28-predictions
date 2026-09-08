library(tidyverse)
library(qs2)

# =============================================================================
# Load finalized provisional field and fitted production model
# =============================================================================

current_field <- qs_read("data/current_field.qs")
production_model <- qs_read("data/production_model.qs")

required_field_cols <- c(
  "gender", "seed", "team", "federation",
  "overall_elo", "offense_elo", "defense_elo"
)

missing_field_cols <- setdiff(required_field_cols, names(current_field))
if (length(missing_field_cols) > 0) {
  stop(
    "current_field is missing required columns: ",
    paste(missing_field_cols, collapse = ", ")
  )
}

if (any(is.na(current_field$overall_elo)) ||
    any(is.na(current_field$offense_elo)) ||
    any(is.na(current_field$defense_elo))) {
  stop("current_field contains missing Elo inputs; cannot build pairwise probabilities.")
}

field_counts <- current_field %>% count(gender, name = "n_teams")
if (any(field_counts$n_teams != 24L)) {
  stop("Expected exactly 24 teams per gender before pairwise probability construction.")
}

# =============================================================================
# Build every unique matchup within each gender
# =============================================================================
# There are choose(24, 2) = 276 unordered matchups per gender.
#
# The historical model has a small non-zero intercept because Team A was not a
# perfectly random label in the source data. In a future simulated matchup,
# however, assigning one team to A or B is arbitrary. To avoid introducing an
# artificial ordering advantage, predict every matchup in BOTH orientations:
#
#   p1 = P(team_1 beats team_2 when team_1 is coded as Team A)
#   p2 = 1 - P(team_2 beats team_1 when team_2 is coded as Team A)
#
# The neutral matchup probability is the mean of p1 and p2. The reverse
# direction is then defined as exactly 1 - p_neutral.
# =============================================================================

build_unordered_matchups <- function(field_g) {
  idx <- combn(seq_len(nrow(field_g)), 2)

  tibble(
    i = idx[1, ],
    j = idx[2, ]
  ) %>%
    transmute(
      gender = field_g$gender[i],
      team_1 = field_g$team[i],
      team_2 = field_g$team[j],
      seed_1 = field_g$seed[i],
      seed_2 = field_g$seed[j],
      federation_1 = field_g$federation[i],
      federation_2 = field_g$federation[j],
      overall_elo_1 = field_g$overall_elo[i],
      overall_elo_2 = field_g$overall_elo[j],
      offense_elo_1 = field_g$offense_elo[i],
      offense_elo_2 = field_g$offense_elo[j],
      defense_elo_1 = field_g$defense_elo[i],
      defense_elo_2 = field_g$defense_elo[j]
    )
}

unordered_matchups <- current_field %>%
  group_split(gender) %>%
  map_dfr(build_unordered_matchups) %>%
  mutate(
    elo_diff = overall_elo_1 - overall_elo_2,
    off_elo_diff = offense_elo_1 - offense_elo_2,
    def_elo_diff = defense_elo_1 - defense_elo_2
  )

# Team 1 coded as Team A.
unordered_matchups$p_team_1_raw <- predict(
  production_model,
  newdata = unordered_matchups %>%
    select(elo_diff, off_elo_diff, def_elo_diff),
  type = "response"
)

# Reverse orientation: Team 2 coded as Team A.
reverse_inputs <- unordered_matchups %>%
  transmute(
    elo_diff = -elo_diff,
    off_elo_diff = -off_elo_diff,
    def_elo_diff = -def_elo_diff
  )

unordered_matchups$p_team_2_raw_as_a <- predict(
  production_model,
  newdata = reverse_inputs,
  type = "response"
)

unordered_matchups <- unordered_matchups %>%
  mutate(
    p_team_1 = (p_team_1_raw + (1 - p_team_2_raw_as_a)) / 2,
    p_team_2 = 1 - p_team_1,
    raw_order_effect = p_team_1_raw - (1 - p_team_2_raw_as_a)
  )

# =============================================================================
# Convert to directed lookup table for the simulator
# =============================================================================
# The tournament simulator should be able to ask for P(A beats B) without caring
# which orientation appeared in the unordered source table. Save both directions
# and force the reverse matchup to be exactly complementary.
# =============================================================================

pairwise_probabilities <- bind_rows(
  unordered_matchups %>%
    transmute(
      gender,
      team_a = team_1,
      team_b = team_2,
      seed_a = seed_1,
      seed_b = seed_2,
      federation_a = federation_1,
      federation_b = federation_2,
      team_a_elo = overall_elo_1,
      team_b_elo = overall_elo_2,
      team_a_offense_elo = offense_elo_1,
      team_b_offense_elo = offense_elo_2,
      team_a_defense_elo = defense_elo_1,
      team_b_defense_elo = defense_elo_2,
      elo_diff,
      off_elo_diff,
      def_elo_diff,
      win_probability = p_team_1
    ),
  unordered_matchups %>%
    transmute(
      gender,
      team_a = team_2,
      team_b = team_1,
      seed_a = seed_2,
      seed_b = seed_1,
      federation_a = federation_2,
      federation_b = federation_1,
      team_a_elo = overall_elo_2,
      team_b_elo = overall_elo_1,
      team_a_offense_elo = offense_elo_2,
      team_b_offense_elo = offense_elo_1,
      team_a_defense_elo = defense_elo_2,
      team_b_defense_elo = defense_elo_1,
      elo_diff = -elo_diff,
      off_elo_diff = -off_elo_diff,
      def_elo_diff = -def_elo_diff,
      win_probability = p_team_2
    )
) %>%
  arrange(gender, team_a, team_b)

# =============================================================================
# QA
# =============================================================================

unordered_counts <- unordered_matchups %>%
  count(gender, name = "unordered_matchups")

directed_counts <- pairwise_probabilities %>%
  count(gender, name = "directed_matchups")

if (any(unordered_counts$unordered_matchups != choose(24, 2))) {
  stop("Unexpected number of unordered pairwise matchups.")
}

if (any(directed_counts$directed_matchups != 24L * 23L)) {
  stop("Unexpected number of directed pairwise matchups.")
}

if (anyDuplicated(pairwise_probabilities[c("gender", "team_a", "team_b")]) > 0) {
  stop("Duplicate directed pairwise matchup rows detected.")
}

if (any(pairwise_probabilities$win_probability <= 0 | pairwise_probabilities$win_probability >= 1)) {
  stop("Pairwise win probabilities must be strictly between 0 and 1.")
}

# Build reverse keys using temporary names. In dplyr::transmute(), newly created
# columns are immediately available to later expressions, so directly writing
# team_a = team_b followed by team_b = team_a would accidentally overwrite the
# original team_a before the second assignment is evaluated.
reverse_probability_lookup <- pairwise_probabilities %>%
  transmute(
    gender,
    original_team_a = team_a,
    original_team_b = team_b,
    p_ba = win_probability
  ) %>%
  transmute(
    gender,
    team_a = original_team_b,
    team_b = original_team_a,
    p_ba
  )

symmetry_qa <- pairwise_probabilities %>%
  select(gender, team_a, team_b, p_ab = win_probability) %>%
  left_join(
    reverse_probability_lookup,
    by = c("gender", "team_a", "team_b")
  ) %>%
  mutate(symmetry_error = abs((p_ab + p_ba) - 1))

if (any(is.na(symmetry_qa$p_ba))) {
  stop("Pairwise symmetry QA could not find a reverse probability for every matchup.")
}

max_symmetry_error <- max(symmetry_qa$symmetry_error)

if (max_symmetry_error > 1e-12) {
  stop("Pairwise probability symmetry QA failed.")
}

# =============================================================================
# Save downstream simulation input
# =============================================================================

dir.create("data", showWarnings = FALSE, recursive = TRUE)
qs_save(pairwise_probabilities, "data/pairwise_probabilities.qs")
qs_save(unordered_matchups, "data/pairwise_probabilities_unordered.qs")

# =============================================================================
# Inspection output
# =============================================================================

cat("\nLA28 pairwise win probabilities\n")
cat("===============================\n")
cat("Production model inputs: overall Elo + offense Elo + defense Elo\n")
cat("Future matchup order effect neutralized by two-orientation averaging.\n")

cat("\nUnordered matchup counts\n")
print(unordered_counts)

cat("\nDirected lookup counts\n")
print(directed_counts)

cat("\nRaw Team-A order effect before neutralization\n")
cat(
  "Mean absolute probability difference: ",
  round(mean(abs(unordered_matchups$raw_order_effect)), 5),
  "\n",
  sep = ""
)
cat(
  "Max absolute probability difference: ",
  round(max(abs(unordered_matchups$raw_order_effect)), 5),
  "\n",
  sep = ""
)

cat("\nMaximum symmetry error after neutralization: ", format(max_symmetry_error, scientific = TRUE), "\n", sep = "")

cat("\nHighest-confidence predicted matchups (first 12)\n")
print(
  pairwise_probabilities %>%
    filter(seed_a < seed_b) %>%
    arrange(desc(abs(win_probability - 0.5))) %>%
    select(gender, team_a, team_b, win_probability) %>%
    slice_head(n = 12),
  n = 12
)

cat("\nSaved: data/pairwise_probabilities.qs\n")
cat("Saved: data/pairwise_probabilities_unordered.qs\n")
