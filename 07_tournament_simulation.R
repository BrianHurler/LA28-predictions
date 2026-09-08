library(tidyverse)
library(qs2)

# =============================================================================
# Configuration
# =============================================================================

N_SIM <- 10000L
SIM_SEED <- 20260908L
set.seed(SIM_SEED)

# =============================================================================
# Load finalized field and simulation inputs
# =============================================================================

current_field <- qs_read("data/current_field.qs")
pairwise_probabilities <- qs_read("data/pairwise_probabilities.qs")
scoreline_library <- qs_read("data/scoreline_library.qs")

if (any(current_field %>% count(gender) %>% pull(n) != 24L)) {
  stop("Expected exactly 24 teams per gender.")
}

# =============================================================================
# Pool allocation
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

if (any(is.na(field_with_pool$pool))) {
  stop("At least one field team could not be assigned to a pool.")
}

# =============================================================================
# Pairwise coverage QA
# =============================================================================
# Before any simulation begins, verify that every directed matchup implied by
# the current 24-team field exists exactly once in the saved pairwise table.
# This catches stale downstream files or team-label mismatches immediately.
# =============================================================================

expected_pairwise <- current_field %>%
  select(gender, team) %>%
  inner_join(
    current_field %>% select(gender, team) %>% rename(team_b = team),
    by = "gender"
  ) %>%
  rename(team_a = team) %>%
  filter(team_a != team_b)

pairwise_coverage <- expected_pairwise %>%
  left_join(
    pairwise_probabilities %>%
      select(gender, team_a, team_b, win_probability),
    by = c("gender", "team_a", "team_b")
  )

missing_pairwise <- pairwise_coverage %>%
  filter(is.na(win_probability))

if (nrow(missing_pairwise) > 0L) {
  print(missing_pairwise, n = Inf)
  stop(
    "Pairwise table does not cover the finalized current field. ",
    "Rerun 05_pairwise_probabilities.R after the latest 04_current_field.R."
  )
}

pairwise_duplicate_qa <- pairwise_probabilities %>%
  semi_join(expected_pairwise, by = c("gender", "team_a", "team_b")) %>%
  count(gender, team_a, team_b) %>%
  filter(n != 1L)

if (nrow(pairwise_duplicate_qa) > 0L) {
  print(pairwise_duplicate_qa, n = Inf)
  stop("At least one current-field pairwise matchup is not represented exactly once.")
}

# =============================================================================
# Helpers
# =============================================================================

probability_bin <- function(p) {
  cut(
    p,
    breaks = c(0, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 1.0000001),
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
  ) %>% as.character()
}

lookup_win_probability <- function(gender, team_a, team_b) {
  x <- pairwise_probabilities %>%
    filter(
      .data$gender == .env$gender,
      .data$team_a == .env$team_a,
      .data$team_b == .env$team_b
    ) %>%
    pull(win_probability)

  if (length(x) != 1L) {
    stop("Pairwise probability lookup failed for: ", team_a, " vs ", team_b)
  }

  x[[1]]
}

sample_scoreline <- function(gender, winner_probability) {
  target_bin <- probability_bin(winner_probability)

  candidates <- scoreline_library %>%
    filter(
      .data$gender == .env$gender,
      as.character(.data$winner_probability_bin) == .env$target_bin
    )

  if (nrow(candidates) == 0L) {
    candidates <- scoreline_library %>%
      filter(.data$gender == .env$gender)
  }

  candidates %>%
    slice_sample(n = 1) %>%
    select(winner_sets, loser_sets, winner_points, loser_points)
}

simulate_match <- function(gender, team_a, team_b) {
  p_a <- lookup_win_probability(gender, team_a, team_b)
  a_wins <- runif(1) < p_a

  winner <- if (a_wins) team_a else team_b
  loser <- if (a_wins) team_b else team_a
  winner_probability <- if (a_wins) p_a else 1 - p_a
  score <- sample_scoreline(gender, winner_probability)

  tibble(
    team_a = team_a,
    team_b = team_b,
    winner = winner,
    loser = loser,
    p_team_a = p_a,
    winner_probability = winner_probability,
    winner_sets = score$winner_sets[[1]],
    loser_sets = score$loser_sets[[1]],
    winner_points = score$winner_points[[1]],
    loser_points = score$loser_points[[1]]
  )
}

rank_table <- function(x) {
  x %>%
    mutate(
      set_ratio = if_else(sets_against == 0, Inf, sets_for / sets_against),
      point_ratio = if_else(points_against == 0, Inf, points_for / points_against)
    ) %>%
    arrange(
      desc(match_points),
      desc(set_ratio),
      desc(point_ratio),
      seed
    ) %>%
    mutate(rank = row_number())
}

simulate_pool <- function(gender, pool_name, pool_field) {
  match_idx <- combn(seq_len(nrow(pool_field)), 2)

  matches <- map_dfr(seq_len(ncol(match_idx)), function(k) {
    i <- match_idx[1, k]
    j <- match_idx[2, k]

    simulate_match(
      gender,
      pool_field$team[[i]],
      pool_field$team[[j]]
    ) %>%
      mutate(pool = pool_name)
  })

  standings <- pool_field %>%
    select(team, seed, federation) %>%
    mutate(
      wins = 0L,
      losses = 0L,
      match_points = 0L,
      sets_for = 0L,
      sets_against = 0L,
      points_for = 0L,
      points_against = 0L
    )

  for (k in seq_len(nrow(matches))) {
    m <- matches[k, ]

    standings <- standings %>%
      mutate(
        wins = wins + as.integer(team == m$winner),
        losses = losses + as.integer(team == m$loser),
        match_points = match_points + case_when(
          team == m$winner ~ 2L,
          team == m$loser ~ 1L,
          TRUE ~ 0L
        ),
        sets_for = sets_for + case_when(
          team == m$winner ~ m$winner_sets,
          team == m$loser ~ m$loser_sets,
          TRUE ~ 0L
        ),
        sets_against = sets_against + case_when(
          team == m$winner ~ m$loser_sets,
          team == m$loser ~ m$winner_sets,
          TRUE ~ 0L
        ),
        points_for = points_for + case_when(
          team == m$winner ~ m$winner_points,
          team == m$loser ~ m$loser_points,
          TRUE ~ 0L
        ),
        points_against = points_against + case_when(
          team == m$winner ~ m$loser_points,
          team == m$loser ~ m$winner_points,
          TRUE ~ 0L
        )
      )
  }

  standings <- rank_table(standings) %>% mutate(pool = pool_name)

  list(matches = matches, standings = standings)
}

valid_two_slot_draw <- function(teams, slots, forbidden_pool_by_slot) {
  perms <- list(teams, rev(teams))
  valid <- keep(perms, function(p) {
    all(map2_lgl(slots, p, function(slot, team_row) {
      forbidden_pool <- forbidden_pool_by_slot[[as.character(slot)]]
      is.null(forbidden_pool) || team_row$pool != forbidden_pool
    }))
  })

  if (length(valid) == 0L) {
    stop("No valid two-slot R16 draw permutation found.")
  }

  valid[[sample(seq_along(valid), 1)]]
}

valid_runner_draw <- function(runners) {
  slots <- c(7L, 8L, 9L, 10L, 11L, 12L)
  max_attempts <- 500L

  for (attempt in seq_len(max_attempts)) {
    shuffled <- runners[sample(seq_len(nrow(runners))), ]
    names(shuffled) <- names(runners)

    assigned <- tibble(slot = slots, pool = shuffled$pool, team = shuffled$team)

    if (!any(assigned$slot == 11L & assigned$pool == "F") &&
        !any(assigned$slot == 12L & assigned$pool == "E")) {
      return(assigned)
    }
  }

  stop("Could not construct a valid runner-up R16 draw.")
}

simulate_gender_tournament <- function(gender) {
  field_g <- field_with_pool %>%
    filter(.data$gender == .env$gender)

  pool_results <- map(set_names(LETTERS[1:6]), function(pool_name) {
    simulate_pool(
      gender,
      pool_name,
      field_g %>% filter(.data$pool == .env$pool_name)
    )
  })

  standings_all <- map_dfr(pool_results, "standings")

  winners <- standings_all %>% filter(rank == 1L)
  runners <- standings_all %>% filter(rank == 2L)
  thirds <- standings_all %>%
    filter(rank == 3L) %>%
    arrange(
      desc(match_points),
      desc(set_ratio),
      desc(point_ratio),
      seed
    ) %>%
    mutate(third_rank = row_number())
  fourths <- standings_all %>% filter(rank == 4L)

  direct_thirds <- thirds %>% filter(third_rank <= 2L)
  lucky_loser_pool <- thirds %>% filter(third_rank >= 3L)

  ll1 <- simulate_match(
    gender,
    lucky_loser_pool$team[lucky_loser_pool$third_rank == 3L],
    lucky_loser_pool$team[lucky_loser_pool$third_rank == 6L]
  )
  ll2 <- simulate_match(
    gender,
    lucky_loser_pool$team[lucky_loser_pool$third_rank == 4L],
    lucky_loser_pool$team[lucky_loser_pool$third_rank == 5L]
  )

  ll_losers <- c(ll1$loser[[1]], ll2$loser[[1]])

  ll_winners <- bind_rows(
    lucky_loser_pool %>% filter(team == ll1$winner[[1]]),
    lucky_loser_pool %>% filter(team == ll2$winner[[1]])
  )

  # Fixed seeds 1-6 are pool winners A-F.
  r16_slots <- winners %>%
    mutate(slot = match(pool, LETTERS[1:6])) %>%
    select(slot, team, pool)

  # Lucky Loser winners into 15/16; avoid Pool B at 15 and Pool A at 16.
  ll_rows <- split(ll_winners, seq_len(nrow(ll_winners)))
  ll_draw <- valid_two_slot_draw(
    ll_rows,
    c(15L, 16L),
    list(`15` = "B", `16` = "A")
  )
  r16_slots <- bind_rows(
    r16_slots,
    map2_dfr(c(15L, 16L), ll_draw, ~tibble(slot = .x, team = .y$team, pool = .y$pool))
  )

  # Direct third-place teams into 13/14; avoid Pool D at 13 and Pool C at 14.
  third_rows <- split(direct_thirds, seq_len(nrow(direct_thirds)))
  third_draw <- valid_two_slot_draw(
    third_rows,
    c(13L, 14L),
    list(`13` = "D", `14` = "C")
  )
  r16_slots <- bind_rows(
    r16_slots,
    map2_dfr(c(13L, 14L), third_draw, ~tibble(slot = .x, team = .y$team, pool = .y$pool))
  )

  # Pool runners-up into 7-12; Pool F cannot be 11, Pool E cannot be 12.
  runner_draw <- valid_runner_draw(runners)
  r16_slots <- bind_rows(r16_slots, runner_draw)

  if (nrow(r16_slots) != 16L || anyDuplicated(r16_slots$slot) > 0) {
    stop("R16 draw did not produce exactly one team in each of 16 slots.")
  }

  team_at <- function(slot) r16_slots$team[r16_slots$slot == slot][[1]]

  r16_pairs <- list(
    c(1L, 16L), c(9L, 8L), c(5L, 12L), c(13L, 4L),
    c(3L, 14L), c(11L, 6L), c(7L, 10L), c(15L, 2L)
  )

  r16 <- map(r16_pairs, ~simulate_match(gender, team_at(.x[[1]]), team_at(.x[[2]])))
  r16_winners <- map_chr(r16, ~.x$winner[[1]])
  r16_losers <- map_chr(r16, ~.x$loser[[1]])

  qf_pairs <- list(c(1L, 2L), c(3L, 4L), c(5L, 6L), c(7L, 8L))
  qf <- map(qf_pairs, ~simulate_match(gender, r16_winners[.x[[1]]], r16_winners[.x[[2]]]))
  qf_winners <- map_chr(qf, ~.x$winner[[1]])
  qf_losers <- map_chr(qf, ~.x$loser[[1]])

  sf1 <- simulate_match(gender, qf_winners[[1]], qf_winners[[2]])
  sf2 <- simulate_match(gender, qf_winners[[3]], qf_winners[[4]])

  gold_match <- simulate_match(gender, sf1$winner[[1]], sf2$winner[[1]])
  bronze_match <- simulate_match(gender, sf1$loser[[1]], sf2$loser[[1]])

  finishes <- tibble(
    team = field_g$team,
    finish = NA_integer_
  ) %>%
    mutate(
      finish = case_when(
        team == gold_match$winner[[1]] ~ 1L,
        team == gold_match$loser[[1]] ~ 2L,
        team == bronze_match$winner[[1]] ~ 3L,
        team == bronze_match$loser[[1]] ~ 4L,
        team %in% qf_losers ~ 5L,
        team %in% r16_losers ~ 9L,
        team %in% ll_losers ~ 17L,
        team %in% fourths$team ~ 19L,
        TRUE ~ NA_integer_
      )
    )

  if (any(is.na(finishes$finish))) {
    stop("At least one team did not receive a tournament finish.")
  }

  finishes
}

# =============================================================================
# Monte Carlo
# =============================================================================

cat("\nLA28 tournament simulation\n")
cat("==========================\n")
cat("Simulations per gender: ", format(N_SIM, big.mark = ","), "\n", sep = "")
cat("Random seed: ", SIM_SEED, "\n", sep = "")
cat("Pairwise coverage QA: complete for all current-field directed matchups\n")

simulation_results <- map_dfr(c("female", "male"), function(g) {
  cat("Running ", g, " simulations...\n", sep = "")

  map_dfr(seq_len(N_SIM), function(sim_id) {
    simulate_gender_tournament(g) %>%
      mutate(gender = g, simulation = sim_id)
  })
})

# =============================================================================
# Aggregate probabilities
# =============================================================================

simulation_summary <- simulation_results %>%
  group_by(gender, team) %>%
  summarise(
    simulations = n(),
    gold_probability = mean(finish == 1L),
    silver_probability = mean(finish == 2L),
    bronze_probability = mean(finish == 3L),
    medal_probability = mean(finish <= 3L),
    semifinal_probability = mean(finish <= 4L),
    quarterfinal_probability = mean(finish <= 5L),
    round_of_16_probability = mean(finish <= 9L),
    expected_finish = mean(finish),
    .groups = "drop"
  ) %>%
  left_join(
    current_field %>% select(gender, team, seed, federation, continental_berth),
    by = c("gender", "team")
  ) %>%
  arrange(gender, desc(gold_probability), desc(medal_probability))

finish_distribution <- simulation_results %>%
  count(gender, team, finish, name = "n") %>%
  group_by(gender, team) %>%
  mutate(probability = n / sum(n)) %>%
  ungroup()

# =============================================================================
# QA
# =============================================================================

qa_gold <- simulation_summary %>%
  group_by(gender) %>%
  summarise(total_gold_probability = sum(gold_probability), .groups = "drop")

qa_medals <- simulation_summary %>%
  group_by(gender) %>%
  summarise(total_medal_probability = sum(medal_probability), .groups = "drop")

if (any(abs(qa_gold$total_gold_probability - 1) > 1e-10)) {
  stop("Gold probabilities do not sum to 1 within gender.")
}

if (any(abs(qa_medals$total_medal_probability - 3) > 1e-10)) {
  stop("Medal probabilities do not sum to 3 within gender.")
}

# =============================================================================
# Save outputs
# =============================================================================

dir.create("data", showWarnings = FALSE, recursive = TRUE)
qs_save(simulation_summary, "data/simulation_summary.qs")
qs_save(finish_distribution, "data/finish_distribution.qs")

cat("\nProbability QA\n")
print(qa_gold)
print(qa_medals)

cat("\nTop 12 teams by gold probability - female\n")
print(
  simulation_summary %>%
    filter(gender == "female") %>%
    select(seed, team, federation, gold_probability, medal_probability, expected_finish) %>%
    slice_head(n = 12),
  n = 12
)

cat("\nTop 12 teams by gold probability - male\n")
print(
  simulation_summary %>%
    filter(gender == "male") %>%
    select(seed, team, federation, gold_probability, medal_probability, expected_finish) %>%
    slice_head(n = 12),
  n = 12
)

cat("\nSaved: data/simulation_summary.qs\n")
cat("Saved: data/finish_distribution.qs\n")
