# LA28-predictions

Predictive model for how beach volleyball teams will finish at the LA28 Olympics.

## Script order

The repository is organized as a sequential modeling workflow:

1. `01_df_prep.R` — build and QA the historical match-level modeling dataset.
2. `02_model_validation.R` — compare candidate match-win models using chronological out-of-time validation.
3. `03_production_model.R` — fit the selected V0 model on the full usable historical dataset and save the fitted model for downstream simulation.
4. `04_current_field.R` — build the provisional 24-team field for each gender from current/latest Elo state, subject to a maximum of two teams per federation.

Future scripts will continue this numbering for pairwise win probabilities and tournament simulation.

## Modeling architecture

The project does **not** predict final Olympic finish directly.

Historical match data are used to estimate the probability that Team A beats Team B. Those match-win probabilities will then feed a tournament simulation that repeatedly plays the provisional LA28 field through pool play, Lucky Loser matches, and the single-elimination bracket.

The intended outputs include:

- Gold probability
- Silver probability
- Bronze probability
- Medal probability
- Quarterfinal / top-five probability
- Expected finish
- Full finish distribution

## Historical modeling data

`01_df_prep.R` creates the historical match-level dataset.

Current sources:

- `performance_data.qs` defines the cleaned match universe and supplies match metadata, outcomes, point differential, federation, gender, and overall match Elo.
- `rallies_with_off_def_elo.rda` supplies exact pre-match offense and defense Elo.

Overall match Elo deliberately uses the latest rating from a **strictly earlier date**. This prevents same-day information leakage. On a team's first observed playing date, its pre-match Elo is initialized to 1500.

Until upstream pipeline QA guarantees complete Elo coverage, `01_df_prep.R` creates a temporary complete-case dataset called `model_df_ready` and saves it to:

```text
data/model_df_ready.qs
```

The full `model_df` remains available in memory for QA; incomplete matches are excluded only from the temporary modeling population.

Current usable modeling population:

- 9,008 total cleaned matches
- 9,005 model-ready matches
- 3 excluded matches, all due to missing outcomes
- 0 missing overall match Elo after first-observed-day initialization
- 0 missing offense Elo in the model-ready population
- 0 missing defense Elo in the model-ready population

Federation QA is retained because federation is needed later for the provisional two-teams-per-country field rule. Known upstream federation inconsistencies should be repaired in the production pipeline, but they do not block match-model development.

## V0 predictive model validation

`02_model_validation.R` compares four logistic-regression specifications:

1. Overall Elo
2. Overall Elo + offense Elo
3. Overall Elo + defense Elo
4. Overall Elo + offense Elo + defense Elo

The primary V0 question is whether rally-derived offense and defense Elo improve out-of-time match prediction beyond overall match Elo alone.

### 80/20 chronological holdout

The first 80% of observed dates are used for training and the final 20% for testing. Matches from the same date remain on the same side of the split.

| Model | Log loss | Brier |
|---|---:|---:|
| Overall Elo | 0.603 | 0.209 |
| Overall + offense Elo | 0.592 | 0.204 |
| Overall + defense Elo | 0.600 | 0.207 |
| Overall + offense + defense Elo | **0.591** | **0.203** |

The full model performed best on both metrics.

### Expanding-window yearly holdouts

The four models were also evaluated using expanding-window out-of-time validation:

- 2024 holdout: train on 2023, test on 2024
- 2025 holdout: train on 2023–2024, test on 2025
- 2026 holdout: train on 2023–2025, test on available 2026 data

The combined overall + offense + defense model improved on the overall-Elo-only baseline in **every holdout year** on both log loss and Brier score.

| Holdout year | Log-loss improvement | Brier improvement |
|---|---:|---:|
| 2024 | 0.00890 | 0.00377 |
| 2025 | 0.0107 | 0.00494 |
| 2026 | 0.0129 | 0.00615 |

Across all validation windows:

- Overall Elo carried the majority of predictive signal.
- Offense Elo added a meaningful and stable incremental improvement.
- Defense Elo added a smaller but consistently positive improvement.
- The full overall + offense + defense model performed best in every yearly holdout.

### V0 model decision

Based on the out-of-time validation results, the selected V0 match-win model is:

```text
team_a_win ~ overall Elo difference
           + offense Elo difference
           + defense Elo difference
```

`03_production_model.R` fits this specification on all 9,005 usable historical matches and saves the fitted model to:

```text
data/production_model.qs
```

This fitted model is the input to the next stage: constructing the current competitive field, generating pairwise win probabilities, and simulating the Olympic tournament.

---

# Provisional LA28 competitive field

The LA28 qualification pathway is not yet known in final detail, so the first simulator uses a deliberately simple provisional field.

`04_current_field.R` builds that field from current production data. Historical model fitting uses leakage-safe pre-match ratings, but future-match prediction uses each partnership's **latest known state**:

- current overall Elo from the latest `team_elo_on_date` in `performance_data.qs`;
- current offense and defense Elo from `off_def_elo_ratings.rda`, using the mean of the two partners' player ratings where available;
- latest observed federation and partnership identifiers from `performance_data.qs`.

For **each gender separately**:

1. Take each team's most recent Elo score.
2. Rank teams from highest to lowest Elo.
3. Select the top 24 teams, subject to a maximum of **two teams per federation/country**.
4. Ignore continental qualification pathways for V0.
5. Seed the selected teams 1 through 24 by Elo.

The script saves:

```text
data/current_team_state.qs
data/current_field.qs
```

No activity cutoff is imposed in V0. `last_observed_date` is retained in the field output so stale or inactive partnerships can be identified and, if necessary, handled through an explicit documented rule rather than manual removal.

This is a modeling assumption, not a prediction of the official LA28 qualification system.

## Pool allocation

The 24 seeds are placed into six pools of four using the Paris-style serpentine structure:

| Pool | Seeds |
|---|---|
| A | 1, 12, 13, 24 |
| B | 2, 11, 14, 23 |
| C | 3, 10, 15, 22 |
| D | 4, 9, 16, 21 |
| E | 5, 8, 17, 20 |
| F | 6, 7, 18, 19 |

Each pool is a four-team single round robin, so every team plays three pool matches.

## Advancement from pool play

Following Paris 2024 as closely as practical:

- Six pool winners advance directly to the Round of 16.
- Six pool runners-up advance directly to the Round of 16.
- The **two best third-place teams** advance directly to the Round of 16.
- The remaining four third-place teams enter two Lucky Loser matches.
- The two Lucky Loser winners complete the 16-team knockout field.

### Ranking the third-place teams

Paris 2024 ranked third-place teams by:

1. Match points
2. Set ratio
3. Rally-point ratio
4. Tournament seeding

The four third-place teams that do not qualify directly are ranked 3rd through 6th among all third-place finishers. The Lucky Loser matches are **not random**:

- 3rd-best third-place team vs 6th-best third-place team
- 4th-best third-place team vs 5th-best third-place team

---

# Paris-style Round of 16 bracket

Paris 2024 did not simply seed the 16 surviving teams 1 through 16 by record. It used a post-pool **drawing of lots** with fixed positions for pool winners and constrained draws for the other qualifiers.

## Fixed pool-winner positions

| Round-of-16 seed | Team |
|---|---|
| 1 | Pool A winner |
| 2 | Pool B winner |
| 3 | Pool C winner |
| 4 | Pool D winner |
| 5 | Pool E winner |
| 6 | Pool F winner |

## Drawn positions

### Seeds 15-16: Lucky Loser winners

The two Lucky Loser winners are randomly drawn into seeds 15 and 16, subject to the no-rematch rule:

- a Lucky Loser winner originally from Pool A cannot be seed 16, because seed 16 plays Pool A's winner;
- a Lucky Loser winner from Pool B cannot be seed 15, because seed 15 plays Pool B's winner.

### Seeds 13-14: directly advancing third-place teams

The two best third-place teams are randomly drawn into seeds 13 and 14, again subject to avoiding a same-pool Round-of-16 rematch:

- a team from Pool D cannot be seed 13;
- a team from Pool C cannot be seed 14.

### Seeds 7-12: pool runners-up

The six pool runners-up are drawn into seeds 7 through 12. Paris used the following draw order:

1. seed 9
2. seed 8
3. seed 12
4. seed 11
5. seed 7
6. seed 10

Blocked positions are skipped and redrawn when a placement would create a Round-of-16 match between two teams from the same pool. In particular, Pool E's runner-up cannot occupy seed 12 and Pool F's runner-up cannot occupy seed 11.

Teams from the **same pool cannot meet in the Round of 16**. Teams from the **same federation/country are allowed to meet** once the knockout phase begins.

## Round-of-16 matchups

Once seeds 1-16 have been assigned, the Paris bracket is:

| Match | Team 1 | Team 2 |
|---|---:|---:|
| R16-1 | Seed 1 | Seed 16 |
| R16-2 | Seed 9 | Seed 8 |
| R16-3 | Seed 5 | Seed 12 |
| R16-4 | Seed 13 | Seed 4 |
| R16-5 | Seed 3 | Seed 14 |
| R16-6 | Seed 11 | Seed 6 |
| R16-7 | Seed 7 | Seed 10 |
| R16-8 | Seed 15 | Seed 2 |

The bracket then proceeds as a fixed single-elimination bracket through quarterfinals and semifinals. Semifinal winners play for gold/silver; semifinal losers play for bronze.

## Olympic finish positions

To mirror Paris 2024 reporting:

- 1st: gold-medal winner
- 2nd: gold-medal loser
- 3rd: bronze-medal winner
- 4th: bronze-medal loser
- 5th: four quarterfinal losers
- 9th: eight Round-of-16 losers
- 17th: two Lucky Loser losers
- 19th: six fourth-place pool teams

---

## Paris 2024 reference material

The provisional tournament simulator is based primarily on the official Paris 2024 Beach Volleyball Specific Competition Regulations and the Volleyball World competition formula:

- FIVB, *Olympic Games Paris 2024 – Beach Volleyball Tournaments: Specific Competition Regulations*, Version 1.0, 25 March 2024: https://images.volleyballworld.com/image/upload/fl_attachment/fivb-prd/zhun4pbvv2jsb0hxszph.pdf
- Volleyball World, *Beach Volleyball Olympic Games Paris 2024 – Competition Formula*: https://en.volleyballworld.com/beachvolleyball/competitions/beach-volleyball-olympic-games-paris-2024/competition/formula

LA28 rules may differ. The simulator should therefore keep qualification, pool allocation, advancement, and bracket-draw logic modular so those rules can be replaced without rebuilding the match-win model.
