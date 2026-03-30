# Function inventory

All existing derived variable functions, mapped to patterns, levels, and
quality tiers.

## Modern functions (v3)

| Function | File | Pattern | Level | Tier | Notes |
|----------|------|---------|-------|------|-------|
| `calculate_SMKDSTY_cat6` | smoking-status.R | Category grouping | L7 | Gold | 3 inputs → 6 categories |
| `calculate_SMKDSTY_A` | smoking-status.R | — | — | — | Deprecated alias for cat6 |
| `calculate_smoke_simple` | smoking-status.R | Category grouping | L7 | Gold | Uses nested helpers |
| `calculate_SMK_06A_cont` | smoking-cessation.R | Cat-to-continuous | L4 | Gold | Quit timing midpoints |
| `calculate_time_quit_smoking_complete` | smoking-cessation.R | Pathway branching | L6 | Gold | 5 pathways |
| `calculate_time_quit_smoking_daily` | smoking-cessation.R | Multi-source routing | L6 | Gold | Master > PUMF priority |
| `assess_quit_pathway` | smoking-cessation.R | Multi-source routing | L5 | Gold | Pathway classifier |
| `calculate_SMKG203_cont` | smoke-start.R | Multi-source routing | L5 | Gold | Filter: daily only |
| `calculate_SMKG207_cont` | smoke-start.R | Multi-source routing | L5 | Gold | Filter: former daily only |
| `calculate_SMKG040_cont` | smoke-start.R | Multi-source routing | L7 | Gold | Combines 203 + 207 |
| `calculate_age_start_smoking` | smoke-start.R | Pass-through | L3 | Gold | Via derive_passthrough |
| `calculate_age_first_cigarette` | smoke-start.R | Pass-through | L3 | Gold | Via derive_passthrough |
| `calculate_smoked_100_lifetime` | smoke-start.R | Pass-through | L3 | Gold | Via derive_passthrough |
| `calculate_cigs_per_day` | smoke-intensity.R | Multi-source routing | L7 | Gold | Status-based source routing |
| `calculate_pack_years` | smoke-pack-years.R | Formula calculation | L7 | Gold | Full decision tree |
| `calculate_pack_years_categorical` | smoke-pack-years.R | Category grouping | L7 | Gold | Uses PACK_YEARS_CONSTANTS |

## Doc stub functions (worksheet-only, no R logic)

These functions exist only to document that the variable is harmonised via
`rec_with_table()` without custom R code. They call `stop()` with a message.

| Function | File | Variable |
|----------|------|----------|
| `calculate_SMKDSTY_cat5` | smoking-status.R | SMKDSTY_cat5 |
| `calculate_SMKDSTY_cat3` | smoking-status.R | SMKDSTY_cat3 |
| `calculate_SMK_005` | smoking-status.R | SMK_005 |
| `calculate_SMK_030` | smoking-status.R | SMK_030 |
| `calculate_SMK_01A` | smoking-status.R | SMK_01A |
| `calculate_SMKG040_cat` | smoke-start.R | SMKG040_cat |
| `calculate_SMKG203_cat` | smoke-start.R | SMKG203_cat |
| `calculate_SMKG207_cat` | smoke-start.R | SMKG207_cat |
| `calculate_SMK_207` | smoke-start.R | SMK_207 |
| `calculate_SMK_203` | smoke-start.R | SMK_203 |
| `calculate_SMK_204` | smoke-intensity.R | SMK_204 |
| `calculate_SMK_208` | smoke-intensity.R | SMK_208 |
| `calculate_SMK_05B` | smoke-intensity.R | SMK_05B |
| `calculate_SMK_05C` | smoke-intensity.R | SMK_05C |

## Legacy functions (v2, to be deprecated)

| Function | File | Pattern | Level | Tier | Modern replacement |
|----------|------|---------|-------|------|--------------------|
| `time_quit_smoking_fun` | smoking.R | Cat-to-continuous | L4 | Bronze | `calculate_time_quit_smoking_complete/daily` |
| `smoke_simple_fun` | smoking.R | Category grouping | L7 | Bronze | `calculate_smoke_simple` |
| `pack_years_fun` | smoking.R | Formula calculation | L7 | Bronze | `calculate_pack_years` |
| `SMKG040_fun` | smoking.R | Multi-source routing | L5 | Bronze | `calculate_SMKG040_cont` |
| `pack_years_fun_cat` | smoking.R | Category grouping | L2 | Bronze | `calculate_pack_years_categorical` |
| `SMKDSTY_fun` | smoking.R | Category grouping | L7 | Bronze | `calculate_SMKDSTY_cat6` |
| `SMKG203_fun` | smoking.R | Multi-source routing | L5 | Bronze | `calculate_SMKG203_cont` |
| `SMKG207_fun` | smoking.R | Multi-source routing | L5 | Bronze | `calculate_SMKG207_cont` |

## Infrastructure functions (L1)

| Function | File | Purpose |
|----------|------|---------|
| `clean_variables` | clean-variables.R | Input/output cleaning (steps 1 and 3) |
| `parse_range_notation` | clean-variables.R | Parse variable_details.csv range notation |
| `derive_passthrough` | clean-variables.R | L3 helper for pass-through functions |
| `any_missing` | missing-data-functions.R | Vectorised missing detection |
| `get_priority_missing` | missing-data-functions.R | Priority-based missing processor |
| `assign_missing` | missing-data-functions.R | Create typed missing values |

## Helpers (not exported)

| Function | File | Level | Purpose |
|----------|------|-------|---------|
| `smkg_age_midpoint` | smoking.R | L2 | Age-started category → midpoint lookup |
| `.calculate_pack_years_core` | smoke-pack-years.R | L7 | Pure arithmetic for pack-years |
| `process_missing_codes` | clean-variables.R | L1 | Internal missing code conversion |
| `convert_input_to_tagged_na` | clean-variables.R | L1 | Raw → tagged_na conversion |
| `detect_missing_vectorized` | missing-data-functions.R | L1 | Element-wise missing detection |
| `apply_priority_hierarchy` | missing-data-functions.R | L1 | Priority processing |
