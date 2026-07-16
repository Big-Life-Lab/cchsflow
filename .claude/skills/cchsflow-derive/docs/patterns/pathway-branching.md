# Pattern: Pathway branching

## What it is

A function that routes to different sources or calculations based on a
respondent's pathway through a complex decision tree. The branching is
driven by status variables that determine which data is applicable and
how to interpret it.

This is the most complex pattern. It combines elements of multi-source
routing and category grouping, but with pathway-aware logic that makes
the branching non-trivial.

## When to use

- Multiple pathways exist for the same concept (e.g., quit timing depends
  on whether the person quit directly or reduced gradually)
- A gate variable determines which pathway applies
- Different formulas or sources apply to each pathway
- Examples: time since quitting (direct quit vs gradual reducer vs
  occasional-only), cessation pathway assessment

## How to recognise from worksheet

```
variable,                          variableStart,                                              recEnd
time_quit_smoking_complete,        DerivedVar::[SMKDSTY_cat5,SMK_10_gate,...,SMKDVSTP],        Func::calculate_time_quit_smoking_complete
```

DerivedVar with many feeders including a gate/pathway variable AND a
Master priority source. The feeder list is long because each pathway
needs its own source.

## Bronze template

Not recommended for this pattern. The branching logic is complex enough
that skipping missing data handling creates subtle bugs. Start at silver.

## Silver template

```r
#' @title Calculate [pathway-branched variable]
#'
#' @description Routes to the appropriate source based on [pathway variable].
#'
#' @param status Smoking status category.
#' @param gate Pathway gate variable (1 = path A, 2 = path B).
#' @param source_a Source for pathway A.
#' @param source_b Source for pathway B.
#' @param source_master Master exact value (priority when available).
#'
#' @return Numeric vector of routed values.
#'
#' @examples
#' # Scalar — pathway A
#' calculate_my_branched_var(status = 4, gate = 1, source_a = 3.5,
#'                          source_b = NA)
#'
#' # Scalar — pathway B
#' calculate_my_branched_var(status = 4, gate = 2, source_a = NA,
#'                          source_b = 5.2)
#'
#' # Vector — mixed pathways
#' calculate_my_branched_var(
#'   status = c(4, 4, 5),
#'   gate = c(1, 2, NA),
#'   source_a = c(3.5, NA, 2.0),
#'   source_b = c(NA, 5.2, NA)
#' )
#'
#' @export
calculate_my_branched_var <- function(status, gate, source_a, source_b,
                                      source_master = NULL) {
  dplyr::case_when(
    # Master priority
    !is.na(source_master) ~ source_master,
    # Universe: only former smokers
    status %in% c(1, 2, 3, 6) ~ NA_real_,
    # Pathway A
    gate == 1 ~ source_a,
    # Pathway B
    gate == 2 ~ source_b,
    .default = NA_real_
  )
}
```

## Gold template

```r
calculate_my_branched_var <- function(status, gate, source_a, source_b,
                                      source_master = NULL,
                                      output_format = "tagged_na") {
  # Step 1: Clean inputs — always tagged_na for Step 2
  cleaned <- clean_variables(
    vars = list(
      status = status,
      gate = gate,
      source_a = source_a,
      source_b = source_b,
      source_master = source_master
    ),
    output_format = "tagged_na"
  )

  # Step 2: Domain logic — pathway-aware routing
  result <- dplyr::case_when(
    # Missing status → propagate
    any_missing(cleaned$status) ~
      get_priority_missing(cleaned$status),

    # Universe: not applicable to current/never smokers
    cleaned$status %in% c(1, 2, 3, 6) ~
      assign_missing("not_applicable", "my_branched_var", output_format),

    # Master priority: exact value available → use it
    !any_missing(cleaned$source_master) ~ cleaned$source_master,

    # Former occasional (no gate needed) → source_a
    cleaned$status == 5 & !any_missing(cleaned$source_a) ~
      cleaned$source_a,

    # Former daily, pathway A (direct quit)
    cleaned$status == 4 & cleaned$gate == 1 &
      !any_missing(cleaned$source_a) ~ cleaned$source_a,

    # Former daily, pathway B (gradual reducer)
    cleaned$status == 4 & cleaned$gate == 2 &
      !any_missing(cleaned$source_b) ~ cleaned$source_b,

    # Former daily, no gate (early cycles) → fallback to source_a
    cleaned$status == 4 & any_missing(cleaned$gate) &
      !any_missing(cleaned$source_a) ~ cleaned$source_a,

    # All pathways exhausted
    .default = get_priority_missing(cleaned$source_a, cleaned$source_b,
                                     cleaned$source_master)
  )

  # Step 3: Clean outputs — user's requested format
  output_clean <- clean_variables(
    vars = list(my_branched_var = result),
    output_format = output_format
  )
  output_clean$my_branched_var
}
```

## Reference implementations

- `calculate_time_quit_smoking_complete()` — R/smoking-cessation.R
  (the canonical example: Master priority → occasional pathway →
  daily/direct quit → daily/gradual reducer → 2001 fallback)
- `assess_quit_pathway()` — R/smoking-cessation.R (L5: classifies which
  pathway a respondent follows)

## Common mistakes

- Not handling the "no gate" case for early cycles (2001-2005 don't have
  the gate variable — need a fallback pathway)
- Forgetting that pathway variables may themselves be missing —
  `any_missing(cleaned$gate)` is a valid condition, not an error
- Making the `.default` arm too aggressive — if all pathways failed, use
  `get_priority_missing()` to propagate the best missing code, not just
  NA::a
- Not documenting the pathway decision tree in `@details` — this pattern
  is complex enough that future maintainers need a prose explanation
