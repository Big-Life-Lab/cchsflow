# Benchmark: Logging and Condition Handling for cchsflow v4 Engine

Research date: 2026-06-11  
Researcher: Claude (claude-sonnet-4-6)  
Task: Ecosystem benchmark for R logging/condition best practices as input to v4 engine design.

---

## 1. Ecosystem landscape

### 1.1 The standard tidyverse/r-lib stack

**rlang** (https://rlang.r-lib.org/reference/abort.html) provides the condition-signalling foundation:

- `abort()` / `warn()` / `inform()` with `class`, `...` metadata, and `.frequency` control
- `.frequency` argument (introduced rlang 0.4.7): `"always"` (default), `"regularly"` (8 h), `"once"` (per session)
- `.frequency_id` required when `.frequency != "always"` — a string key used to track which conditions have already fired
- `reset_warning_verbosity()` / `reset_message_verbosity()` added in rlang 1.0.3 for testing `.frequency` behaviour
- `class` argument (renamed from deprecated `.subclass` in rlang 0.4.2) creates classed conditions catchable with `tryCatch`/`withCallingHandlers`
- `warn()` / `inform()` signal `"rlang_warning"` / `"rlang_message"` root classes (since rlang 1.0.0)

**cli** (https://cli.r-lib.org/reference/cli_abort.html) wraps rlang with superior formatting:

- `cli_abort()` / `cli_warn()` / `cli_inform()` — pass `...` through to rlang, so `class`, `.frequency`, `.frequency_id` all work
- Bullet syntax: named vector elements with `"i"`, `"x"`, `"v"`, `"!"`, `"*"` prefixes produce semantic formatting
- Glue interpolation with semantic markup: `{.var x}`, `{.code foo()}`, `{.file path}`, `{.help topic}`
- Pluralisation: `{n} row{?s}`
- Respects `NO_COLOR` and `cli.default_handler` option (suppressible in tests)
- Enable globally with `local_use_cli()` in `.onLoad`

**Tidyverse style guide** (https://style.tidyverse.org/errors.html) mandates:
- Use `cli::cli_abort()` as the standard; `stop()` requires "much more work"
- Problem statement first (sentence case, full stop), then `✖` cross bullets for detail, `ℹ` info bullets for context
- `must` for known-invalid inputs; `can't` for unclear failures
- Surround argument names in backticks

**rOpenSci dev guide** (https://devguide.ropensci.org/pkg_building.html) recommends:
- Use cli package or base R `message()`/`warning()` — never `print()`/`cat()` for user-facing output
- Verbosity control at **package level**, not function level (option or env var, e.g., `usethis.quiet`)
- Support multiple levels ("none", "inform", "debug") not binary on/off

### 1.2 Classed conditions — pattern for package authors

From Advanced R (https://adv-r.hadley.nz/conditions.html) and rlang docs:

```r
# Define a condition class (use pkg_* prefix convention)
abort(
  message = "Variable 'SMKDSTY' not found in database 'cchs2022_p'",
  class = c("cchsflow_missing_variable", "cchsflow_error"),
  variable = "SMKDSTY",
  database = "cchs2022_p"
)

# Catch specific class
withCallingHandlers(
  cchsflow_missing_variable = function(cnd) {
    collect_warning(cnd)   # accumulate for end-of-run summary
    cnd_muffle(cnd)        # suppress default printing
  },
  {
    # ... recode loop body ...
  }
)
```

Key properties:
- **withCallingHandlers** vs **tryCatch**: `withCallingHandlers` is non-exiting — execution continues after the handler. `tryCatch` is exiting — it aborts the wrapped expression. For collecting warnings during a long recode loop, `withCallingHandlers` is the right tool.
- `cnd_muffle()` prevents double-printing after handling
- Condition metadata (`variable =`, `database =`) is accessible in the handler as `cnd$variable`, `cnd$database`
- Class hierarchy: `c("cchsflow_missing_variable", "cchsflow_error", "error")` — most specific first

Pattern for accumulating warnings (end-of-run summary):

```r
collect_cnds <- function(expr) {
  collected <- list()
  withCallingHandlers(
    cchsflow_warning = function(cnd) {
      collected[[length(collected) + 1]] <<- cnd
      cnd_muffle(cnd)
    },
    expr
  )
  collected
}
# After the recode loop: summarise collected
```

### 1.3 Once-per-session deduplication

Two approaches exist:

**Approach A — rlang `.frequency = "once"` (recommended)**:
```r
warn(
  "Multiple databases detected for {.var {variable}}",
  class = "cchsflow_db_ambiguity",
  .frequency = "once",
  .frequency_id = paste0("db_ambiguity_", variable)
)
```
- Zero package-level state required
- Survives `options()` resets that would clear a manual cache
- Testable with `reset_warning_verbosity()`

**Approach B — manual env cache (current cchsflow pattern)**:
```r
# Three separate caches in cchsflow:
.cchsflow_cache$pattern_warnings   # in clean-variables.R
.database_warnings_cache           # in missing-pattern-cache.R
.variable_warnings_cache           # in worksheet-getters.R

if (!exists(warning_key, envir = .database_warnings_cache)) {
  assign(warning_key, TRUE, envir = .database_warnings_cache)
  warning("Multiple databases...")
}
```
- Redundant — rlang `.frequency = "once"` does the same thing natively
- Three separate caches that must each be documented and tested
- Cannot be reset for testing without explicitly clearing each environment
- Not testable with `reset_warning_verbosity()`

### 1.4 `logger` package

Homepage: https://daroczig.github.io/logger/  
Version: 0.4.0 (CRAN April 2026 via https://cran.r-project.org/web/packages/logger/logger.pdf)

Levels: TRACE < DEBUG < INFO < SUCCESS < WARN < ERROR < FATAL < OFF

Appenders:
- `appender_console()`, `appender_file()`, `appender_tee()` (both)
- `appender_async()` — non-blocking background writes
- No `appender_memory()` in `logger` (that is `lgr`'s `AppenderMemory`)

Key hooks for condition integration:
- `log_warnings()` — injects `log_warn()` call when `warning()` fires; optional `muffle = TRUE`
- `log_messages()` — same for `message()`
- `log_errors()` — same for `stop()`

Namespace support: each package can have its own threshold and appender:
```r
logger::log_threshold(DEBUG, namespace = "cchsflow")
logger::log_appender(appender_file("cchsflow-recode.log"), namespace = "cchsflow")
```

Formatters include `formatter_json()` for structured/machine-readable output and `formatter_cli()` for cli-formatted messages.

**Assessment**: `logger` is a good choice for an optional, user-controlled logging layer (debug trace of row-level recode decisions). It is NOT a replacement for rlang-classed conditions for error/warning signalling — those are orthogonal concerns.

### 1.5 `lgr` package

Homepage: https://cran.r-project.org/web/packages/lgr/vignettes/lgr.html

Unlike `logger`, `lgr` has `AppenderBuffer` / `AppenderMemory`:
- Retains `LogEvent` objects in memory as an R6 object
- Can flush to a downstream appender on trigger or on-demand
- `$data` returns a `data.frame` of all buffered events
- `$clear()` resets the buffer

This enables a genuine end-of-run summary:
```r
mem_app <- lgr::AppenderBuffer$new(buffer_size = 1000)
lgr::get_logger("cchsflow")$add_appender(mem_app, name = "memory")
# ... recode run ...
warn_df <- mem_app$data[mem_app$data$level <= lgr::WARN, ]
message(nrow(warn_df), " warnings during recode:")
print(warn_df[, c("msg", "variable", "database")])
```

**Assessment**: `lgr` is heavier (R6-based) but uniquely supports in-memory event collection for post-run summaries. Worth considering for v4's optional verbose/debug mode.

---

## 2. Current cchsflow condition handling audit

### 2.1 recode-with-table.R (legacy engine)

Pattern: raw `stop()`, `warning()`, `message()` throughout — ~15 stop() calls, 3 warning() calls, 5 message() calls.

- No classed conditions
- Unformatted string concatenation (`paste(...)`)
- `call. = FALSE` used inconsistently (only once: line 241)
- No deduplication — every warning fires on every call
- `message()` used for informational side effects that should be `cli_inform()` or suppressed by default

### 2.2 clean-variables.R (v3 Level 6)

Pattern: raw `stop()`, one `warning()`, tryCatch for pattern lookup.

- Has manual cache `.cchsflow_cache$pattern_warnings` for once-per-session warnings (line 15)
- Cache is a module-level env — session state not documented, not testable
- One good pattern: fallback warning fires once per variable name, not once per row

### 2.3 missing-pattern-cache.R (v3 Level 4)

Pattern: raw `stop()`, `warning()`, `tryCatch`.

- `.database_warnings_cache` env (line 27) — second separate warning cache
- Two categories of deduplicated warnings:
  - `config_fallback_warned` — fires once per session when database config not available
  - `{variable_name}_db_selection` — fires once per variable when multiple databases found
- Warning deduplication logic is correct but verbose (exists/assign dance)
- No classed conditions — downstream code cannot catch specific warning types

### 2.4 worksheet-getters.R (v3 Level 3)

Pattern: raw `stop()`, one `warning()`.

- `.variable_warnings_cache` env (line 22) — third separate warning cache
- Same exists/assign deduplication pattern

### 2.5 check-worksheet.R

Pattern: uses `tryCatch` for CSV parse errors; constructs condition-like objects via `list(message=..., ...)` — not using `structure()` with proper condition class, so these are plain lists, not S3 conditions. Cannot be caught by class name.

### 2.6 smoking-status.R, smoke-stop.R, smoke-start.R, smoke-intensity.R

Pattern: raw `stop()` for argument validation. No warnings or messages needed in these pure-function files.

---

## 3. Gap analysis: current vs best practice

| Dimension | Current cchsflow | Best practice | Gap |
|-----------|-----------------|---------------|-----|
| Error signals | `stop()` with `paste()` strings | `cli_abort()` with class + metadata | No machine-catchable classes; no structured context |
| Warning signals | `warning()` with `paste()` strings | `cli_warn()` with class + `.frequency`/`.frequency_id` | No classes; manual dedup caches fragile |
| Info messages | `message()` — always fires | `cli_inform()` with opt-out option | No package-level verbosity control |
| Deduplication | 3 separate manual env caches | rlang `.frequency = "once"` + `.frequency_id` | Duplicated logic; not testable with `reset_warning_verbosity()` |
| End-of-run summary | Not implemented | `withCallingHandlers` + `lgr::AppenderBuffer` or manual collection | No summary capability at all |
| Check-worksheet conditions | Plain list objects | `structure(list(...), class = c("cchsflow_worksheet_error", "error"))` | Not catchable by class; not S3 conditions |
| Logging levels | None | `logger` or `lgr` for debug traces | No DEBUG/TRACE level for developer diagnostics |
| Verbosity control | Per-call `notes = TRUE/FALSE` | Package-level option `cchsflow.verbose` | Option-based suppression not implemented |

---

## 4. Concrete v4 logging and condition design

### 4.1 Condition class hierarchy

```
error
  └─ cchsflow_error
       ├─ cchsflow_worksheet_error
       │    ├─ cchsflow_column_order_error
       │    ├─ cchsflow_row_sort_error
       │    └─ cchsflow_csv_format_error
       ├─ cchsflow_recode_error
       │    ├─ cchsflow_missing_variable_error  (variable not found in data)
       │    ├─ cchsflow_duplicate_from_error    (duplicate recStart values)
       │    └─ cchsflow_invalid_interval_error
       └─ cchsflow_engine_error                 (internal/unexpected)

warning
  └─ cchsflow_warning
       ├─ cchsflow_missing_variable_warning    (variable absent → skip, not halt)
       ├─ cchsflow_db_ambiguity_warning        (multiple databases matched)
       ├─ cchsflow_pattern_fallback_warning    (no config → default pattern used)
       └─ cchsflow_interval_default_warning    (invalid interval → using default)

message
  └─ cchsflow_message
       ├─ cchsflow_load_message               (loading built-in worksheets)
       └─ cchsflow_recode_progress_message    (per-variable progress)
```

### 4.2 Signal functions

```r
# In R/conditions.R (new file)

cchsflow_abort <- function(message, class, ..., call = rlang::caller_env()) {
  cli::cli_abort(
    message,
    class = c(class, "cchsflow_error"),
    ...,
    call = call
  )
}

cchsflow_warn <- function(message, class, ...,
                          .frequency = "always",
                          .frequency_id = NULL) {
  cli::cli_warn(
    message,
    class = c(class, "cchsflow_warning"),
    ...,
    .frequency = .frequency,
    .frequency_id = .frequency_id
  )
}

cchsflow_inform <- function(message, ...) {
  if (getOption("cchsflow.verbose", default = TRUE)) {
    cli::cli_inform(message, class = c("cchsflow_message"), ...)
  }
}
```

### 4.3 Once-per-session deduplication (replacing 3 manual caches)

```r
# Replace exists/assign dance in missing-pattern-cache.R and worksheet-getters.R:

# BEFORE (current):
if (!exists(warning_key, envir = .database_warnings_cache)) {
  assign(warning_key, TRUE, envir = .database_warnings_cache)
  warning("Multiple databases...")
}

# AFTER (v4):
cchsflow_warn(
  "Multiple databases available for {.var {variable_name}}: {db_list}. Auto-selected {.val {selected_db}}.",
  class = "cchsflow_db_ambiguity_warning",
  variable = variable_name,
  selected_db = selected_db,
  .frequency = "once",
  .frequency_id = paste0("db_ambiguity_", variable_name)
)
```

### 4.4 End-of-run summary

Implement via `withCallingHandlers` wrapper around the recode loop:

```r
rec_with_table_safe <- function(...) {
  collected_warnings <- list()
  
  result <- withCallingHandlers(
    cchsflow_warning = function(cnd) {
      collected_warnings[[length(collected_warnings) + 1]] <<- cnd
      cnd_muffle(cnd)
    },
    rec_with_table(...)
  )
  
  if (length(collected_warnings) > 0) {
    n <- length(collected_warnings)
    cli::cli_warn(
      c(
        "Recode completed with {n} warning{?s}.",
        "i" = "Call {.run cchsflow::last_recode_warnings()} to review."
      ),
      class = "cchsflow_recode_summary_warning"
    )
    .cchsflow_last_warnings <<- collected_warnings
  }
  
  result
}

last_recode_warnings <- function() {
  if (!exists(".cchsflow_last_warnings")) {
    cli::cli_inform("No warnings from the last recode run.")
    return(invisible(NULL))
  }
  lapply(.cchsflow_last_warnings, function(cnd) {
    list(class = class(cnd)[1], message = cnd$message,
         variable = cnd$variable, database = cnd$database)
  })
}
```

### 4.5 Verbosity / logging levels

```r
# Package-level option (set in .onLoad):
options(cchsflow.verbose = TRUE)   # shows cchsflow_message
options(cchsflow.debug = FALSE)    # shows per-row trace (very chatty)

# For debug trace, use logger with package namespace:
if (getOption("cchsflow.debug", FALSE)) {
  logger::log_debug(
    "Processing variable {variable}: row {i} of {n}",
    namespace = "cchsflow"
  )
}
```

Users can redirect debug output:
```r
options(cchsflow.debug = TRUE)
logger::log_appender(logger::appender_file("recode-debug.log"), namespace = "cchsflow")
```

### 4.6 Testing conditions

```r
# testthat / snapshot tests:
expect_warning(
  rec_with_table(...),
  class = "cchsflow_missing_variable_warning"
)

# Test once-per-session dedup:
rlang::reset_warning_verbosity("db_ambiguity_SMKDSTY")
expect_warning(my_fn(), class = "cchsflow_db_ambiguity_warning")

# Suppress in tests where not relevant:
suppressWarnings(
  rec_with_table(...),
  classes = "cchsflow_load_message"
)
```

---

## 5. Overlap with cchsflow-specific concerns

### The 3-step architecture and condition signalling

The v3 3-step functions (`clean_variables()` → `case_when` → output validation) generate condition signals at two layers:

1. **Layer 1 (clean_variables input)**:  convert raw CCHS codes → tagged_na. Signals: `cchsflow_pattern_fallback_warning` (once per variable/session), `cchsflow_missing_variable_error` (if variable missing from data entirely).

2. **Layer 2 (case_when body)**: domain logic. Should NOT signal conditions directly — any error is a programming error in the worksheet (use `cchsflow_recode_error`).

3. **Layer 3 (output validation)**: check output is in expected range. Signals: `cchsflow_recode_summary_warning` (end-of-run), `cchsflow_invalid_interval_error` (schema mismatch).

### check-worksheet.R

The existing plain-list condition objects (`list(message=..., ...)`) should be replaced with proper S3 conditions using `structure()` with `class = c("cchsflow_worksheet_error", "error")`. This enables `tryCatch(cchsflow_worksheet_error = ...)` in any validator script.

### Suppression of legacy messages in rec_with_table

Lines 171–181 of recode-with-table.R use `message()` for loading notifications ("No variable_details detected..."). In v4 these should become `cchsflow_inform()` which respects `cchsflow.verbose = FALSE`.

---

## 6. Package dependency implications

Current `DESCRIPTION` already includes `cli` in `Depends`. rlang is an implicit dependency through cli (cli Imports rlang). For v4:

- `cli` (already present): for `cli_abort`, `cli_warn`, `cli_inform`
- `rlang` (add explicitly to Imports): for `.frequency`/`.frequency_id`, `reset_warning_verbosity`, `cnd_muffle`, `withCallingHandlers`
- `logger` (suggest, not require): for optional debug trace. Package-level logging should be opt-in — rOpenSci guidance says use package-level option, not a hard dependency on a logging framework.

---

## 7. Sources

- https://cli.r-lib.org/reference/cli_abort.html — cli_abort/cli_warn/cli_inform API
- https://rlang.r-lib.org/reference/abort.html — abort/warn/inform with .frequency, class, metadata
- https://rlang.r-lib.org/reference/topic-condition-formatting.html — bullet structure, prefixes
- https://rlang.r-lib.org/news/index.html — rlang changelog (.frequency v0.4.7, reset_*_verbosity v1.0.3)
- https://adv-r.hadley.nz/conditions.html — condition system, withCallingHandlers vs tryCatch, accumulation
- https://blog.r-hub.io/2023/11/30/cliff-notes-about-cli/ — package author patterns for cli
- https://style.tidyverse.org/errors.html — tidyverse style guide for error messages
- https://devguide.ropensci.org/pkg_building.html — rOpenSci guidance on messaging and verbosity
- https://daroczig.github.io/logger/ — logger package overview
- https://daroczig.github.io/logger/reference/index.html — logger function reference (appenders, formatters, log_warnings)
- https://rdrr.io/cran/logger/man/log_warnings.html — log_warnings() API
- https://cran.r-project.org/web/packages/lgr/vignettes/lgr.html — lgr AppenderBuffer for in-memory collection
- https://www.rdocumentation.org/packages/logger/versions/0.4.0 — logger package docs
