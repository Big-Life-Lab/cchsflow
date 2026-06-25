test_that("energy_exp_fun has expected outputs when all inputs are out of
          range", {
        expect_equal(energy_exp_fun(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                     tagged_na("b"))
          })

test_that("energy_exp_fun has expected outputs when all inputs are in
          range", {
        expect_equal(energy_exp_fun(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                     0.0214285714)
          })

# ==============================================================================
# Tests for categorize_energy_exp
# ==============================================================================

# --- Category 1: Inactive (< 1.5) ---

test_that("categorize_energy_exp returns 1 for 0", {
  expect_equal(categorize_energy_exp(0), 1)
})

test_that("categorize_energy_exp returns 1 for 0.5", {
  expect_equal(categorize_energy_exp(0.5), 1)
})

test_that("categorize_energy_exp returns 1 for 1.49", {
  expect_equal(categorize_energy_exp(1.49), 1)
})

# --- Category 2: Moderately active (1.5 to < 3.0) ---

test_that("categorize_energy_exp returns 2 for 1.5 (boundary)", {
  expect_equal(categorize_energy_exp(1.5), 2)
})

test_that("categorize_energy_exp returns 2 for 2.0", {
  expect_equal(categorize_energy_exp(2.0), 2)
})

test_that("categorize_energy_exp returns 2 for 2.99", {
  expect_equal(categorize_energy_exp(2.99), 2)
})

# --- Category 3: Active (>= 3.0) ---

test_that("categorize_energy_exp returns 3 for 3.0 (boundary)", {
  expect_equal(categorize_energy_exp(3.0), 3)
})

test_that("categorize_energy_exp returns 3 for 5.0", {
  expect_equal(categorize_energy_exp(5.0), 3)
})

test_that("categorize_energy_exp returns 3 for 10.0", {
  expect_equal(categorize_energy_exp(10.0), 3)
})

# --- NA(a): not applicable ---

test_that("categorize_energy_exp returns NA(a) for tagged_na a", {
  expect_equal(categorize_energy_exp(haven::tagged_na("a")), haven::tagged_na("a"))
})

# --- NA(b): missing ---

test_that("categorize_energy_exp returns NA(b) for tagged_na b", {
  expect_equal(categorize_energy_exp(haven::tagged_na("b")), haven::tagged_na("b"))
})

test_that("categorize_energy_exp returns NA(b) for regular NA", {
  expect_equal(categorize_energy_exp(NA_real_), haven::tagged_na("b"))
})

test_that("categorize_energy_exp returns NA(b) for negative values", {
  expect_equal(categorize_energy_exp(-1), haven::tagged_na("b"))
})

# --- Vector inputs ---

test_that("categorize_energy_exp works with vector inputs", {
  result <- categorize_energy_exp(c(0.5, 2.0, 4.0))
  expect_equal(result, c(1, 2, 3))
})

test_that("categorize_energy_exp handles mixed vector with NA", {
  result <- categorize_energy_exp(c(0.5, haven::tagged_na("a"), 3.0))
  expect_equal(result[1], 1)
  expect_equal(result[2], haven::tagged_na("a"))
  expect_equal(result[3], 3)
})