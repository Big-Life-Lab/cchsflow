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
# Tests for energy_exp_fun_cat
# ==============================================================================

# --- Category 1: Inactive (< 1.5) ---

test_that("energy_exp_fun_cat returns 1 for 0", {
  expect_equal(energy_exp_fun_cat(0), 1)
})

test_that("energy_exp_fun_cat returns 1 for 0.5", {
  expect_equal(energy_exp_fun_cat(0.5), 1)
})

test_that("energy_exp_fun_cat returns 1 for 1.49", {
  expect_equal(energy_exp_fun_cat(1.49), 1)
})

# --- Category 2: Moderately active (1.5 to < 3.0) ---

test_that("energy_exp_fun_cat returns 2 for 1.5 (boundary)", {
  expect_equal(energy_exp_fun_cat(1.5), 2)
})

test_that("energy_exp_fun_cat returns 2 for 2.0", {
  expect_equal(energy_exp_fun_cat(2.0), 2)
})

test_that("energy_exp_fun_cat returns 2 for 2.99", {
  expect_equal(energy_exp_fun_cat(2.99), 2)
})

# --- Category 3: Active (>= 3.0) ---

test_that("energy_exp_fun_cat returns 3 for 3.0 (boundary)", {
  expect_equal(energy_exp_fun_cat(3.0), 3)
})

test_that("energy_exp_fun_cat returns 3 for 5.0", {
  expect_equal(energy_exp_fun_cat(5.0), 3)
})

test_that("energy_exp_fun_cat returns 3 for 10.0", {
  expect_equal(energy_exp_fun_cat(10.0), 3)
})

# --- NA(a): not applicable ---

test_that("energy_exp_fun_cat returns NA(a) for tagged_na a", {
  expect_equal(energy_exp_fun_cat(haven::tagged_na("a")), "NA(a)")
})

# --- NA(b): missing ---

test_that("energy_exp_fun_cat returns NA(b) for tagged_na b", {
  expect_equal(energy_exp_fun_cat(haven::tagged_na("b")), "NA(b)")
})

test_that("energy_exp_fun_cat returns NA(b) for regular NA", {
  expect_equal(energy_exp_fun_cat(NA_real_), "NA(b)")
})

test_that("energy_exp_fun_cat returns NA(b) for negative values", {
  expect_equal(energy_exp_fun_cat(-1), "NA(b)")
})

# --- Vector inputs ---

test_that("energy_exp_fun_cat works with vector inputs", {
  result <- energy_exp_fun_cat(c(0.5, 2.0, 4.0))
  expect_equal(result, c("1", "2", "3"))
})

test_that("energy_exp_fun_cat handles mixed vector with NA", {
  result <- energy_exp_fun_cat(c(0.5, haven::tagged_na("a"), 3.0))
  expect_equal(result[1], "1")
  expect_equal(result[2], "NA(a)")
  expect_equal(result[3], "3")
})