test_that("time_quit_smoking_fun() has expected outputs when
          SMK_09A_B is out of range", {
            expect_equal(time_quit_smoking_fun(10, 2),
                         tagged_na("b"))
          })

test_that("time_quit_smoking_fun() has expected outputs when
          all parameters are in range", {
            expect_equal(time_quit_smoking_fun(2,2),
                         1.5)
          })


# test_that("smoke_simple_fun() has expected outputs when
#           SMKDSTY is out of range", {
#             expect_equal(smoke_simple_fun(100, 2),
#                          "NA(b)")
#           })

test_that("smoke_simple_fun() has expected outputs when
          all parameters are in range", {
            expect_equal(smoke_simple_fun(1,"NA(a)"),
                         1)
          })


test_that("pack_years_fun() has expected outputs when
          SMKDSTY is out of range", {
            expect_equal(pack_years_fun(10, 40, 6, 6, 22, 96, 12, 996, 996, 96, 96),
               tagged_na("b"))
          })

test_that("pack_years_fun() has expected outputs when
          DHHGAGE_cont is out of range", {
            expect_equal(pack_years_fun(1, -1, 6, 6, 22, 96, 12, 996, 996, 96, 96),
                         tagged_na("b"))
          })

test_that("pack_years_fun() has expected outputs when
          all parameters are in range", {
            expect_equal(pack_years_fun(1, 40, 96, 16, 96, 10, 5, 996, 996, 96, 96),
                         12)
          })

test_that("pack_years_fun", {
  test_derived_function(
    test_data <- read.csv("../testdata/pack_years.csv"),
    pack_years_fun
  )
})


# test_that("pack_years_fun_cat() has expected outputs when
#           pack_years_der is out of range", {
#             expect_equal(pack_years_fun_cat(-1),
#                          "NA(b)")
#           })

test_that("pack_years_fun() has expected outputs for former daily current
          occasional smoker", {
  expect_equal(pack_years_fun(2, 45, 10, NA, 21, NA, 5, 20, 15, NA, NA), 15.25)
})

test_that("pack_years_fun_cat() has expected outputs when
          pack_years_der is in range", {
            expect_equal(pack_years_fun_cat(1),
                         3)
          })

test_that("SMKG040_fun() has expected outputs when
          both inputs are out of range", {
            expect_equal(SMKG040_fun(NA, NA),
                         tagged_na("b"))
          })

test_that("SMKG040_fun() has expected outputs when
          all parameters are in range", {
            expect_equal(SMKG040_fun(1, 1),
                         1)
          })

test_that("SMKDSTY_fun() has expected outputs when
          SMK_01A is out of range",{
            expect_equal(SMKDSTY_fun(2, 2, NA),
                         3)
          })

test_that("SMKDSTY_fun() has expected outputs when
          SMK_030 is out of range",{
            expect_equal(SMKDSTY_fun(2, NA, 1),
                         tagged_na("b"))
          })

test_that("SMKDSTY_fun() has expected outputs when
          SMK_005 is out of range",{
            expect_equal(SMKDSTY_fun(NA, 1, 1),
                         tagged_na("a"))
          })

test_that("SMKDSTY_fun() has expected outputs when
          all parameters are in range",{
            expect_equal(SMKDSTY_fun(3, 2, 1),
                         5)
          })

test_that("SMKDSTY_fun() has expected outputs when
          all parameters are out of range",{
            expect_equal(SMKDSTY_fun(NA, NA, NA),
                         tagged_na("b"))
          })

test_that("SMKG203_fun() has expected outputs when
          both inputs are out of range",{
            expect_equal(SMKG203_fun(2, 1),
                         tagged_na("b"))
          })

test_that("SMKG203_fun() has expected outputs when
          both inputs are in range",{
            expect_equal(SMKG203_fun(1, 10),
                         47)
          })

test_that("SMKG203_fun() has expected outputs when
          SMK_005 is out of range",{
            expect_equal(SMKG203_fun("NA(a)", 10),
                         tagged_na("a"))
          })

test_that("SMKG207_fun() has expected outputs when
          both inputs are out of range",{
            expect_equal(SMKG207_fun(2, 1),
                         tagged_na("b"))
          })

test_that("SMKG207_fun() has expected outputs when
          both inputs are in range",{
            expect_equal(SMKG207_fun(1, 10),
                         47)
          })

test_that("SMKG207_fun() has expected outputs when
          SMK_005 is out of range",{
            expect_equal(SMKG207_fun("NA(a)", 10),
                         tagged_na("a"))
          })

# =============================================================================
# calculate_SMKG040 — v3 wrapper combining SMKG203_cont + SMKG207_cont
# =============================================================================

test_that("calculate_SMKG040() returns SMKG203_cont when it is valid", {
  expect_equal(calculate_SMKG040(SMKG203_cont = 22, SMKG207_cont = tagged_na("a")), 22)
})

test_that("calculate_SMKG040() falls back to SMKG207_cont when SMKG203_cont is NA", {
  expect_equal(calculate_SMKG040(SMKG203_cont = tagged_na("a"), SMKG207_cont = 32), 32)
})

test_that("calculate_SMKG040() returns NA(b) when both inputs are NA", {
  expect_true(is_tagged_na(calculate_SMKG040(tagged_na("a"), tagged_na("a")), "b"))
  expect_true(is_tagged_na(calculate_SMKG040(NA_real_, NA_real_), "b"))
})

# =============================================================================
# calculate_SMKG203_continuous — PUMF: filter daily smoker, map grouped→midpoint
# =============================================================================

test_that("calculate_SMKG203_continuous() maps categories to midpoints for daily smoker", {
  midpoints <- c(8, 13, 16, 18.5, 22, 27, 32, 37, 42, 47, 55)
  for (cat in seq_along(midpoints)) {
    expect_equal(calculate_SMKG203_continuous(SMKG005 = 1, SMKG040 = cat), midpoints[cat],
                 info = paste("Category", cat))
  }
})

test_that("calculate_SMKG203_continuous() returns NA(b) for non-daily smokers", {
  expect_true(is_tagged_na(calculate_SMKG203_continuous(SMKG005 = 2, SMKG040 = 3), "b"))
  expect_true(is_tagged_na(calculate_SMKG203_continuous(SMKG005 = 3, SMKG040 = 3), "b"))
})

test_that("calculate_SMKG203_continuous() returns NA(a) when SMKG040 is 'NA(a)'", {
  expect_true(is_tagged_na(calculate_SMKG203_continuous(SMKG005 = 1, SMKG040 = "NA(a)"), "a"))
})

test_that("calculate_SMKG203_continuous() returns NA(b) for out-of-range category", {
  expect_true(is_tagged_na(calculate_SMKG203_continuous(SMKG005 = 1, SMKG040 = 99), "b"))
})

# =============================================================================
# calculate_SMKG203_from_combined — Master: same filtering via SMKG203_fun
# =============================================================================

test_that("calculate_SMKG203_from_combined() maps categories to midpoints for daily smoker", {
  expect_equal(calculate_SMKG203_from_combined(SMK_005 = 1, SMK_040 = 1),   8)
  expect_equal(calculate_SMKG203_from_combined(SMK_005 = 1, SMK_040 = 4),  18.5)
  expect_equal(calculate_SMKG203_from_combined(SMK_005 = 1, SMK_040 = 11), 55)
})

test_that("calculate_SMKG203_from_combined() returns NA(b) for non-daily smokers", {
  expect_true(is_tagged_na(calculate_SMKG203_from_combined(SMK_005 = 2, SMK_040 = 3), "b"))
})

test_that("calculate_SMKG203_from_combined() returns NA(a) when SMK_040 is 'NA(a)'", {
  expect_true(is_tagged_na(calculate_SMKG203_from_combined(SMK_005 = 1, SMK_040 = "NA(a)"), "a"))
})

# =============================================================================
# calculate_SMKG207_continuous — PUMF: filter former daily, map→midpoint
# =============================================================================

test_that("calculate_SMKG207_continuous() maps categories to midpoints for former daily smokers", {
  expect_equal(calculate_SMKG207_continuous(SMKG005 = 2, SMKG030 = 1, SMKG040 = 2),  13)
  expect_equal(calculate_SMKG207_continuous(SMKG005 = 3, SMKG030 = 1, SMKG040 = 9),  42)
  expect_equal(calculate_SMKG207_continuous(SMKG005 = 3, SMKG030 = 1, SMKG040 = 11), 55)
})

test_that("calculate_SMKG207_continuous() returns NA(b) for current daily smokers", {
  expect_true(is_tagged_na(calculate_SMKG207_continuous(SMKG005 = 1, SMKG030 = 1, SMKG040 = 3), "b"))
})

test_that("calculate_SMKG207_continuous() returns NA(b) when SMKG030 != 1", {
  expect_true(is_tagged_na(calculate_SMKG207_continuous(SMKG005 = 3, SMKG030 = 2, SMKG040 = 3), "b"))
})

test_that("calculate_SMKG207_continuous() returns NA(a) when SMKG040 is 'NA(a)' for former daily", {
  expect_true(is_tagged_na(calculate_SMKG207_continuous(SMKG005 = 2, SMKG030 = 1, SMKG040 = "NA(a)"), "a"))
})

# =============================================================================
# calculate_SMKG207_from_combined — Master: same logic, SMK_ prefix
# =============================================================================

test_that("calculate_SMKG207_from_combined() maps categories to midpoints for former daily smokers", {
  expect_equal(calculate_SMKG207_from_combined(SMK_005 = 2, SMK_030 = 1, SMK_040 = 2),  13)
  expect_equal(calculate_SMKG207_from_combined(SMK_005 = 3, SMK_030 = 1, SMK_040 = 7),  32)
  expect_equal(calculate_SMKG207_from_combined(SMK_005 = 3, SMK_030 = 1, SMK_040 = 11), 55)
})

test_that("calculate_SMKG207_from_combined() returns NA(b) for current daily smokers", {
  expect_true(is_tagged_na(calculate_SMKG207_from_combined(SMK_005 = 1, SMK_030 = 1, SMK_040 = 3), "b"))
})

test_that("calculate_SMKG207_from_combined() returns NA(b) when SMK_030 != 1", {
  expect_true(is_tagged_na(calculate_SMKG207_from_combined(SMK_005 = 3, SMK_030 = 2, SMK_040 = 3), "b"))
})

test_that("calculate_SMKG207_from_combined() returns NA(a) when SMK_040 is 'NA(a)' for former daily", {
  expect_true(is_tagged_na(calculate_SMKG207_from_combined(SMK_005 = 2, SMK_030 = 1, SMK_040 = "NA(a)"), "a"))
})
