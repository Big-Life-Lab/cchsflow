# ==============================================================================
# Tests for sedentary_activity_fun
# ==============================================================================

test_that("sedentary_activity_fun sums all 4 components for age >= 20", {
  expect_equal(sedentary_activity_fun(25, 5, 3, 10, 7), 25)
})

test_that("sedentary_activity_fun returns NA(a) for age < 20", {
  expect_equal(sedentary_activity_fun(18, 5, 3, 10, 7), tagged_na("a"))
})

test_that("sedentary_activity_fun returns NA(a) if any input is NA(a)", {
  expect_equal(sedentary_activity_fun(25, tagged_na("a"), 3, 10, 7), tagged_na("a"))
})

test_that("sedentary_activity_fun returns NA(b) if any input is NA(b)", {
  expect_equal(sedentary_activity_fun(25, tagged_na("b"), 3, 10, 7), tagged_na("b"))
})

test_that("sedentary_activity_fun handles zero values", {
  expect_equal(sedentary_activity_fun(30, 0, 0, 0, 0), 0)
})

test_that("sedentary_activity_fun boundary at age 20", {
  expect_equal(sedentary_activity_fun(20, 1, 2, 3, 4), 10)
})

# ==============================================================================
# Tests for sedentary_activity2_fun
# ==============================================================================

test_that("sedentary_activity2_fun sums 3 components (excl. reading) for age >= 20", {
  expect_equal(sedentary_activity2_fun(25, 5, 3, 10), 18)
})

test_that("sedentary_activity2_fun returns NA(a) for age < 20", {
  expect_equal(sedentary_activity2_fun(18, 5, 3, 10), tagged_na("a"))
})

test_that("sedentary_activity2_fun returns NA(a) if any input is NA(a)", {
  expect_equal(sedentary_activity2_fun(25, 5, tagged_na("a"), 10), tagged_na("a"))
})

test_that("sedentary_activity2_fun returns NA(b) if any input is NA(b)", {
  expect_equal(sedentary_activity2_fun(25, tagged_na("b"), 3, 10), tagged_na("b"))
})

test_that("sedentary_activity2_fun handles zero values", {
  expect_equal(sedentary_activity2_fun(30, 0, 0, 0), 0)
})

test_that("sedentary_activity2_fun boundary at age 20", {
  expect_equal(sedentary_activity2_fun(20, 1, 2, 3), 6)
})

# ==============================================================================
# Tests for weekly_screen_time_fun
# ==============================================================================

test_that("weekly_screen_time_fun returns daily * 7", {
  expect_equal(weekly_screen_time_fun(2), 14)
})

test_that("weekly_screen_time_fun returns 0 for 0 input", {
  expect_equal(weekly_screen_time_fun(0), 0)
})

test_that("weekly_screen_time_fun returns NA(a) for tagged_na a", {
  expect_equal(weekly_screen_time_fun(tagged_na("a")), tagged_na("a"))
})

test_that("weekly_screen_time_fun returns NA(b) for regular NA", {
  expect_equal(weekly_screen_time_fun(tagged_na("b")), tagged_na("b"))
})

# ==============================================================================
# Tests for weekly_screen_time_adult_fun
# ==============================================================================

test_that("weekly_screen_time_adult_fun returns value for age >= 20", {
  expect_equal(weekly_screen_time_adult_fun(25, 3), 21)
})

test_that("weekly_screen_time_adult_fun returns NA(a) for age < 20", {
  expect_equal(weekly_screen_time_adult_fun(18, 3), tagged_na("a"))
})

test_that("weekly_screen_time_adult_fun boundary at age 20", {
  expect_equal(weekly_screen_time_adult_fun(20, 3), 21)
})

test_that("weekly_screen_time_adult_fun returns NA(a) for tagged_na a input", {
  expect_equal(weekly_screen_time_adult_fun(25, tagged_na("a")), tagged_na("a"))
})

test_that("weekly_screen_time_adult_fun returns NA(b) for tagged_na b input", {
  expect_equal(weekly_screen_time_adult_fun(25, tagged_na("b")), tagged_na("b"))
})

# ==============================================================================
# Tests for weekly_screen_time_youth_fun
# ==============================================================================

test_that("weekly_screen_time_youth_fun returns value for age < 20", {
  expect_equal(weekly_screen_time_youth_fun(15, 4), 28)
})

test_that("weekly_screen_time_youth_fun returns NA(a) for age >= 20", {
  expect_equal(weekly_screen_time_youth_fun(25, 4), tagged_na("a"))
})

test_that("weekly_screen_time_youth_fun boundary at age 20", {
  expect_equal(weekly_screen_time_youth_fun(20, 4), tagged_na("a"))
})

test_that("weekly_screen_time_youth_fun returns NA(b) for tagged_na b input", {
  expect_equal(weekly_screen_time_youth_fun(15, tagged_na("b")), tagged_na("b"))
})
