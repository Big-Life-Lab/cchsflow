# ==============================================================================
# Tests for derive_EDUDR04_2015plus
# ==============================================================================

# --- Category 4: Post-secondary graduation ---

test_that("derive_EDUDR04_2015plus returns 4 for trade certificate (EHG2_04 = 3)", {
  expect_equal(derive_EDUDR04_2015plus(3, 1, 1, 3), 4)
})

test_that("derive_EDUDR04_2015plus returns 4 for college diploma (EHG2_04 = 4)", {
  expect_equal(derive_EDUDR04_2015plus(3, 1, 1, 4), 4)
})

test_that("derive_EDUDR04_2015plus returns 4 for university below bachelors (EHG2_04 = 5)", {
  expect_equal(derive_EDUDR04_2015plus(3, 1, 1, 5), 4)
})

test_that("derive_EDUDR04_2015plus returns 4 for bachelors degree (EHG2_04 = 6)", {
  expect_equal(derive_EDUDR04_2015plus(3, 1, 1, 6), 4)
})

test_that("derive_EDUDR04_2015plus returns 4 for above bachelors (EHG2_04 = 7)", {
  expect_equal(derive_EDUDR04_2015plus(3, 1, 1, 7), 4)
})

# --- Category 3: Some post-secondary ---

test_that("derive_EDUDR04_2015plus returns 3 for some post-sec with HS credential (EHG2_03=1, EHG2_04=2)", {
  expect_equal(derive_EDUDR04_2015plus(3, 1, 1, 2), 3)
})

test_that("derive_EDUDR04_2015plus returns 3 for some post-sec with less than HS credential (EHG2_03=1, EHG2_04=1)", {
  expect_equal(derive_EDUDR04_2015plus(3, 1, 1, 1), 3)
})

# --- Category 2: High school graduate, no post-secondary ---

test_that("derive_EDUDR04_2015plus returns 2 for HS grad no post-sec (EHG2_02=1, EHG2_03=2)", {
  expect_equal(derive_EDUDR04_2015plus(3, 1, 2, 96), 2)
})

# --- Category 1: Less than secondary ---

test_that("derive_EDUDR04_2015plus returns 1 for grade 8 or lower, no HS, no post-sec", {
  expect_equal(derive_EDUDR04_2015plus(1, 2, 2, 96), 1)
})

test_that("derive_EDUDR04_2015plus returns 1 for grade 9-10, no HS, no post-sec", {
  expect_equal(derive_EDUDR04_2015plus(2, 2, 2, 96), 1)
})

# --- NA(a): Valid skip ---

test_that("derive_EDUDR04_2015plus returns NA(a) when all inputs are valid skip codes", {
  expect_equal(derive_EDUDR04_2015plus(6, 6, 6, 96), haven::tagged_na("a"))
})

test_that("derive_EDUDR04_2015plus returns NA(a) with NA(a) string inputs", {
  expect_equal(derive_EDUDR04_2015plus(haven::tagged_na("a"), haven::tagged_na("a"), haven::tagged_na("a"), haven::tagged_na("a")), haven::tagged_na("a"))
})

# --- NA(b): Missing ---

test_that("derive_EDUDR04_2015plus returns NA(b) when EHG2_02 is DK (7)", {
  expect_equal(derive_EDUDR04_2015plus(3, 7, 2, 96), haven::tagged_na("b"))
})

test_that("derive_EDUDR04_2015plus returns NA(b) when EHG2_02 is refusal (8)", {
  expect_equal(derive_EDUDR04_2015plus(3, 8, 2, 96), haven::tagged_na("b"))
})

test_that("derive_EDUDR04_2015plus returns NA(b) when EHG2_03 is refusal (8)", {
  expect_equal(derive_EDUDR04_2015plus(3, 1, 8, 96), haven::tagged_na("b"))
})

test_that("derive_EDUDR04_2015plus returns NA(b) when EHG2_04 is not stated (99)", {
  expect_equal(derive_EDUDR04_2015plus(3, 1, 1, 99), haven::tagged_na("b"))
})

test_that("derive_EDUDR04_2015plus returns NA(b) when EHG2_04 is DK (97)", {
  expect_equal(derive_EDUDR04_2015plus(3, 1, 1, 97), haven::tagged_na("b"))
})

test_that("derive_EDUDR04_2015plus returns NA(b) when EHG2_01 DK and no HS diploma", {
  expect_equal(derive_EDUDR04_2015plus(7, 2, 2, 96), haven::tagged_na("b"))
})

test_that("derive_EDUDR04_2015plus returns NA(b) with NA(b) string input on EHG2_02", {
  expect_equal(derive_EDUDR04_2015plus(3, haven::tagged_na("b"), 2, 96), haven::tagged_na("b"))
})

test_that("derive_EDUDR04_2015plus returns NA(b) with NA(b) string input on EHG2_04", {
  expect_equal(derive_EDUDR04_2015plus(3, 1, 1, haven::tagged_na("b")), haven::tagged_na("b"))
})

# --- Vector inputs ---

test_that("derive_EDUDR04_2015plus works with vector inputs", {
  result <- derive_EDUDR04_2015plus(
    EHG2_01 = c(1, 3, 3, 3, 6),
    EHG2_02 = c(2, 1, 1, 1, 6),
    EHG2_03 = c(2, 2, 1, 1, 6),
    EHG2_04 = c(96, 96, 1, 6, 96)
  )
  expect_equal(length(result), 5)
  expect_equal(result[1], 1)      # less than secondary
  expect_equal(result[2], 2)      # HS grad, no post-sec
  expect_equal(result[3], 3)      # some post-sec
  expect_equal(result[4], 4)      # post-sec grad (bachelor's)
  expect_equal(result[5], haven::tagged_na("a"))  # valid skip
})

# --- Edge cases ---

test_that("derive_EDUDR04_2015plus: EHG2_01 DK but has HS diploma is not missing", {
  # EHG2_01 = 7 (DK) but EHG2_02 = 1 (has HS diploma)
  # is_missing rule: (EHG2_01 %in% c(7,8,9) & EHG2_02 == 2) — EHG2_02 != 2 here
  expect_equal(derive_EDUDR04_2015plus(7, 1, 2, 96), 2)
})

test_that("derive_EDUDR04_2015plus: cat4 takes priority over cat3", {
  # EHG2_03 = 1 and EHG2_04 = 5 — cat4 checked first
  expect_equal(derive_EDUDR04_2015plus(3, 1, 1, 5), 4)
})
