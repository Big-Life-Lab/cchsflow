# derive_CCC_091_2001to2003-------------------------------

test_that("derive_CCC_091_2001to2003 returns 1 when CCC_91A is positive", {
  expect_equal(derive_CCC_091_2001to2003(1, 2), 1)
})

test_that("derive_CCC_091_2001to2003 returns 1 when CCC_91B is positive", {
  expect_equal(derive_CCC_091_2001to2003(2, 1), 1)
})

test_that("derive_CCC_091_2001to2003 returns 2 when neither is positive", {
  expect_equal(derive_CCC_091_2001to2003(2, 2), 2)
})

test_that("derive_CCC_091_2001to2003 returns NA(a) when both are NA(a)", {
  expect_warning(out <- derive_CCC_091_2001to2003(haven::tagged_na("a"), haven::tagged_na("a")))
  expect_equal(out, "NA(a)")
})

test_that("derive_CCC_091_2001to2003 warns and returns 1 when CCC_91A is out of range but CCC_91B is positive", {
  expect_warning(out <- derive_CCC_091_2001to2003(0, 1))
  expect_equal(out, 1)
})

test_that("derive_CCC_091_2001to2003 warns and returns NA(b) when both args are out of range", {
  expect_warning(out <- derive_CCC_091_2001to2003(0, 0))
  expect_equal(out, "NA(b)")
})

# derive_CCC_091_2005to2008-------------------------------

test_that("derive_CCC_091_2005to2008 returns 1 when CCC_91A is positive", {
  expect_equal(derive_CCC_091_2005to2008(1, 2, 2), 1)
})

test_that("derive_CCC_091_2005to2008 returns 1 when CCC_91E is positive", {
  expect_equal(derive_CCC_091_2005to2008(2, 1, 2), 1)
})

test_that("derive_CCC_091_2005to2008 returns 1 when CCC_91F is positive", {
  expect_equal(derive_CCC_091_2005to2008(2, 2, 1), 1)
})

test_that("derive_CCC_091_2005to2008 returns 2 when none are positive", {
  expect_equal(derive_CCC_091_2005to2008(2, 2, 2), 2)
})

test_that("derive_CCC_091_2005to2008 returns NA(a) when all are NA(a)", {
  expect_warning(out <- derive_CCC_091_2005to2008(haven::tagged_na("a"), haven::tagged_na("a"), haven::tagged_na("a")))
  expect_equal(out, "NA(a)")
})

test_that("derive_CCC_091_2005to2008 warns and returns 1 when CCC_91E is out of range but CCC_91A is positive", {
  expect_warning(out <- derive_CCC_091_2005to2008(1, 0, 2))
  expect_equal(out, 1)
})

test_that("derive_CCC_091_2005to2008 warns and returns NA(b) when all args are out of range", {
  expect_warning(out <- derive_CCC_091_2005to2008(0, 0, 0))
  expect_equal(out, "NA(b)")
})

# resp_condition_fun-------------------------------

test_that("resp_condition_fun has expected output when age is out of range", {
  expect_equal(resp_condition_fun(-1, 1, 1), "NA(b)")
})

test_that("resp_condition_fun has expected output when
          COPD/Emphs is out of range", {
  expect_equal(resp_condition_fun(40, 0, 1), 1)
})

test_that("resp_condition_fun has expected output when
          Asthma is out of range", {
  expect_equal(resp_condition_fun(40, 1, 0), 1)
})

test_that("resp_condition_fun has expected output when age is NA", {
  expect_equal(resp_condition_fun(NA, 1, 1), "NA(b)")
})

test_that("resp_condition_fun has expected output when COPD/Emphs is NA", {
  expect_equal(resp_condition_fun(40, NA, 1), 1)
})

test_that("resp_condition_fun has expected output when Asthma is NA", {
  expect_equal(resp_condition_fun(40, 1, NA), 1)
})

test_that("resp_condition_fun has expected output when all arguments are NA", {
  expect_equal(resp_condition_fun(NA, NA, NA), "NA(b)")
})

test_that("resp_condition_fun has expected output when
          all arguments are in range", {
  expect_equal(resp_condition_fun(40, 1, 1), 1)
})

# categorize_CCC_091 --------------------------------

test_that("categorize_CCC_091 returns 1 for age >= 35 with condition", {
  expect_equal(categorize_CCC_091(40, 1), 1L)
})

test_that("categorize_CCC_091 returns 2 for age < 35 with condition", {
  expect_equal(categorize_CCC_091(25, 1), 2L)
})

test_that("categorize_CCC_091 returns 3 for no condition", {
  expect_equal(categorize_CCC_091(40, 2), 3L)
})

test_that("categorize_CCC_091 returns NA(a) for NA(a) input", {
  expect_equal(categorize_CCC_091(40, haven::tagged_na("a")), "NA(a)")
})

test_that("categorize_CCC_091 returns NA(b) for NA(b) input", {
  expect_equal(categorize_CCC_091(40, haven::tagged_na("b")), "NA(b)")
})

test_that("categorize_CCC_091 handles boundary at age 35", {
  expect_equal(categorize_CCC_091(35, 1), 1L)
  expect_equal(categorize_CCC_091(34, 1), 2L)
})
