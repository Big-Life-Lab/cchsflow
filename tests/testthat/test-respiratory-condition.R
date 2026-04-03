# CCC_091_fun1-------------------------------

test_that("CCC_091_fun1 returns 1 when CCC_91A is positive", {
  expect_equal(CCC_091_fun1(1, 2), 1)
})

test_that("CCC_091_fun1 returns 1 when CCC_91B is positive", {
  expect_equal(CCC_091_fun1(2, 1), 1)
})

test_that("CCC_091_fun1 returns 2 when neither is positive", {
  expect_equal(CCC_091_fun1(2, 2), 2)
})

test_that("CCC_091_fun1 returns NA(a) when both are NA(a)", {
  expect_warning(out <- CCC_091_fun1("NA(a)", "NA(a)"))
  expect_equal(out, "NA(a)")
})

test_that("CCC_091_fun1 warns and returns 1 when CCC_91A is out of range but CCC_91B is positive", {
  expect_warning(out <- CCC_091_fun1(0, 1))
  expect_equal(out, 1)
})

test_that("CCC_091_fun1 warns and returns NA(b) when both args are out of range", {
  expect_warning(out <- CCC_091_fun1(0, 0))
  expect_equal(out, "NA(b)")
})

# CCC_091_fun2-------------------------------

test_that("CCC_091_fun2 returns 1 when CCC_91A is positive", {
  expect_equal(CCC_091_fun2(1, 2, 2), 1)
})

test_that("CCC_091_fun2 returns 1 when CCC_91E is positive", {
  expect_equal(CCC_091_fun2(2, 1, 2), 1)
})

test_that("CCC_091_fun2 returns 1 when CCC_91F is positive", {
  expect_equal(CCC_091_fun2(2, 2, 1), 1)
})

test_that("CCC_091_fun2 returns 2 when none are positive", {
  expect_equal(CCC_091_fun2(2, 2, 2), 2)
})

test_that("CCC_091_fun2 returns NA(a) when all are NA(a)", {
  expect_warning(out <- CCC_091_fun2("NA(a)", "NA(a)", "NA(a)"))
  expect_equal(out, "NA(a)")
})

test_that("CCC_091_fun2 warns and returns 1 when CCC_91E is out of range but CCC_91A is positive", {
  expect_warning(out <- CCC_091_fun2(1, 0, 2))
  expect_equal(out, 1)
})

test_that("CCC_091_fun2 warns and returns NA(b) when all args are out of range", {
  expect_warning(out <- CCC_091_fun2(0, 0, 0))
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

# COPD_Emph_der_fun1--------------------------------
test_that("COPD_Emph_der_fun1 has expected output when
          age is out of range", {
  expect_equal(COPD_Emph_der_fun1(-1, 1, 1), "NA(b)")
})

test_that("COPD_Emph_der_fun1 has expected output when
          CCC_91E is out of range", {
  expect_warning(out <- COPD_Emph_der_fun1(20, -1, 1))
  expect_equal(out, 2)
})

test_that("COPD_Emph_der_fun1 has expected output when
          CCC_91F is out of range", {
  expect_warning(out <- COPD_Emph_der_fun1(20, 1, -1))
  expect_equal(out, 2)
})

test_that("COPD_Emph_der_fun1 has expected output when
          all parameters are in range", {
  expect_equal(COPD_Emph_der_fun1(20, 1, 1), 2)
})

# COPD_Emph_der_fun2--------------------------------
test_that("COPD_Emph_der_fun2 has expected output when
          age is out of range", {
  expect_equal(COPD_Emph_der_fun2(-1, 1), "NA(b)")
})

test_that("COPD_Emph_der_fun2 has expected output when
          CCC_091 is out of range", {
  expect_equal(COPD_Emph_der_fun2(20, -1), "NA(b)")
})
test_that("COPD_Emph_der_fun2 has expected output when
          all parameters are in range", {
  expect_equal(COPD_Emph_der_fun2(20, 1), 2)
})
