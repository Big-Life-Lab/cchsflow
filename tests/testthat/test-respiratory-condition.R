# resp_condition_fun1-------------------------------

test_that("resp_condition_fun1 has expected output when age is out of range", {
            expect_equal(resp_condition_fun1(-1, 1, 1), "NA(b)")
          })

test_that("resp_condition_fun1 has expected output when
          COPD/Emphs is out of range", {
            expect_equal(resp_condition_fun1(40, 0, 1), 1)
          })

test_that("resp_condition_fun1 has expected output when
          Asthma is out of range", {
            expect_equal(resp_condition_fun1(40, 1, 0), 1)
          })

test_that("resp_condition_fun1 has expected output when age is NA", {
  expect_equal(resp_condition_fun1(NA, 1, 1), "NA(b)")
})

test_that("resp_condition_fun1 has expected output when COPD/Emphs is NA", {
  expect_equal(resp_condition_fun1(40, NA, 1), 1)
})

test_that("resp_condition_fun1 has expected output when Asthma is NA", {
  expect_equal(resp_condition_fun1(40, 1, NA), 1)
})

test_that("resp_condition_fun1 has expected output when all arguments are NA", {
            expect_equal(resp_condition_fun1(NA, NA, NA), "NA(b)")
          })

test_that("resp_condition_fun1 has expected output when
          all arguments are in range", {
            expect_equal(resp_condition_fun1(40, 1, 1), 1)
          })

# resp_condition_fun2-----------------------

test_that("resp_condition_fun2 has expected output when age is out of range", {
            expect_equal(resp_condition_fun2(-1, 1, 1, 1, 1), "NA(b)")
          })

test_that("resp_condition_fun2 has expected output when
          emphysema is out of range", {
            expect_warning(out <- resp_condition_fun2(40, 3, 1, 1, 1))
            expect_equal(out, 1)
          })

test_that("resp_condition_fun2 has expected output when COPD is out of range", {
            expect_warning(out <- resp_condition_fun2(40, 1, 3, 1, 1))
            expect_equal(out, 1)
          })

test_that("resp_condition_fun2 has expected output when
          chronic bronchitis is out of range", {
            expect_warning(out <- resp_condition_fun2(40, 1, 1, 3, 1))
            expect_equal(out, 1)
          })

test_that("resp_condition_fun2 has expected output when Age is out of range", {
            expect_warning(out <- resp_condition_fun2(40, 1, 3, 1, -1))
            expect_equal(out, 1)
          })

test_that("resp_condition_fun2 has expected output when
          all parameters are in range", {
            expect_equal(resp_condition_fun2(40, 1, 1, 1, 1), 1)
          })

test_that("resp_condition_fun2 has expected output when
          COPD, bronchitis, and asthma are negative but emphysema is
          positive", {
            expect_equal(resp_condition_fun2(40, 1, 2, 2, 2), 1)
          })

test_that("resp_condition_fun2 has expected output when
          emphysema, bronchitis, and asthma are negative but COPD is positive",
          {
            expect_equal(resp_condition_fun2(40, 2, 1, 2, 2), 1)
          })

test_that("resp_condition_fun2 has expected output when
          emphysema, COPD, and asthma are negative but bronchitis is positive",
          {
            expect_equal(resp_condition_fun2(40, 2, 2, 1, 2), 1)
          })

test_that("resp_condition_fun2 has expected output when
          emphysema, COPD, and bronchitis are negative but asthma is positive",
          {
            expect_equal(resp_condition_fun2(40, 2, 2, 2, 1), 1)
          })

test_that("resp_condition_fun2 has expected output when
          all parameters are negative", {
            expect_equal(resp_condition_fun2(40, 2, 2, 2, 2), 3)
          })

test_that("resp_condition_fun2 has expected output when
          age is less than 35 and condition positive", {
            expect_equal(resp_condition_fun2(34, 1, 2, 2, 2), 2)
          })

# resp_condition_fun3--------------------------------

test_that("resp_condition_fun3 has expected output when
          age is out of range", {
            expect_equal(resp_condition_fun3(-1, 1, 1, 1), "NA(b)")
          })

test_that("resp_condition_fun3 has expected output when
          COPD/Emphys is out of range", {
            expect_warning(out <- resp_condition_fun3(35, 4, 1, 1))
            expect_equal(out, 1)
          })

test_that("resp_condition_fun3 has expected output when
          bronchitis is out of range", {
            expect_warning(out <- resp_condition_fun3(35, 1, 4, 1))
            expect_equal(out, 1)
          })

test_that("resp_condition_fun3 has expected output when
          age is out of range", {
            expect_warning(out <- resp_condition_fun3(35, 1, 1, 4))
            expect_equal(out, 1)
          })

test_that("resp_condition_fun3 has expected output when
          all parameters are in range", {
            expect_equal(resp_condition_fun3(35, 1, 1, 1), 1)
          })

test_that("resp_condition_fun3 has expected output when
          COPD/Emphys & asthma are negative but bronchitis is positive", {
            expect_equal(resp_condition_fun3(40, 2, 1, 2), 1)
          })

test_that("resp_condition_fun3 has expected output when
          bronchitis & asthma are negative but COPD/Emphys is positive", {
            expect_equal(resp_condition_fun3(40, 1, 2, 2), 1)
          })

test_that("resp_condition_fun3 has expected output when
          bronchitis & COPD/Emphys are negative but asthma is positive", {
            expect_equal(resp_condition_fun3(40, 2, 2, 1), 1)
          })

test_that("resp_condition_fun3 has expected output when
          all parameters are negative", {
            expect_equal(resp_condition_fun3(40, 2, 2, 2), 3)
          })

test_that("resp_condition_fun3 has expected output when
          age is less than 35 and condition positive", {
            expect_equal(resp_condition_fun3(34, 1, 2, 1), 2)
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
          CCC_91E is out of range", {
            expect_equal(COPD_Emph_der_fun2(20, -1), "NA(b)")
          })
test_that("COPD_Emph_der_fun2 has expected output when
          all parameters are in range", {
            expect_equal(COPD_Emph_der_fun2(20, 1), 2)
          })