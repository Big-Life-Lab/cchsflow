# Testing Variables
test_that("variables included with cchsflow is saved in correct format", {
  expect_s3_class(variables, "data.frame", exact = TRUE)
})
test_that("variables included with cchsflow contains the needed columns for
          rec_with_table()", {
            expect_false(is.null(variables$variable))
            expect_false(is.null(variables$label))
            expect_false(is.null(variables$labelLong))
            expect_false(is.null(variables$variableType))
            expect_false(is.null(variables$databaseStart))
            expect_false(is.null(variables$variableStart))

          })
test_that("variables included with cchsflow is not empty", {
  expect_true(nrow(variables) > 0)
})

# Testing variable_details
test_that("variable_details included with cchsflow
          is saved in correct format", {
            expect_s3_class(variable_details, "data.frame", exact = TRUE)
          })
test_that(
  "variable_details included with cchsflow contains the needed columns for
  rec_with_table()", {
    expect_false(is.null(variable_details$variable))
    expect_false(is.null(variable_details$toType))
    expect_false(is.null(variable_details$databaseStart))
    expect_false(is.null(variable_details$variableStart))
    expect_false(is.null(variable_details$fromType))
    expect_false(is.null(variable_details$recTo))
    expect_false(is.null(variable_details$catLabel))
    expect_false(is.null(variable_details$catLabelLong))
    expect_false(is.null(variable_details$recFrom))
    expect_false(is.null(variable_details$units))
    expect_false(is.null(variable_details$catStartLabel))
    expect_false(is.null(variable_details$notes))
  }
)
test_that("variable_details included with cchsflow is not empty", {
  expect_true(nrow(variable_details) > 0)
})

# Testing cchs2001
test_that("cchs2001 dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2001, "data.frame", exact = TRUE)
          })
test_that("cchs2001 dataset included with cchsflow contains
          only the 200 sample rows", {
            expect_true(nrow(cchs2001) == 200)
          })

# Testing cchs2003
test_that("cchs2003 dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2003, "data.frame", exact = TRUE)
          })
test_that("cchs2003 dataset included with cchsflow contains
          only the 200 sample rows", {
            expect_true(nrow(cchs2003) == 200)
          })

# Testing cchs2005
test_that("cchs2005 dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2005, "data.frame", exact = TRUE)
          })
test_that("cchs2005 dataset included with cchsflow contains
          only the 200 sample rows", {
            expect_true(nrow(cchs2005) == 200)
          })

# Testing cchs2007_2008
test_that("cchs2007_2008 dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2007_2008, "data.frame", exact = TRUE)
          })
test_that("cchs2007_2008 dataset included with cchsflow
          contains only the 200 sample rows", {
            expect_true(nrow(cchs2007_2008) == 200)
          })

# Testing cchs2009_2010
test_that("cchs2009_2010 dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2009_2010, "data.frame", exact = TRUE)
          })
test_that("cchs2009_2010 dataset included with cchsflow
          contains only the 200 sample rows", {
            expect_true(nrow(cchs2009_2010) == 200)
          })

# Testing cchs2010
test_that("cchs2010 dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2010, "data.frame", exact = TRUE)
          })
test_that("cchs2010 dataset included with cchsflow
          ontains only the 200 sample rows", {
            expect_true(nrow(cchs2010) == 200)
          })

# Testing cchs2011_2012
test_that("cchs2011_2012 dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2011_2012, "data.frame", exact = TRUE)
          })
test_that("cchs2011_2012 dataset included with cchsflow
          contains only the 200 sample rows", {
            expect_true(nrow(cchs2011_2012) == 200)
          })

# Testing cchs2012
test_that("cchs2012 dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2012, "data.frame", exact = TRUE)
          })
test_that("cchs2012 dataset included with cchsflow
          contains only the 200 sample rows", {
            expect_true(nrow(cchs2012) == 200)
          })

# Testing cchs2013_2014
test_that("cchs2013_2014 dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2013_2014, "data.frame", exact = TRUE)
          })
test_that("cchs2013_2014 dataset included with cchsflow
          contains only the 200 sample rows", {
            expect_true(nrow(cchs2013_2014) == 200)
          })

# Testing cchs2014
test_that("cchs2014 dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2014, "data.frame", exact = TRUE)
          })
test_that("cchs2014 dataset included with cchsflow
          contains only the 200 sample rows", {
            expect_true(nrow(cchs2014) == 200)
          })
