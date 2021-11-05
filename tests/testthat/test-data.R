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
    expect_false(is.null(variable_details$typeEnd))
    expect_false(is.null(variable_details$databaseStart))
    expect_false(is.null(variable_details$variableStart))
    expect_false(is.null(variable_details$typeStart))
    expect_false(is.null(variable_details$recEnd))
    expect_false(is.null(variable_details$catLabel))
    expect_false(is.null(variable_details$catLabelLong))
    expect_false(is.null(variable_details$recStart))
    expect_false(is.null(variable_details$units))
    expect_false(is.null(variable_details$catStartLabel))
    expect_false(is.null(variable_details$notes))
  }
)
test_that("variable_details included with cchsflow is not empty", {
  expect_true(nrow(variable_details) > 0)
})

# Testing cchs2001_p
test_that("cchs2001_p dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2001_p, "data.frame", exact = TRUE)
          })
test_that("cchs2001_p dataset included with cchsflow contains
          only the 200 sample rows", {
            expect_true(nrow(cchs2001_p) == 200)
          })

# Testing cchs2003_p
test_that("cchs2003_p dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2003_p, "data.frame", exact = TRUE)
          })
test_that("cchs2003_p dataset included with cchsflow contains
          only the 200 sample rows", {
            expect_true(nrow(cchs2003_p) == 200)
          })

# Testing cchs2005_p
test_that("cchs2005_p dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2005_p, "data.frame", exact = TRUE)
          })
test_that("cchs2005_p dataset included with cchsflow contains
          only the 200 sample rows", {
            expect_true(nrow(cchs2005_p) == 200)
          })

# Testing cchs2007_2008_p
test_that("cchs2007_2008_p dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2007_2008_p, "data.frame", exact = TRUE)
          })
test_that("cchs2007_2008_p dataset included with cchsflow
          contains only the 200 sample rows", {
            expect_true(nrow(cchs2007_2008_p) == 200)
          })

# Testing cchs2009_2010_p
test_that("cchs2009_2010_p dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2009_2010_p, "data.frame", exact = TRUE)
          })
test_that("cchs2009_2010_p dataset included with cchsflow
          contains only the 200 sample rows", {
            expect_true(nrow(cchs2009_2010_p) == 200)
          })

# Testing cchs2011_2012_p
test_that("cchs2011_2012_p dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2011_2012_p, "data.frame", exact = TRUE)
          })
test_that("cchs2011_2012_p dataset included with cchsflow
          contains only the 200 sample rows", {
            expect_true(nrow(cchs2011_2012_p) == 200)
          })

# Testing cchs2013_2014_p
test_that("cchs2013_2014_p dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2013_2014_p, "data.frame", exact = TRUE)
          })
test_that("cchs2013_2014_p dataset included with cchsflow
          contains only the 200 sample rows", {
            expect_true(nrow(cchs2013_2014_p) == 200)
          })

# Testing cchs2014_p
test_that("cchs2013_2014_p dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2014_p, "data.frame", exact = TRUE)
          })
test_that("cchs2013_2014_p dataset included with cchsflow
          contains only the 200 sample rows", {
            expect_true(nrow(cchs2014_p) == 200)
          })

# Testing cchs2015_2016_p
test_that("cchs2015_2016_p dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2015_2016_p, "data.frame", exact = TRUE)
          })
test_that("cchs2015_2016_p dataset included with cchsflow
          contains only the 200 sample rows", {
            expect_true(nrow(cchs2015_2016_p) == 200)
          })

# Testing cchs2017_2018_p
test_that("cchs2017_2018_p dataset included with cchsflow
          is saved in correct format", {
            expect_s3_class(cchs2017_2018_p, "data.frame", exact = TRUE)
          })
test_that("cchs2017_2018_p dataset included with cchsflow
          contains only the 200 sample rows", {
            expect_true(nrow(cchs2017_2018_p) == 200)
          })