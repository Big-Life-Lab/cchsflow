# Worksheet regression tests — SDCDCGT (re issue #196 successor review)
# SDCDVVM code 13 ("Not a visible minority") must not be labelled "White".
# It includes Indigenous respondents and some multiple population-group responses.

# --- SDCDCGT_cat7: 2019+ category 1 must NOT be labelled White ---

test_that("SDCDCGT_cat7 2019+ category 1 is Not a visible minority", {
  vd <- variable_details[variable_details$variable == "SDCDCGT_cat7" &
    grepl("cchs2019_2020_m", variable_details$databaseStart) &
    variable_details$recEnd == "1", ]
  expect_equal(nrow(vd), 1)
  expect_equal(vd$catLabel, "Not a visible minority")
  expect_equal(vd$catLabelLong, "Not a visible minority")
})

test_that("SDCDCGT_cat7 2019+ has no Aboriginal category", {
  vd <- variable_details[variable_details$variable == "SDCDCGT_cat7" &
    grepl("cchs2019_2020_m", variable_details$databaseStart) &
    variable_details$recEnd == "4", ]
  expect_equal(nrow(vd), 0)
})

test_that("SDCDCGT_cat7 2019+ category 1 has construct-break note", {
  vd <- variable_details[variable_details$variable == "SDCDCGT_cat7" &
    grepl("cchs2019_2020_m", variable_details$databaseStart) &
    variable_details$recEnd == "1", ]
  expect_true(grepl("Indigenous", vd$notes))
})

# --- SDCDCGT_2015plus: 2019+ category 1 must NOT be labelled White only ---

test_that("SDCDCGT_2015plus 2019+ category 1 is Not a visible minority", {
  vd <- variable_details[variable_details$variable == "SDCDCGT_2015plus" &
    grepl("cchs2019_2020_m", variable_details$databaseStart) &
    variable_details$recEnd == "1", ]
  expect_equal(nrow(vd), 1)
  expect_equal(vd$catLabel, "Not a visible minority")
})
