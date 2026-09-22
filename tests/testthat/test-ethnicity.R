# Worksheet regression tests — SDCDVVM and SDCDCGT (re issue #196 successor review)

# --- SDCDVVM: standalone 2019+ visible minority variable ---

test_that("SDCDVVM exists in variables", {
  v <- variables[variables$variable == "SDCDVVM", ]
  expect_equal(nrow(v), 1)
  expect_true(grepl("cchs2019_2020_m", v$databaseStart))
})

test_that("SDCDVVM has 13 substantive categories", {
  vd <- variable_details[variable_details$variable == "SDCDVVM" &
    !grepl("^NA", variable_details$recEnd), ]
  expect_equal(nrow(vd), 13)
})

test_that("SDCDVVM covers all 2019+ cycles", {
  vd <- variable_details[variable_details$variable == "SDCDVVM", ]
  dbs <- unique(vd$databaseStart)
  expect_true(all(grepl("cchs2019_2020_m", dbs)))
  expect_true(all(grepl("cchs2021_m", dbs)))
  expect_true(all(grepl("cchs2022_m", dbs)))
  expect_true(all(grepl("cchs2023_m", dbs)))
})

test_that("SDCDVVM category 13 is Not a visible minority", {
  vd <- variable_details[variable_details$variable == "SDCDVVM" &
    variable_details$recEnd == "13", ]
  expect_equal(nrow(vd), 1)
  expect_equal(vd$catLabel, "Not a visible minority")
})

test_that("SDCDVVM category 13 has Indigenous note", {
  vd <- variable_details[variable_details$variable == "SDCDVVM" &
    variable_details$recEnd == "13", ]
  expect_true(grepl("Indigenous", vd$notes))
})

# --- SDCDCGT_cat7 and SDCDCGT_2015plus no longer have 2019+ rows ---

test_that("SDCDCGT_cat7 has no 2019+ rows", {
  vd <- variable_details[variable_details$variable == "SDCDCGT_cat7" &
    grepl("cchs2019_2020_m", variable_details$databaseStart), ]
  expect_equal(nrow(vd), 0)
})

test_that("SDCDCGT_2015plus has no 2019+ rows", {
  vd <- variable_details[variable_details$variable == "SDCDCGT_2015plus" &
    grepl("cchs2019_2020_m", variable_details$databaseStart), ]
  expect_equal(nrow(vd), 0)
})

# --- immigration_der no longer covers 2019+ Master cycles ---

test_that("immigration_der Master scope ends at 2017_2018", {
  vd <- variable_details[variable_details$variable == "immigration_der" &
    grepl("cchs2019_2020_m", variable_details$databaseStart), ]
  expect_equal(nrow(vd), 0)
})
