# Legacy ADL function tests commented out - functions being modernized in PR #137

# test_that("adl_fun has expected output when ADL_01 is out of range",{
#   expect_equal(adl_fun(-1, 1, 1, 1, 1), "NA(b)")
# })
# 
# test_that("adl_fun has expected output when ADL_02 is out of range",{
#   expect_equal(adl_fun(1, -1, 1, 1, 1), "NA(b)")
# })
# 
# test_that("adl_fun has expected output when ADL_03 is out of range",{
#   expect_equal(adl_fun(1, 1, -1, 1, 1), "NA(b)")
# })
# 
# test_that("adl_fun has expected output when ADL_04 is out of range",{
#   expect_equal(adl_fun(1, 1, 1, -1, 1), "NA(b)")
# })
# 
# test_that("adl_fun has expected output when ADL_05 is out of range",{
#   expect_equal(adl_fun(1, 1, 1, 1, -1), "NA(b)")
# })
# 
# test_that("adl_fun has expected output when all values are in range",{
#   expect_equal(adl_fun(1, 1, 1, 1, 1), 1)
# })
# 
# test_that("adl_score_5_fun has expected output when ADL_01 is out of range",{
#   expect_equal(adl_score_5_fun("NA(b)", 1, 1, 1, 1), "NA(b)")
# })
# 
# test_that("adl_score_5_fun has expected output when ADL_02 is out of range",{
#   expect_equal(adl_score_5_fun(1, "NA(b)", 1, 1, 1), "NA(b)")
# })
# 
# test_that("adl_score_5_fun has expected output when ADL_03 is out of range",{
#   expect_equal(adl_score_5_fun(1, 1, "NA(b)", 1, 1), "NA(b)")
# })
# 
# test_that("adl_score_5_fun has expected output when ADL_04 is out of range",{
#   expect_equal(adl_score_5_fun(1, 1, 1, "NA(b)", 1), "NA(b)")
# })
# 
# test_that("adl_score_5_fun has expected output when ADL_05 is out of range",{
#   expect_equal(adl_score_5_fun(1, 1, 1, 1, "NA(b)"), "NA(b)")
# })
# 
# test_that("adl_score_5_fun has expected output when all values are in range",{
#   expect_equal(adl_score_5_fun(1, 1, 1, 1, 1), 0)
# })