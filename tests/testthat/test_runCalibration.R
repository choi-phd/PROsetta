library(mirt)

test_that("runCalibration", {

  d <- data_asq

  skip_on_cran()

  set.seed(1)
  calib <- runCalibration(d, technical = list(NCYCLES = 1000))
  ipar <- mirt::coef(calib, simplify = TRUE)$items
  expect_equal(sum(ipar), -553.4556, tolerance = 1e-04)

  set.seed(1)
  calib <- runCalibration(d, fix_method = "item", technical = list(NCYCLES = 1000))
  ipar <- mirt::coef(calib, IRTpars = TRUE, simplify = TRUE)$items

  ipar_anchor_original <- d@anchor[, c("a", "cb1", "cb2", "cb3", "cb4")]
  ipar_anchor_original
  n_anchor_items <- dim(ipar_anchor_original)[1]
  d <- ipar[1:n_anchor_items, ] - ipar_anchor_original
  expect_equal(sum(d), 0, tolerance = 1e-06)

})
