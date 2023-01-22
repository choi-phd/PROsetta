library(mirt)

test_that("runCalibration() works", {

  d <- data_asq

  skip_on_cran()
  skip_on_os("mac")
  skip_on_os("linux")
  skip_on_os("solaris")

  # free calibration, used for diagnostics -------------------------------------
  set.seed(1)
  calib <- runCalibration(d, technical = list(NCYCLES = 1000))
  ipar <- mirt::coef(calib, simplify = TRUE)$items
  expect_equal(sum(ipar), -553.4556, tolerance = 1e-04)

  # fixed-parameter calibration, used for linking ------------------------------
  set.seed(1)
  calib <- runCalibration(d, fix_method = "item", technical = list(NCYCLES = 1000))
  ipar <- mirt::coef(calib, IRTpars = TRUE, simplify = TRUE)$items

  ipar_anchor_original <- d@anchor[, c("a", "cb1", "cb2", "cb3", "cb4")]
  n_anchor_items <- dim(ipar_anchor_original)[1]
  ipar_diff <- ipar[1:n_anchor_items, ] - ipar_anchor_original
  expect_equal(sum(ipar_diff), 0, tolerance = 1e-06)

  # free calibration, used for diagnostics -------------------------------------
  # use calibrated projection (without anchoring)
  set.seed(1)
  calib <- runCalibration(d, dimensions = 2, technical = list(NCYCLES = 1000))
  ipar <- mirt::coef(calib, simplify = TRUE)$items
  expect_equal(sum(ipar), -593.1672, tolerance = 1e-04)

  # fixed-parameter calibration, used for linking ------------------------------
  # use calibrated projection (with anchoring)
  set.seed(1)
  calib <- runCalibration(d, dimensions = 2, fix_method = "item", technical = list(NCYCLES = 1000))
  ipar <- mirt::coef(calib, simplify = TRUE)$items

  ipar_anchor_original <- d@anchor[, c("a", "cb1", "cb2", "cb3", "cb4")]
  ipar_anchor_original <- convertABtoAD(ipar_anchor_original)
  n_anchor_items <- dim(ipar_anchor_original)[1]
  ipar_diff <- ipar[1:n_anchor_items, c(1, 3:6)] - ipar_anchor_original
  expect_equal(sum(ipar_diff), 0, tolerance = 1e-06)

  # ignore_nonconv works -------------------------------------------------------
  set.seed(1)
  expect_error( runCalibration(d, technical = list(NCYCLES = 1)) )
  set.seed(1)
  expect_warning( runCalibration(d, ignore_nonconv = TRUE, technical = list(NCYCLES = 1)) )

})
