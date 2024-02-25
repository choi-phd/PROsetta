library(mirt)

test_that("runCalibration() works", {

  d <- data_asq

  skip_on_cran()
  skip_on_ci()

  n_cycles <- 1e5
  tol <- 1e-6
  codetest_tol <- 1e-4

  # free calibration, used for diagnostics -------------------------------------
  set.seed(1)
  o <- runCalibration(d, technical = list(NCYCLES = n_cycles), TOL = tol)
  ipar <- mirt::coef(o, simplify = TRUE)$items
  expect_equal(sum(ipar), -552.6381, tolerance = codetest_tol)

  # fixed-parameter calibration, used for linking ------------------------------
  set.seed(1)
  o <- runCalibration(d, fix_method = "item", technical = list(NCYCLES = n_cycles), TOL = tol)
  ipar <- mirt::coef(o, IRTpars = TRUE, simplify = TRUE)$items

  # anchoring parameters
  ipar_anchor_original <- extractAnchorParameters(d, as = "AB")
  use_these <- unique(c(
    grep("^a$", names(ipar_anchor_original), value = TRUE),
    grep("^cb[1-9]$", names(ipar_anchor_original), value = TRUE)
  ))
  ipar_anchor_original <- ipar_anchor_original[, use_these]
  ipar_diff <- ipar[rownames(ipar_anchor_original), ] - ipar_anchor_original
  expect_equal(sum(ipar_diff), 0, tolerance = codetest_tol)

  # new item parameters
  expect_equal(sum(ipar), 364.7369, tolerance = codetest_tol)

  # free calibration, used for diagnostics -------------------------------------
  set.seed(1)
  o <- runCalibration(d, dimensions = 2, technical = list(NCYCLES = n_cycles), TOL = tol)
  ipar <- mirt::coef(o, simplify = TRUE)$items
  expect_equal(sum(ipar), -593.5108, tolerance = codetest_tol)

  # fixed-parameter calibration, used for linking ------------------------------
  # use calibrated projection (with anchoring)
  set.seed(1)
  o <- runCalibration(d, dimensions = 2, fix_method = "item", technical = list(NCYCLES = n_cycles), TOL = tol)
  ipar <- mirt::coef(o, simplify = TRUE)$items

  # source parameters
  ipar_anchor_original <- extractAnchorParameters(d, as = "AD")
  use_these <- unique(c(
    grep("^a[1-9]$", names(ipar_anchor_original), value = TRUE),
    grep("^cb[1-9]$", names(ipar_anchor_original), value = TRUE)
  ))
  ipar_anchor_original <- ipar_anchor_original[, use_these]
  ipar_diff <- ipar[rownames(ipar_anchor_original), -2] - ipar_anchor_original
  expect_equal(sum(ipar_diff), 0, tolerance = codetest_tol)

  # new item parameters
  expect_equal(sum(ipar), -526.6563, tolerance = codetest_tol)

  # ignore_nonconv works -------------------------------------------------------
  set.seed(1)
  expect_error( runCalibration(d, technical = list(NCYCLES = 1)) )
  set.seed(1)
  expect_warning( runCalibration(d, ignore_nonconv = TRUE, technical = list(NCYCLES = 1)) )

})
