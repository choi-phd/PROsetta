library(mirt)

test_that("runLinking", {

  d <- data_asq

  skip_on_cran()
  skip_on_travis()
  skip_on_os("mac")
  skip_on_os("linux")
  skip_on_os("solaris")

  # Hard-coded values are from Windows

  set.seed(1)
  calib <- runLinking(d, method = "MM", technical = list(NCYCLES = 1000))
  expect_equal(prod(calib$constants), -0.1123204, tolerance = 1e-03)
  expect_equal(sum(calib$ipar_linked), 364.0231, tolerance = 1e-03)

  set.seed(1)
  calib <- runLinking(d, method = "MS", technical = list(NCYCLES = 1000))
  expect_equal(prod(calib$constants), -0.1744304, tolerance = 1e-03)
  expect_equal(sum(calib$ipar_linked), 361.7756, tolerance = 1e-03)

  set.seed(1)
  calib <- runLinking(d, method = "HB", technical = list(NCYCLES = 1000))
  expect_equal(prod(calib$constants), -0.175121, tolerance = 1e-03)
  expect_equal(sum(calib$ipar_linked), 361.1959, tolerance = 1e-03)

  set.seed(1)
  calib <- runLinking(d, method = "SL", technical = list(NCYCLES = 1000))
  expect_equal(prod(calib$constants), -0.1620018, tolerance = 1e-03)
  expect_equal(sum(calib$ipar_linked), 362.1947, tolerance = 1e-03)

  set.seed(1)
  calib <- runLinking(d, method = "FIXEDPAR", technical = list(NCYCLES = 1000))
  n_anchor_items <- dim(calib$ipar_anchor)[1]
  par_diff <- calib$ipar_linked[1:n_anchor_items, ] - calib$ipar_anchor
  expect_equal(sum(par_diff), 0, tolerance = 1e-08)
  expect_equal(sum(calib$ipar_linked), 364.7372, tolerance = 1e-03)

  set.seed(1)
  calib <- runLinking(d, method = "CP", technical = list(NCYCLES = 1000))
  n_anchor_items <- dim(calib$ipar_anchor)[1]
  par_diff <- calib$ipar_linked[1:n_anchor_items, -2] - calib$ipar_anchor
  expect_equal(sum(par_diff), 0, tolerance = 1e-08)
  expect_equal(sum(calib$ipar_linked), -526.696, tolerance = 1e-03)

})
