library(mirt)

test_that("runLinking() works", {

  d <- data_asq

  skip_on_cran()
  skip_on_ci()

  n_cycles <- 1e5
  tol <- 1e-6
  test_tol <- 1e-4

  set.seed(1)
  o <- runLinking(d, method = "MM", technical = list(NCYCLES = n_cycles), TOL = tol)
  expect_equal(unname(o$constants)[1], 1.007857, tolerance = test_tol)
  expect_equal(unname(o$constants)[2], -0.109299, tolerance = test_tol)
  expect_equal(sum(o$ipar_linked), 364.0088, tolerance = test_tol)

  set.seed(1)
  o <- runLinking(d, method = "MS", technical = list(NCYCLES = n_cycles), TOL = tol)
  expect_equal(unname(o$constants)[1], 1.044384, tolerance = test_tol)
  expect_equal(unname(o$constants)[2], -0.165325, tolerance = test_tol)
  expect_equal(sum(o$ipar_linked), 361.7414, tolerance = test_tol)

  set.seed(1)
  o <- runLinking(d, method = "HB", technical = list(NCYCLES = n_cycles), TOL = tol)
  expect_equal(unname(o$constants)[1], 1.042146, tolerance = test_tol)
  expect_equal(unname(o$constants)[2], -0.166319, tolerance = test_tol)
  expect_equal(sum(o$ipar_linked), 361.165, tolerance = test_tol)

  set.seed(1)
  o <- runLinking(d, method = "SL", technical = list(NCYCLES = n_cycles), TOL = tol)
  expect_equal(prod(o$constants), -0.1601742, tolerance = test_tol)
  expect_equal(sum(o$ipar_linked), 362.1613, tolerance = test_tol)

  set.seed(1)
  o <- runLinking(d, method = "FIXEDPAR", technical = list(NCYCLES = n_cycles), TOL = tol)
  n_anchor_items <- dim(o$ipar_anchor)[1]
  ipar_diff <- o$ipar_linked[1:n_anchor_items, ] - o$ipar_anchor
  expect_equal(sum(ipar_diff), 0, tolerance = test_tol)
  expect_equal(sum(o$ipar_linked), 364.7369, tolerance = test_tol)

  set.seed(1)
  o <- runLinking(d, method = "CP", technical = list(NCYCLES = n_cycles), TOL = tol)
  n_anchor_items <- dim(o$ipar_anchor)[1]
  ipar_diff <- o$ipar_linked[1:n_anchor_items, -2] - o$ipar_anchor
  expect_equal(sum(ipar_diff), 0, tolerance = test_tol)
  expect_equal(sum(o$ipar_linked), -526.6563, tolerance = test_tol)

})
