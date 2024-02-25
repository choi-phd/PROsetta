library(mirt)

test_that("runLinking() works", {

  d <- data_asq

  skip_on_cran()
  skip_on_ci()

  n_cycles <- 1e5
  tol <- 1e-6
  codetest_tol <- 1e-4

  set.seed(1)
  o <- runLinking(d, method = "MM", technical = list(NCYCLES = n_cycles), TOL = tol)
  use_these <- unique(c(
    grep("^a$", names(o$ipar_linked), value = TRUE),
    grep("^b[1-9]$", names(o$ipar_linked), value = TRUE)
  ))
  expect_equal(unname(o$constants)[1], 1.007857, tolerance = codetest_tol)
  expect_equal(unname(o$constants)[2], -0.109299, tolerance = codetest_tol)
  expect_equal(sum(o$ipar_linked[, use_these]), 364.0088, tolerance = codetest_tol)

  set.seed(1)
  o <- runLinking(d, method = "MS", technical = list(NCYCLES = n_cycles), TOL = tol)
  expect_equal(unname(o$constants)[1], 1.044384, tolerance = codetest_tol)
  expect_equal(unname(o$constants)[2], -0.165325, tolerance = codetest_tol)
  use_these <- unique(c(
    grep("^a$", names(o$ipar_linked), value = TRUE),
    grep("^b[1-9]$", names(o$ipar_linked), value = TRUE)
  ))
  expect_equal(sum(o$ipar_linked[, use_these]), 361.7414, tolerance = codetest_tol)

  set.seed(1)
  o <- runLinking(d, method = "HB", technical = list(NCYCLES = n_cycles), TOL = tol)
  expect_equal(unname(o$constants)[1], 1.042146, tolerance = codetest_tol)
  expect_equal(unname(o$constants)[2], -0.166319, tolerance = codetest_tol)
  use_these <- unique(c(
    grep("^a$", names(o$ipar_linked), value = TRUE),
    grep("^b[1-9]$", names(o$ipar_linked), value = TRUE)
  ))
  expect_equal(sum(o$ipar_linked[, use_these]), 361.165, tolerance = codetest_tol)

  set.seed(1)
  o <- runLinking(d, method = "SL", technical = list(NCYCLES = n_cycles), TOL = tol)
  expect_equal(prod(o$constants), -0.1601742, tolerance = codetest_tol)
  use_these <- unique(c(
    grep("^a$", names(o$ipar_linked), value = TRUE),
    grep("^b[1-9]$", names(o$ipar_linked), value = TRUE)
  ))
  expect_equal(sum(o$ipar_linked[, use_these]), 362.1613, tolerance = codetest_tol)

  set.seed(1)
  o <- runLinking(d, method = "FIXEDPAR", technical = list(NCYCLES = n_cycles), TOL = tol)
  n_anchor_items <- dim(o$ipar_anchor)[1]
  use_these_linked <- unique(c(
    grep("^a$", names(o$ipar_linked), value = TRUE),
    grep("^b[1-9]$", names(o$ipar_linked), value = TRUE)
  ))
  use_these_anchor <- unique(c(
    grep("^a$", names(o$ipar_anchor), value = TRUE),
    grep("^cb[1-9]$", names(o$ipar_anchor), value = TRUE)
  ))
  ipar_diff <- o$ipar_linked[1:n_anchor_items, use_these_linked] - o$ipar_anchor[use_these_anchor]
  expect_equal(sum(ipar_diff), 0, tolerance = codetest_tol)
  expect_equal(sum(o$ipar_linked[, use_these_linked]), 364.7369, tolerance = codetest_tol)

  set.seed(1)
  o <- runLinking(d, method = "CP", technical = list(NCYCLES = n_cycles), TOL = tol)
  n_anchor_items <- dim(o$ipar_anchor)[1]
  use_these_linked <- unique(c(
    grep("^a[1-9]$", names(o$ipar_linked), value = TRUE),
    grep("^d[1-9]$", names(o$ipar_linked), value = TRUE)
  ))
  use_these_anchor <- unique(c(
    grep("^a$", names(o$ipar_anchor), value = TRUE),
    grep("^d[1-9]$", names(o$ipar_anchor), value = TRUE)
  ))
  ipar_diff <- o$ipar_linked[1:n_anchor_items, setdiff(use_these_linked, "a2")] - o$ipar_anchor[use_these_anchor]
  expect_equal(sum(ipar_diff), 0, tolerance = codetest_tol)
  expect_equal(sum(o$ipar_linked[, use_these_linked]), -526.6563, tolerance = codetest_tol)

})
