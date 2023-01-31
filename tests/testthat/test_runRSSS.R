library(mirt)

test_that("runRSSS() works", {

  d <- data_asq

  skip_on_cran()
  skip_on_ci()

  n_cycles <- 1e5
  tol <- 1e-6
  test_tol <- 1e-4

  set.seed(1)
  o <- runLinking(d, method = "FIXEDPAR", technical = list(NCYCLES = n_cycles), TOL = tol)

  score_table <- runRSSS(d, o)

  expect_equal(sum(score_table$`1`$eap    ** 2), 413.6903, tolerance = test_tol)
  expect_equal(sum(score_table$`1`$eap_se ** 2), 3.516712, tolerance = test_tol)

  expect_equal(sum(log(score_table$`1`$escore_1))       , 511.1817, tolerance = test_tol)
  expect_equal(sum(log(score_table$`1`$escore_2))       , 382.0981, tolerance = test_tol)
  expect_equal(sum(log(score_table$`1`$escore_combined)), 544.7580, tolerance = test_tol)

  expect_equal(sum(score_table$`2`$eap    ** 2), 224.7130, tolerance = test_tol)
  expect_equal(sum(score_table$`2`$eap_se ** 2), 5.638760, tolerance = test_tol)

  expect_equal(sum(log(score_table$`2`$escore_1))       , 200.5239, tolerance = test_tol)
  expect_equal(sum(log(score_table$`2`$escore_2))       , 150.7272, tolerance = test_tol)
  expect_equal(sum(log(score_table$`2`$escore_combined)), 213.4005, tolerance = test_tol)

  expect_equal(sum(score_table$`combined`$eap    ** 2), 677.9268, tolerance = test_tol)
  expect_equal(sum(score_table$`combined`$eap_se ** 2), 4.451822, tolerance = test_tol)

  expect_equal(sum(log(score_table$`combined`$escore_1))       , 708.9261, tolerance = test_tol)
  expect_equal(sum(log(score_table$`combined`$escore_2))       , 531.1788, tolerance = test_tol)
  expect_equal(sum(log(score_table$`combined`$escore_combined)), 755.0976, tolerance = test_tol)

  set.seed(1)
  o <- runLinking(d, method = "CP", technical = list(NCYCLES = n_cycles), TOL = tol)

  score_table <- runRSSS(d, o)

  expect_equal(sum(score_table$`1`$eap_dim1    ** 2), 414.3302, tolerance = test_tol)
  expect_equal(sum(score_table$`1`$eap_dim2    ** 2), 371.4656, tolerance = test_tol)
  expect_equal(sum(score_table$`1`$eap_se_dim1 ** 2), 3.604095, tolerance = test_tol)
  expect_equal(sum(score_table$`1`$eap_se_dim2 ** 2), 18.52239, tolerance = test_tol)

  expect_equal(sum(score_table$`2`$eap_dim1    ** 2), 180.4573, tolerance = test_tol)
  expect_equal(sum(score_table$`2`$eap_dim2    ** 2), 220.6060, tolerance = test_tol)
  expect_equal(sum(score_table$`2`$eap_se_dim1 ** 2), 10.73075, tolerance = test_tol)
  expect_equal(sum(score_table$`2`$eap_se_dim2 ** 2), 4.724066, tolerance = test_tol)

  expect_equal(sum(score_table$`combined`$eap_dim1    ** 2), 670.6871, tolerance = test_tol)
  expect_equal(sum(score_table$`combined`$eap_dim2    ** 2), 653.0106, tolerance = test_tol)
  expect_equal(sum(score_table$`combined`$eap_se_dim1 ** 2), 5.859618, tolerance = test_tol)
  expect_equal(sum(score_table$`combined`$eap_se_dim2 ** 2), 15.94244, tolerance = test_tol)

})
