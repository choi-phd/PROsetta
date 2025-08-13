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
  expect_equal(sum(log(score_table$`1`$escore_2))       , 382.0982, tolerance = test_tol)
  expect_equal(sum(log(score_table$`1`$escore_combined)), 544.7580, tolerance = test_tol)

  expect_equal(sum(score_table$`2`$eap    ** 2), 224.7131, tolerance = test_tol)
  expect_equal(sum(score_table$`2`$eap_se ** 2), 5.638773, tolerance = test_tol)

  expect_equal(sum(log(score_table$`2`$escore_1))       , 200.5239, tolerance = test_tol)
  expect_equal(sum(log(score_table$`2`$escore_2))       , 150.7272, tolerance = test_tol)
  expect_equal(sum(log(score_table$`2`$escore_combined)), 213.4005, tolerance = test_tol)

  expect_equal(sum(score_table$`combined`$eap    ** 2), 677.9269, tolerance = test_tol)
  expect_equal(sum(score_table$`combined`$eap_se ** 2), 4.451827, tolerance = test_tol)

  expect_equal(sum(log(score_table$`combined`$escore_1))       , 708.9261, tolerance = test_tol)
  expect_equal(sum(log(score_table$`combined`$escore_2))       , 531.1789, tolerance = test_tol)
  expect_equal(sum(log(score_table$`combined`$escore_combined)), 755.0976, tolerance = test_tol)

  set.seed(1)
  o <- runLinking(d, method = "CP", technical = list(NCYCLES = n_cycles), TOL = tol)

  score_table <- runRSSS(d, o)

  expect_equal(sum(score_table$`1`$eap_dim1    ** 2), 414.3355, tolerance = test_tol)
  expect_equal(sum(score_table$`1`$eap_dim2    ** 2), 371.4398, tolerance = test_tol)
  expect_equal(sum(score_table$`1`$eap_se_dim1 ** 2), 3.604257, tolerance = test_tol)
  expect_equal(sum(score_table$`1`$eap_se_dim2 ** 2), 18.51872, tolerance = test_tol)

  expect_equal(sum(score_table$`2`$eap_dim1    ** 2), 180.4772, tolerance = test_tol)
  expect_equal(sum(score_table$`2`$eap_dim2    ** 2), 220.5954, tolerance = test_tol)
  expect_equal(sum(score_table$`2`$eap_se_dim1 ** 2), 10.73102, tolerance = test_tol)
  expect_equal(sum(score_table$`2`$eap_se_dim2 ** 2), 4.723732, tolerance = test_tol)

  expect_equal(sum(score_table$`combined`$eap_dim1    ** 2), 670.6993, tolerance = test_tol)
  expect_equal(sum(score_table$`combined`$eap_dim2    ** 2), 652.9615, tolerance = test_tol)
  expect_equal(sum(score_table$`combined`$eap_se_dim1 ** 2), 5.859760, tolerance = test_tol)
  expect_equal(sum(score_table$`combined`$eap_se_dim2 ** 2), 15.93959, tolerance = test_tol)

})
