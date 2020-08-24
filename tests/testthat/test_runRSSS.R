library(mirt)

test_that("runRSSS", {

  d <- data_asq

  skip_on_cran()
  skip_on_travis()

  set.seed(1)
  calib <- runLinking(d, method = "FIXEDPAR", technical = list(NCYCLES = 1000))

  score_table <- runRSSS(d, calib)

  expect_equal(sum(score_table$`1`$eap    ** 2), 413.327 , tolerance = 1e-04)
  expect_equal(sum(score_table$`1`$eap_se ** 2), 3.507493, tolerance = 1e-04)

  expect_equal(sum(log(score_table$`1`$escore_1))       , 511.1791, tolerance = 1e-04)
  expect_equal(sum(log(score_table$`1`$escore_2))       , 382.0931, tolerance = 1e-04)
  expect_equal(sum(log(score_table$`1`$escore_combined)), 544.7549, tolerance = 1e-04)

  expect_equal(sum(score_table$`2`$eap    ** 2), 224.0535, tolerance = 1e-04)
  expect_equal(sum(score_table$`2`$eap_se ** 2), 5.618085, tolerance = 1e-04)

  expect_equal(sum(log(score_table$`2`$escore_1))       , 200.5182, tolerance = 1e-04)
  expect_equal(sum(log(score_table$`2`$escore_2))       , 150.7171, tolerance = 1e-04)
  expect_equal(sum(log(score_table$`2`$escore_combined)), 213.3937, tolerance = 1e-04)

  expect_equal(sum(score_table$`combined`$eap    ** 2), 676.6168, tolerance = 1e-04)
  expect_equal(sum(score_table$`combined`$eap_se ** 2), 4.435159, tolerance = 1e-04)

  expect_equal(sum(log(score_table$`combined`$escore_1))       , 708.9185, tolerance = 1e-04)
  expect_equal(sum(log(score_table$`combined`$escore_2))       , 531.1623, tolerance = 1e-04)
  expect_equal(sum(log(score_table$`combined`$escore_combined)), 755.0878, tolerance = 1e-04)

  set.seed(1)
  calib <- runLinking(d, method = "CP", technical = list(NCYCLES = 1000))

  score_table <- runRSSS(d, calib, inc = 0.2)

  expect_equal(sum(score_table$`1`$eap_dim1    ** 2), 415.6739, tolerance = 1e-04)
  expect_equal(sum(score_table$`1`$eap_dim2    ** 2), 373.2189, tolerance = 1e-04)
  expect_equal(sum(score_table$`1`$eap_se_dim1 ** 2), 3.636858, tolerance = 1e-04)
  expect_equal(sum(score_table$`1`$eap_se_dim2 ** 2), 18.59797, tolerance = 1e-04)

  expect_equal(sum(score_table$`2`$eap_dim1    ** 2), 182.6433, tolerance = 1e-04)
  expect_equal(sum(score_table$`2`$eap_dim2    ** 2), 222.8172, tolerance = 1e-04)
  expect_equal(sum(score_table$`2`$eap_se_dim1 ** 2), 10.82769, tolerance = 1e-04)
  expect_equal(sum(score_table$`2`$eap_se_dim2 ** 2), 4.788995, tolerance = 1e-04)

  expect_equal(sum(score_table$`combined`$eap_dim1    ** 2), 675.0856, tolerance = 1e-04)
  expect_equal(sum(score_table$`combined`$eap_dim2    ** 2), 658.0294, tolerance = 1e-04)
  expect_equal(sum(score_table$`combined`$eap_se_dim1 ** 2), 5.927989, tolerance = 1e-04)
  expect_equal(sum(score_table$`combined`$eap_se_dim2 ** 2), 16.02533, tolerance = 1e-04)

})
