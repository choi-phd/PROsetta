library(mirt)

test_that("runRSSS", {

  d <- data_asq

  skip_on_cran()

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

})
