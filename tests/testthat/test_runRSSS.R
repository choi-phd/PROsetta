library(mirt)

test_that("runRSSS", {

  d <- data_asq

  skip_on_cran()
  skip_on_travis()
  skip_on_os("mac")
  skip_on_os("linux")
  skip_on_os("solaris")

  set.seed(1)
  calib <- runLinking(d, method = "FIXEDPAR", technical = list(NCYCLES = 1000))

  score_table <- runRSSS(d, calib)

  expect_equal(sum(score_table$`1`$eap    ** 2), 413.6903, tolerance = 1e-04)
  expect_equal(sum(score_table$`1`$eap_se ** 2), 3.516712, tolerance = 1e-04)

  expect_equal(sum(log(score_table$`1`$escore_1))       , 511.1817, tolerance = 1e-04)
  expect_equal(sum(log(score_table$`1`$escore_2))       , 382.0980, tolerance = 1e-04)
  expect_equal(sum(log(score_table$`1`$escore_combined)), 544.7580, tolerance = 1e-04)

  expect_equal(sum(score_table$`2`$eap    ** 2), 224.7138, tolerance = 1e-04)
  expect_equal(sum(score_table$`2`$eap_se ** 2), 5.638811, tolerance = 1e-04)

  expect_equal(sum(log(score_table$`2`$escore_1))       , 200.5240, tolerance = 1e-04)
  expect_equal(sum(log(score_table$`2`$escore_2))       , 150.7272, tolerance = 1e-04)
  expect_equal(sum(log(score_table$`2`$escore_combined)), 213.4005, tolerance = 1e-04)

  expect_equal(sum(score_table$`combined`$eap    ** 2), 677.9280, tolerance = 1e-04)
  expect_equal(sum(score_table$`combined`$eap_se ** 2), 4.451836, tolerance = 1e-04)

  expect_equal(sum(log(score_table$`combined`$escore_1))       , 708.9261, tolerance = 1e-04)
  expect_equal(sum(log(score_table$`combined`$escore_2))       , 531.1787, tolerance = 1e-04)
  expect_equal(sum(log(score_table$`combined`$escore_combined)), 755.0976, tolerance = 1e-04)

  set.seed(1)
  calib <- runLinking(d, method = "CP", technical = list(NCYCLES = 1000))

  score_table <- runRSSS(d, calib)

  expect_equal(sum(score_table$`1`$eap_dim1    ** 2), 414.3229, tolerance = 1e-04)
  expect_equal(sum(score_table$`1`$eap_dim2    ** 2), 371.6067, tolerance = 1e-04)
  expect_equal(sum(score_table$`1`$eap_se_dim1 ** 2), 3.604093, tolerance = 1e-04)
  expect_equal(sum(score_table$`1`$eap_se_dim2 ** 2), 18.52500, tolerance = 1e-04)

  expect_equal(sum(score_table$`2`$eap_dim1    ** 2), 180.4345, tolerance = 1e-04)
  expect_equal(sum(score_table$`2`$eap_dim2    ** 2), 220.6753, tolerance = 1e-04)
  expect_equal(sum(score_table$`2`$eap_se_dim1 ** 2), 10.73108, tolerance = 1e-04)
  expect_equal(sum(score_table$`2`$eap_se_dim2 ** 2), 4.724330, tolerance = 1e-04)

  expect_equal(sum(score_table$`combined`$eap_dim1    ** 2), 670.6838, tolerance = 1e-04)
  expect_equal(sum(score_table$`combined`$eap_dim2    ** 2), 653.2407, tolerance = 1e-04)
  expect_equal(sum(score_table$`combined`$eap_se_dim1 ** 2), 5.859767, tolerance = 1e-04)
  expect_equal(sum(score_table$`combined`$eap_se_dim2 ** 2), 15.94437, tolerance = 1e-04)

})
