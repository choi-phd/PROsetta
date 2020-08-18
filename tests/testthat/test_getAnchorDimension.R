test_that("getAnchorDimension", {

  d <- data_asq

  skip_on_cran()

  anchor_dim <- PROsetta:::getAnchorDimension(d)
  expect_equal(anchor_dim, 1)

  d@itemmap$instrument <- 3 - d@itemmap$instrument
  anchor_dim <- PROsetta:::getAnchorDimension(d)
  expect_equal(anchor_dim, 2)

})
