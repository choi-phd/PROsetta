test_that("getAnchorDimension", {

  d <- data_asq

  skip_on_cran()

  anchor_dim <- PROsetta:::getAnchorDimension(d)
  expect_equal(anchor_dim, 1)

  d@itemmap$scale_id <- 3 - d@itemmap$scale_id
  anchor_dim <- PROsetta:::getAnchorDimension(d)
  expect_equal(anchor_dim, 2)

})
