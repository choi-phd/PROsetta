test_that("loadData() works", {

  d <- loadData(
    response = response_asq,
    itemmap  = itemmap_asq,
    anchor   = anchor_asq
  )

  expect_true(d@item_id   == "item_id")
  expect_true(d@person_id == "prosettaid")
  expect_true(d@scale_id  == "scale_id")

})

test_that("loadData() catches item_model inconsistency between anchor and itemmap", {

  itemmap_malformed <- itemmap_asq
  itemmap_malformed$item_model[1] <- "GPC"

  expect_error({
    d <- loadData(
      response = response_asq,
      itemmap  = itemmap_malformed,
      anchor   = anchor_asq
    )
  }, "has different models"
  )

  anchor_malformed <- anchor_asq
  anchor_malformed$item_model[1] <- "GPC"

  expect_error({
    d <- loadData(
      response = response_asq,
      itemmap  = itemmap_asq,
      anchor   = anchor_malformed
    )
  }, "has different models"
  )

  itemmap_malformed <- itemmap_asq
  itemmap_malformed$item_model[1] <- "GPC"
  anchor_malformed <- anchor_asq
  anchor_malformed$item_model[2] <- "GPC"

  expect_error({
    d <- loadData(
      response = response_asq,
      itemmap  = itemmap_malformed,
      anchor   = anchor_malformed
    )
  }, "has different models"
  )

})
