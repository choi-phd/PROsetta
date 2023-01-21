test_that("loadData() works", {

  d <- loadData(
    response = response_asq,
    itemmap  = itemmap_asq,
    anchor   = anchor_asq
  )

  expect_true(d@item_id   == "item_id")
  expect_true(d@person_id == "prosettaid")
  expect_true(d@scale_id  == "instrument")

  # TODO: add more codetests

})
