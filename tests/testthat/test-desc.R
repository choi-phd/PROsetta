test_that("runRSSS", {
  f1 <- tempfile()
  f2 <- tempfile()
  f3 <- tempfile()
  write.csv(response_asq, f1, row.names = FALSE)
  write.csv(itemmap_asq, f2, row.names = FALSE)
  write.csv(anchor_asq, f3, row.names = FALSE)

  d <- loadData(response = f1, itemmap = f2, anchor = f3)

  out_cfa     <- runCFA(d)

  solution    <- runLinking(d, method = "SL", technical = list(NCYCLES = 1000))
  score_table <- runRSSS(d, solution)
  solution    <- runLinking(d, method = "FIXEDPAR")
  score_table <- runRSSS(d, solution)

  solution    <- runEquateObserved(d)

  file.remove(f1)
  file.remove(f2)
  file.remove(f3)
})
