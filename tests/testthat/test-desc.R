test_that("runRSSS", {
  f1 <- tempfile()
  f2 <- tempfile()
  f3 <- tempfile()
  write.csv(response_asq, f1, row.names = FALSE)
  write.csv(itemmap_asq, f2, row.names = FALSE)
  write.csv(anchor_asq, f3, row.names = FALSE)
  cfg <- createConfig(response_file = f1, itemmap_file = f2, anchor_file = f3)

  solution    <- runCalibration(cfg)
  score_table <- runRSSS(cfg, calibration = solution)

  cfg2 <- cfg
  cfg2@linking_method <- "SL"

  solution    <- runLinking(cfg2, technical = list(NCYCLES = 1000))
  score_table <- runRSSS(cfg2, calibration = solution)

  file.remove(f1)
  file.remove(f2)
  file.remove(f3)
})
