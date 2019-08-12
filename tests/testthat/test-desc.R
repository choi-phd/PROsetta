test_that("runRSSS", {
  solution    <- runCalibration(cfg_asq)
  score_table <- runRSSS(cfg_asq, calibration = solution)

  cfg_asq2   <- cfg_asq
  cfg_asq2@linking_method <- "SL"

  solution    <- runLinking(cfg_asq2, technical = list(NCYCLES = 1000))
  score_table <- runRSSS(cfg_asq2, calibration = solution)
})
