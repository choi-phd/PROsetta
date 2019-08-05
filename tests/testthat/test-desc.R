test_that("RunRSSS", {

  solution   <- RunCalibration(cfg_asq)
  scoreTable <- RunRSSS(solution)

  cfg_asq2   <- cfg_asq
  cfg_asq2@linkingMethod <- "SL"

  solution   <- RunLinking(cfg_asq2, technical = list(NCYCLES = 1000))
  scoreTable <- RunRSSS(cfg_asq2, calibration = solution)

})
