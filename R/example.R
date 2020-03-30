#' @include datasets.R
NULL

if (FALSE) {

  cfg <- createConfig(
    response_file = "inst/data-raw/dat_decesd_v2.csv",
    anchor_file = "inst/data-raw/anchor_decesd.csv",
    itemmap_file = "inst/data-raw/imap_decesd.csv"
  )

  input_data <- loadData(cfg)
  freq_table <- runFrequency(cfg)
  desc_table <- runDescriptive(cfg)
  classical_table  <- runClassical(cfg)
  classical_table2 <- runClassical(cfg, omega = TRUE)

  out_CFA <- runCFA(cfg)

  summary(out_CFA$combined, fit.measures = TRUE, standardized = TRUE)

  out_calib <- runCalibration(cfg)

  mirt::coef(out_calib, IRTpars = TRUE, simplify = TRUE)
  mirt::itemfit(out_calib, empirical.plot = 1)
  mirt::itemplot(out_calib, item = 1, type = "info")
  mirt::itemfit(out_calib, "S_X2", na.rm = TRUE)

  cfg@linking_method <- "SL"
  out_calib_free <- runCalibration(cfg)
  out_calib_free <- runCalibration(cfg, technical = list(NCYCLES = 1000))

  mirt::coef(out_calib_free, IRTpars = TRUE, simplify = TRUE)

  out_equate <- runLinking(cfg, technical = list(NCYCLES = 1000))
  out_equate$link@constants$SL

  out_equate_equiper <- runEquateObserved(cfg, scaleTo = 1, scaleFrom = 2, type = "equipercentile", smooth = "loglinear")

  inputData
}
