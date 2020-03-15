#' @include datasets.R
NULL

if (FALSE) {

  cfg <- createConfig(
    response_file = "data-raw/dat_axmasq_v2.csv",
    anchor_file = "data-raw/anchor_axmasq.csv",
    itemmap_file = "data-raw/imap_axmasq.csv"
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
