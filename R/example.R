#' @include datasets.R
NULL

if (FALSE) {

  fp <- system.file("data-raw", package = "PROsetta")
  d <- loadData(
    response  = "dat_DeCESD_v2.csv",
    itemmap   = "imap_DeCESD.csv",
    anchor    = "anchor_DeCESD.csv",
    input_dir = fp
  )

  freq_table <- runFrequency(d)
  desc_table <- runDescriptive(d)
  classical_table  <- runClassical(d)
  classical_table2 <- runClassical(d, omega = TRUE)

  out_CFA <- runCFA(d)

  lavaan::summary(out_CFA$combined, fit.measures = TRUE, standardized = TRUE)

  ## Item parameter linking to anchor data
  out_link <- runLinking(d, technical = list(NCYCLES = 1000))
  out_link <- runLinking(d, method = "FIXEDPAR", technical = list(NCYCLES = 1000))
  out_link$constants
  out_link$method
  out_link$ipar_linked
  out_link$ipar_anchor

  rsss <- runRSSS(d, out_link) # Map raw scores to standardized scores using linked item parameters
  head(rsss$`2`)

  rsss$`1`$theta_score

  ## For item parameter examination, does not perform linking
  out_calib <- runCalibration(d)
  out_calib <- runCalibration(d, technical = list(NCYCLES = 1000))
  out <- mirt::coef(out_calib, IRTpars = TRUE, simplify = TRUE)
  mirt::itemfit(out_calib, empirical.plot = 1)
  mirt::itemplot(out_calib, item = 1, type = "info")
  mirt::itemfit(out_calib, "S_X2", na.rm = TRUE)

  ## Map raw scores from one scale to another scale
  out_eqp <- runEquateObserved(d, scaleTo = 1, scaleFrom = 2, type = "equipercentile", smooth = "loglinear")
  out_eqp$concordance

}
