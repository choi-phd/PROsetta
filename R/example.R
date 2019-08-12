
if (FALSE) {
  new.Config <- new.config(
    inputDirectory = "/Users/sl47276/Downloads/",
    anchorFile = "RADAR_promisanchor.csv",
    responseFile = "RADAR_promis_asr.csv",
    itemmapFile = "promis_asr_imap.csv",
    linkingMethod = "FIXEDPAR",
    guessID = T
  )

  inputData <- LoadData(new.Config)
  freqTable <- RunFrequency(new.Config, inputData)
  descTable <- RunDescriptive(new.Config, inputData)
  classicalTable <- capture.output(RunClassical(new.Config, inputData))
  cat(classicalTable, sep = "\n")
  names(classicalTable)

  classicalTable$item.stat

  classicalTable2 <- RunClassical(new.Config, inputData, omega = TRUE)
  outCFA <- RunCFA(new.Config, inputData)
  summary(outCFA$all, fit.measures = TRUE, standardized = TRUE)
  summary(outCFA$anchor, fit.measures = TRUE, standardized = TRUE)

  outCalib <- RunCalibration(new.Config, inputData)

  mirt::coef(outCalib, IRTpars = TRUE, simplify = TRUE)
  mirt::itemfit(outCalib, empirical.plot = 1)
  mirt::itemplot(outCalib, item = 1, type = "info")
  mirt::itemfit(outCalib, "S_X2", na.rm = TRUE)

  new.Config@linkingMethod <- "SL"
  outCalib.Free <- RunCalibration(new.Config, inputData)
  outCalib.Free <- RunCalibration(new.Config, inputData, technical = list(NCYCLES = 1000))
  mirt::coef(outCalib.Free, IRTpars = TRUE, simplify = TRUE)

  outEquate <- RunLinking(new.Config, inputData, technical = list(NCYCLES = 1000))
  outEquate$link@constants$SL

  outEquateEquipercentile <- RunEquateObserved(new.Config, inputData, scaleTo = 1, scaleFrom = 2, type = "equipercentile", smooth = "loglinear")

  inputData
}
