
if(FALSE){

new.Config = new("PROsetta.Config", inputDirectory = getwd(),
                 anchorFile = "data-raw\\anchor_AxMASQ.csv",
                 responseFile = "data-raw\\dat_axmasq_v2.csv",
                 itemmapFile = "data-raw\\imap_AxMASQ.csv",
                 linkingMethod = "FIXEDPAR")
new.Config@itemID = "item_id"
new.Config@personID = "prosettaid"
new.Config@scaleID = "instrument"

inputData = LoadData(new.Config)
freqTable = RunFrequency(new.Config, inputData)
descTable = RunDescriptive(new.Config, inputData)
classicalTable = RunClassical(new.Config, inputData)
classicalTable2 = RunClassical(new.Config, inputData, omega = TRUE)
outCFA = RunCFA(new.Config, inputData)
summary(outCFA$all, fit.measures = TRUE, standardized = TRUE)
summary(outCFA$anchor, fit.measures = TRUE, standardized = TRUE)

outCalib = RunCalibration(new.Config, inputData)

mirt::coef(outCalib, IRTpars = TRUE, simplify = TRUE)
mirt::itemfit(outCalib, empirical.plot = 1)
mirt::itemplot(outCalib, item = 1, type = "info")
mirt::itemfit(outCalib, "S_X2", na.rm = TRUE)

new.Config@linkingMethod = "SL"
outCalib.Free = RunCalibration(new.Config, inputData)
outCalib.Free = RunCalibration(new.Config, inputData, technical = list(NCYCLES = 1000))
mirt::coef(outCalib.Free, IRTpars = TRUE, simplify = TRUE)

outEquate = RunLinking(new.Config, inputData, technical = list(NCYCLES = 1000))
outEquate$link@constants$SL

outEquateEquipercentile = RunEquateObserved(new.Config, inputData, scaleTo = 1, scaleFrom = 2, type = "equipercentile", smooth = "loglinear")

inputData
}
