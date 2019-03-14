## Written by Seung W. Choi (schoi@austin.utexas.edu)
#' @import psych
#' @import mirt
#' @import plink
#' @importFrom methods new
#' @importFrom utils read.csv
NULL

#' An S4 class to represent configurations for reading datasets and writing outputs.
#'
#' @slot studyName Character. A user-specified name of the study.
#' @slot inputDirectory Character. The path of the input directory for reading dataset.
#' @slot outputDirectory Character. The path of the output directory for writing outputs. A new folder is created at the path if it does not exist.
#' @slot itemID Character. The name of the variable representing item IDs in \code{anchorFile} and \code{itemmapFile}.
#' @slot personID Character. The name of the variable representing person IDs in \code{responseFile}.
#' @slot scaleID Character. The name of the variable representing scale IDs in \code{itemmapFile}.
#' @slot responseFile Character. The filename of a dataset containing IDs and the responses of the items.
#' @slot itemmapFile Character. The filename for an item map.
#' @slot anchorFile Character. The filename of a dataset containing the parameters of the items.
#' @slot linkingMethod Character. The linking method to use. Accepts "\code{MM}", "\code{MS}", "\code{HB}", "\code{SL}", "\code{FIXEDPAR}", or "\code{NONE}".
#'
#' @export
#' @examples
#' \dontrun{
#' new.Config = new("PROsetta.Config",
#'                  inputDirectory = getwd(),
#'                  anchorFile = "data-raw\\anchor_axmasq.csv",
#'                  responseFile = "data-raw\\dat_axmasq_v2.csv",
#'                  itemmapFile = "data-raw\\imap_axmasq.csv",
#'                  linkingMethod = "FIXEDPAR")
#' new.Config@itemID = "item_id"
#' new.Config@personID = "prosettaid"
#' new.Config@scaleID = "instrument"
#' }

setClass("PROsetta.Config",
         slots = c(studyName = "character",
                   inputDirectory = "character",
                   outputDirectory = "character",
                   itemID = "character",
                   personID = "character",
                   scaleID = "character",
                   responseFile = "character",
                   itemmapFile = "character",
                   anchorFile = "character",
                   linkingMethod = "character"),
         prototype = list(studyName = "PROsetta",
                          inputDirectory = getwd(),
                          outputDirectory = getwd(),
                          itemID = "itemID",
                          personID = "personID",
                          scaleID = "scaleID",
                          responseFile = "",
                          itemmapFile = "",
                          anchorFile = "",
                          linkingMethod = "NONE"),
         validity = function(object) {
           if (!dir.exists(object@inputDirectory)) {
             stop(paste("invalid inputDirectory :", object@inputDirectory))
           }
           if (object@outputDirectory != "") {
             if (!dir.exists(object@outputDirectory)) dir.create(object@outputDirectory, recursive = TRUE)
           }
           if (object@responseFile != "") {
             if (!file.exists(object@responseFile)) stop(paste("responseFile does not exist :", object@responseFile))
           }
           if (object@itemmapFile != "") {
             if (!file.exists(object@itemmapFile)) stop(paste("itemmapFile does not exist :", object@itemmapFile))
           }
           if (object@linkingMethod != "NONE" && object@anchorFile != "") {
             if (!file.exists(object@anchorFile)) stop(paste("anchorFile does not exist :", object@anchorFile))
           }
           if (!object@linkingMethod %in% c("MM", "MS", "HB", "SL", "FIXEDPAR", "NONE")){
             stop("invalid option for linkingMethod")
           }
           return (TRUE)
         }
)

#' Creates a config object for PROsetta
#'
#' Creates a config object for PROsetta
#'
#' @param studyName Character. A user-specified name of the study.
#' @param inputDirectory Character. The path of the input directory for reading dataset. Defaults to \code{getwd()}.
#' @param outputDirectory Character. The path of the output directory for writing outputs. Defaults to \code{getwd()}. A new folder is created at the path if it does not exist.
#' @param itemID Character. The name of the variable representing item IDs in \code{anchorFile} and \code{itemmapFile}.
#' @param personID Character. The name of the variable representing person IDs in \code{responseFile}.
#' @param scaleID Character. The name of the variable representing scale IDs in \code{itemmapFile}.
#' @param responseFile Character. The filename of a dataset containing IDs and the responses of the items.
#' @param itemmapFile Character. The filename for an item map.
#' @param anchorFile Character. The filename of a dataset containing the parameters of the items.
#' @param linkingMethod Character. The linking method to use. Accepts "\code{MM}", "\code{MS}", "\code{HB}", "\code{SL}", "\code{FIXEDPAR}", or "\code{NONE}".
#'
#' @return A \code{\linkS4class{PROsetta.Config}} object.
#'
#' @export

new.config = function(studyName = "Study",
                      inputDirectory = getwd(), outputDirectory = getwd(),
                      itemID, personID, scaleID,
                      responseFile, itemmapFile, anchorFile,
                      linkingMethod = "FIXEDPAR") {
  new.Config = new("PROsetta.Config",
                   inputDirectory = inputDirectory,
                   outputDirectory = outputDirectory,
                   responseFile = responseFile,
                   itemmapFile = itemmapFile,
                   anchorFile = anchorFile,
                   linkingMethod = toupper(linkingMethod)
                   )
  new.Config@itemID = itemID
  new.Config@personID = personID
  new.Config@scaleID = scaleID
  return(new.Config)
}

#' An S4 class to represent \code{PROsetta} datasets.
#'
#' @slot response A list containing IDs and the responses of the items.
#' @slot itemmap A list containing an item map.
#' @slot anchor A list containing the parameters of the items.
#' @export

setClass("PROsetta.Data",
         slots = c(response = "list",
                   itemmap = "list",
                   anchor = "list"),
         prototype = list(response = NULL,
                          itemmap = NULL,
                          anchor = NULL),
         validity = function(object){
           return (TRUE)
         }
)

#' Load data from supplied config
#'
#' Loads data from the supplied configuration.
#'
#' @param Config A PROsetta.Config object. See \linkS4class{PROsetta.Config}.
#'
#' @return An S4-class object containing the loaded data.
#'
#' @export

LoadData = function(Config) {
  if (class(Config) != "PROsetta.Config") stop("Config must be a class of PROsetta.Config")
  Data = new("PROsetta.Data")
  if (file.exists(Config@responseFile)) {
    Data@response = read.csv(Config@responseFile, as.is = TRUE)
    if (!(Config@personID %in% names(Data@response))) {
      warning(sprintf("%s is not included in responseFile", Config@personID))
    }
  }
  if (file.exists(Config@itemmapFile)) {
    Data@itemmap = read.csv(Config@itemmapFile, as.is = TRUE)
    if (!(Config@itemID %in% names(Data@itemmap))) {
      warning(sprintf("%s is not included in itemmapFile", Config@itemID))
    }
  }
  if (file.exists(Config@anchorFile)) {
    Data@anchor = read.csv(Config@anchorFile, as.is = TRUE)
    if (!(Config@itemID %in% names(Data@anchor))) {
      warning(sprintf("%s is not included in anchorFile", Config@itemID))
    }
  }
  if (!is.null(Data@itemmap) && !is.null(Data@anchor)) {
    if (Config@itemID %in% names(Data@itemmap) && Config@itemID %in% names(Data@anchor)) {
      if (!all(Data@anchor[[Config@itemID]] %in% Data@itemmap[[Config@itemID]])) {
        stop(sprintf("%s in anchorFile contains items that are not in itemmapFile", Config@itemID))
      }
    }
  }
  if (!is.null(Data@itemmap) && !is.null(Data@response)) {
    if (!all(Data@itemmap[[Config@itemID]] %in% names(Data@response))) {
      stop(sprintf("%s in itemmapFile contains items that are not in responseFile", Config@itemID))
    }
  }
  return(Data)
}


#' Obtain a frequency table
#'
#' Obtains a frequency table from the supplied configuration and the dataset.
#'
#' @param Config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param Data A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#'
#' @return A \code{data.frame} containing the frequency table of the dataset.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' inputData = new("PROsetta.Data")
#' inputData@anchor = anchor_asq
#' inputData@response = response_asq
#' inputData@itemmap = itemmap_asq
#' freqTable = RunFrequency(new.Config, inputData)
#' }

RunFrequency = function(Config, Data) {
  if (class(Config) != "PROsetta.Config") stop("Config must be a class of PROsetta.Config")
  if (class(Data) != "PROsetta.Data") stop("Data must be a class of PROsetta.Data")
  Freq = as.data.frame(t(apply(Data@response[Data@itemmap[[Config@itemID]]], 2, table)))
  return(Freq)
}

#' Obtain a descriptive statistics table
#'
#' Obtains a table with descriptive statistics for each variable, from the supplied configuration and the dataset.
#'
#' @param Config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param Data A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#'
#' @return A \code{data.frame} containing the descriptive statistics (mean, standard deviation, median, ...) of the variables in the dataset. These are calculated with \code{\link[psych]{describe}} in \href{https://CRAN.R-project.org/package=psych}{\code{psych}} package.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' inputData = LoadData(new.Config)
#' descTable = RunDescriptive(new.Config, inputData)
#' }

RunDescriptive = function(Config, Data) {
  if (class(Config) != "PROsetta.Config") stop("Config must be a class of PROsetta.Config")
  if (class(Data) != "PROsetta.Data") stop("Data must be a class of PROsetta.Data")
  Descriptive = psych::describe(Data@response[Data@itemmap[[Config@itemID]]])[-1]
  return(Descriptive)
}

#' Run a CTT-based reliability analysis
#'
#' Performs a Classial Test Theory (CTT) reliability analysis.
#'
#' @param Config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param Data A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#' @param omega If TRUE, also obtains McDonald's omega using \code{\link[psych]{omega}} in \href{https://CRAN.R-project.org/package=psych}{\code{psych}} package.
#' @param ... Additional arguments to pass onto \code{\link[psych]{omega}}.
#'
#' @return The results of reliability analysis.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' inputData = LoadData(new.Config)
#' classicalTable = RunClassical(new.Config, inputData)
#' classicalTable2 = RunClassical(new.Config, inputData, omega = TRUE)  # also obtains omega
#' }

RunClassical = function(Config, Data, omega = FALSE, ...) {
  if (class(Config) != "PROsetta.Config") stop("Config must be a class of PROsetta.Config")
  if (class(Data) != "PROsetta.Data") stop("Data must be a class of PROsetta.Data")
  CIA = psych::alpha(Data@response[Data@itemmap[[Config@itemID]]])
  if (omega) {
    CIA[["Omega"]] = psych::omega(Data@response[Data@itemmap[[Config@itemID]]], ...)
  }
  return(CIA)
}


#' Run a confirmatory factor analysis
#'
#' Performs a one-factor confirmatory factor analysis (CFA) to test unidimensionality.
#'
#' @param Config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param Data A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#' @param estimator The estimator to be used. Passed onto \code{\link[lavaan]{cfa}} in \href{https://CRAN.R-project.org/package=lavaan}{\code{lavaan}} package.
#' @param std.lv If \code{TRUE}, the metric of the latent variable is determined by fixing their (residual) variances to 1.0. If \code{FALSE}, the metric of each latent variable is determined by fixing the factor loading of the first indicator to 1.0. Passed onto \code{\link[lavaan]{cfa}}.
#' @param ... Additional arguments to pass onto \code{\link[lavaan]{cfa}}.
#'
#' @return A list containing the CFA results. The models are as follows:
#' \item{all}{A one-factor model where all items in the \code{itemmap} slot of \code{Data} are loaded onto the factor.}
#' \item{anchor}{A one-factor model where the items in the \code{anchor} slot of \code{Data} are loaded onto the factor.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' inputData = LoadData(new.Config)
#' outCFA = RunCFA(new.Config, inputData)
#' summary(outCFA$all, fit.measures = TRUE, standardized = TRUE)
#' summary(outCFA$anchor, fit.measures = TRUE, standardized = TRUE)
#' }

RunCFA = function(Config, Data, estimator = "WLSMV", std.lv = TRUE, ...) {
  if (class(Config) != "PROsetta.Config") stop("Config must be a class of PROsetta.Config")
  if (class(Data) != "PROsetta.Data") stop("Data must be a class of PROsetta.Data")
  all.items = Data@itemmap[[Config@itemID]]
  model.all = paste("Factor =~", paste0(all.items, collapse = " + "))
  model.all.fit = lavaan::cfa(model.all, Data@response, estimator = estimator, ordered = all.items, std.lv = std.lv, ...)
  out = list(all = model.all.fit)
  if (!is.null(Data@anchor)) {
    anchor.items = Data@anchor[[Config@itemID]]
    model.anchor = paste("Factor =~", paste0(anchor.items, collapse = " + "))
    model.anchor.fit = lavaan::cfa(model.anchor, Data@response, estimator = estimator, ordered = anchor.items, std.lv = std.lv, ...)
    out$anchor = model.anchor.fit
  }
  return(out)
}

#' Run Calibration
#'
#' Performs item calibration for the response data based on the supplied anchor information.
#'
#' @param Config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param Data A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#' @param ... Additional arguments to pass onto \code{\link[mirt]{mirt}} in \href{https://CRAN.R-project.org/package=mirt}{\code{mirt}} package.
#'
#' @return An object containing item calibration results. This object can be used in \code{\link[mirt:coef-method]{coef}}, \code{\link[mirt]{itemfit}}, \code{\link[mirt]{itemplot}} in \href{https://CRAN.R-project.org/package=mirt}{\code{mirt}} package to extract wanted information.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' inputData = LoadData(new.Config)
#' outCalib = RunCalibration(new.Config, inputData)
#' mirt::coef(outCalib, IRTpars = TRUE, simplify = TRUE)
#' mirt::itemfit(outCalib, empirical.plot = 1)
#' mirt::itemplot(outCalib, item = 1, type = "info")
#' mirt::itemfit(outCalib, "S_X2", na.rm = TRUE)
#'
#' new.Config@linkingMethod = "SL"
#' outCalib.Free = RunCalibration(new.Config, inputData)
#' outCalib.Free = RunCalibration(new.Config, inputData, technical = list(NCYCLES = 1000))
#' mirt::coef(outCalib.Free, IRTpars = TRUE, simplify = TRUE)
#' }
RunCalibration = function(Config, Data, ...) {
  if (class(Config) != "PROsetta.Config") stop("Config must be a class of PROsetta.Config")
  if (class(Data) != "PROsetta.Data") stop("Data must be a class of PROsetta.Data")
  if (Config@linkingMethod == "FIXEDPAR") {
    parLayout = mirt::mirt(Data@response[Data@itemmap[[Config@itemID]]], 1, itemtype = "graded", pars = "values")
    fixed = which(parLayout$item %in% Data@anchor[[Config@itemID]])
    parLayout[fixed, "est"] = FALSE
    parLayout[which(parLayout$class == "GroupPars"), "est"] = TRUE
    for (i in fixed) {
      item = which(Data@anchor[[Config@itemID]] == parLayout$item[i])
      if (substr(parLayout$name[i], 1, 1) == "a") {
        parLayout[i, "value"] = Data@anchor[item, "a"]
      } else {
        k = as.numeric(gsub("[[:alpha:]]", "", parLayout$name[i]))
        parLayout[i, "value"] = -Data@anchor[item, "a"] * Data@anchor[item, paste0("cb", k)]
      }
    }
    Calibration = mirt::mirt(Data@response[Data@itemmap[[Config@itemID]]], 1, itemtype = "graded", pars = parLayout, ...)
  } else {
    Calibration = mirt::mirt(Data@response[Data@itemmap[[Config@itemID]]], 1, itemtype = "graded", ...)
  }
  #ipar = mirt::coef(Calibration, IRTpars = TRUE, simplify = TRUE)$items
  return(Calibration)
}

#' Run Scale Linking
#'
#' Performs scale linking and obtains a set of transformation coefficients.
#'
#' @param Config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param Data A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#' @param ... Additional arguments to pass onto \code{\link[mirt]{mirt}} in \href{https://CRAN.R-project.org/package=mirt}{\code{mirt}} package.
#'
#' @return A list containing the scale linking results. These are obtained with \code{\link[plink]{plink-methods}} in \href{https://CRAN.R-project.org/package=plink}{\code{plink}} package.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' inputData = LoadData(new.Config)
#' outEquate = RunLinking(new.Config, inputData, technical = list(NCYCLES = 1000))
#' outEquate$link@constants$SL
#' }

RunLinking = function(Config, Data, ...) {
  if (class(Config) != "PROsetta.Config") stop("Config must be a class of PROsetta.Config")
  if (class(Data) != "PROsetta.Data") stop("Data must be a class of PROsetta.Data")
  if (is.null(Data@anchor)) stop("anchor cannot be NULL")
  if (!Config@linkingMethod %in% c("MM","MS","HB","SL","LS")) stop("Config@linkingMethod must be one of the following: \"MM\", \"MS\", \"HB\", \"SL\", \"LS\".")
  Calibration = RunCalibration(Config, Data, ...)
  ipar = mirt::coef(Calibration, IRTpars = TRUE, simplify = TRUE)$items
  ni.all = nrow(ipar)
  ni.anchor = nrow(Data@anchor)
  maxCat = max(Data@anchor$NCAT)
  ID.new = data.frame(New = 1:ni.all, ID = Data@itemmap[[Config@itemID]])
  ID.old = data.frame(Old = 1:ni.anchor, ID = Data@anchor[[Config@itemID]])
  common = merge(ID.new, ID.old, by = "ID", sort = FALSE)[c("New", "Old")]
  pars = vector("list", 2)
  pars[[1]] = ipar
  pars[[2]] = Data@anchor[c("a", paste0("cb", 1:(maxCat - 1)))]
  pm.all = as.poly.mod(ni.all, "grm", 1:ni.all)
  pm.anchor = as.poly.mod(ni.anchor, "grm", 1:ni.anchor)
  ncat = list(Data@itemmap$NCAT, Data@anchor$NCAT)
  out = plink::plink(as.irt.pars(pars, common, cat = ncat, list(pm.all, pm.anchor), grp.names=c("From","To")), rescale = Config@linkingMethod, base.grp = 2)
  return(out)
}

#' Run Test Equating
#'
#' Performs equipercentile test equating between two scales.
#'
#' @param Config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param Data A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#' @param scaleTo Numeric. The index of the target scale to equate to. This and \code{scaleFrom} below both reference to the information stored in the \code{itemmap} slot of \code{Data} argument. The \code{scaleID} slot of \code{Config} argument needs to be specified as the name of the varible containing the scale IDs in the \code{itemmap} slot.
#' @param scaleFrom Numeric. The index of the scale in need of test equating.
#' @param type The type of equating to be passed onto \code{\link[equate]{equate}} in \href{https://CRAN.R-project.org/package=equate}{\code{equate}} package.
#' @param smooth The type of smoothing method to be passed onto \code{\link[equate]{presmoothing}} in \href{https://CRAN.R-project.org/package=equate}{\code{equate}} package.
#' @param degrees The degrees of smoothing to be passed onto \code{\link[equate]{presmoothing}}.
#' @param boot Logical. Performs bootstrapping if \code{TRUE}.
#' @param reps Numeric. The number of replications in bootsrapping.
#' @param ... Other arguments to pass onto \code{\link[equate]{equate}}..
#'
#' @return An \code{equate} object containing the test equating result. The printed summary statistics indicate the distributional properties of the two supplied scales and the equated scale. The rows titled \code{x} and \code{y} correspond to the scales specified in \code{scaleFrom} and \code{scaleTo} respectively. The row titled \code{yx} corresponds to the \code{scaleFrom} scale transformed to \code{scaleTo}. See \code{\link[equate]{equate}} for details.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' inputData = LoadData(new.Config)
#' outEquateEquipercentile = RunEquateObserved(new.Config, inputData, scaleTo = 1, scaleFrom = 2,
#'             type = "equipercentile", smooth = "loglinear")
#' }

RunEquateObserved = function(Config, Data, scaleTo = 1, scaleFrom = 2, type = "equipercentile", smooth = "loglinear", degrees = list(3, 1), boot = TRUE, reps = 100, ...) {
  if (class(Config) != "PROsetta.Config") stop("Config must be a class of PROsetta.Config")
  if (class(Data) != "PROsetta.Data") stop("Data must be a class of PROsetta.Data")
  scaleID    = Data@itemmap[[Config@scaleID]]
  scaleCode  = unique(scaleID)
  itemsTo    = which(scaleID %in% scaleTo)
  itemsFrom  = which(scaleID %in% scaleFrom)
  scoresTo   = rowSums(Data@response[Data@itemmap[[Config@itemID]][itemsTo]])
  scoresFrom = rowSums(Data@response[Data@itemmap[[Config@itemID]][itemsFrom]])
  freqTo     = equate::freqtab(Data@response[Data@itemmap[[Config@itemID]]], items = itemsTo)
  freqFrom   = equate::freqtab(Data@response[Data@itemmap[[Config@itemID]]], items = itemsFrom)
  scoreStat  = rbind(From = summary(freqFrom), To = summary(freqTo))
  #plot(x = freqTo, lwd = 2, xlab = "Score", ylab = "Count")
  #plot(x = sfreqTo, lwd = 2, xlab = "Score", ylab = "Count")
  if (smooth != "none") {
    freqTo   = equate::presmoothing(freqTo, smoothmethod = smooth, degrees = degrees)
    freqFrom = equate::presmoothing(freqFrom, smoothmethod = smooth, degrees = degrees)
  }
  out = equate::equate(freqFrom, freqTo, type = type, boot = boot, reps = reps, ...)
  return(out)
}
