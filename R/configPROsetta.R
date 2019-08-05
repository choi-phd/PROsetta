## Written by Seung W. Choi (schoi@austin.utexas.edu)
#' @import psych
#' @import mirt
#' @import plink
#' @importFrom methods new
#' @importFrom utils read.csv
#' @importFrom stats dnorm
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
             p = check.file.path(object@inputDirectory, object@responseFile)
             if (!p$exists) stop(paste("responseFile does not exist :", p$path))
           }
           if (object@itemmapFile != "") {
             p = check.file.path(object@inputDirectory, object@itemmapFile)
             if (!p$exists) stop(paste("itemmapFile does not exist :", p$path))
           }
           if (object@linkingMethod != "NONE" && object@anchorFile != "") {
             p = check.file.path(object@inputDirectory, object@anchorFile)
             if (!p$exists) stop(paste("anchorFile does not exist :", p$path))
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
#' @param guessID Logical. If \code{TRUE}, attempts to guess \code{itemID, personID, scaleID}.
#' @return A \code{\linkS4class{PROsetta.Config}} object.
#'
#' @export

new.config = function(studyName = "Study",
                      inputDirectory = getwd(), outputDirectory = getwd(),
                      itemID = "", personID = "", scaleID = "",
                      responseFile, itemmapFile, anchorFile,
                      linkingMethod = "FIXEDPAR",
                      guessID = F) {
  new.Config = new("PROsetta.Config",
                   inputDirectory = inputDirectory,
                   outputDirectory = outputDirectory,
                   responseFile = responseFile,
                   itemmapFile = itemmapFile,
                   anchorFile = anchorFile,
                   linkingMethod = toupper(linkingMethod))
  if (guessID){
    p = check.file.path(inputDirectory, responseFile)
    if (p$exists) d = read.csv(p$path, as.is = TRUE)
    ids_response = colnames(d)

    p = check.file.path(inputDirectory, itemmapFile)
    if (p$exists) d = read.csv(p$path, as.is = TRUE)
    ids_itemmap = colnames(d)

    n_match = rep(NA, dim(d)[2])
    for (j in 1:dim(d)[2]){ n_match[j] = sum(ids_response %in% d[,j]) }
    idx = which(n_match == max(n_match))[1]
    new.Config@itemID = ids_itemmap[idx]
    cat("ItemID guessed as  : ", new.Config@itemID, "\n")

    idx = which(ids_response %in% d[,idx] == F)[1]
    new.Config@personID = ids_response[idx]
    cat("PersonID guessed as: ", new.Config@personID, "\n")

    n_unique = rep(NA, dim(d)[2])
    for (j in 1:dim(d)[2]){ n_unique[j] = length(unique(d[,j])) }
    idx = which((n_unique != max(n_unique)) & (n_unique != 1))[1]
    new.Config@scaleID = ids_itemmap[idx]
    cat("ScaleID guessed as : ", new.Config@scaleID, "\n")

  } else {
    new.Config@itemID = itemID
    new.Config@personID = personID
    new.Config@scaleID = scaleID
  }
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

#' Check file path
#'
#' Check file path to see if it exists.
#'
#' @param abspath Absolute path of an input directory.
#' @param path Absolute or relative path of a file.
#'
#' @return a list containing normalized file path and whether it exists.
#'
#' @export
check.file.path = function(abspath, path){
  p = path
  if (file.exists(p)) return(list(path = normalizePath(p), exists = T))
  p = file.path(abspath, path)
  if (file.exists(p)) return(list(path = normalizePath(p), exists = T))
  return(list(path = NULL, exists = F))
}


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
  if (class(Config) != "PROsetta.Config") {
    stop("Config must be a class of PROsetta.Config")
  }
  Data = new("PROsetta.Data")

  p = check.file.path(Config@inputDirectory, Config@responseFile)
  if (p$exists){
    Data@response = read.csv(p$path, as.is = TRUE)
    if (!(Config@personID %in% names(Data@response))) {
      warning(sprintf("%s is not included in responseFile", Config@personID))
    }
  }

  p = check.file.path(Config@inputDirectory, Config@itemmapFile)
  if (p$exists) {
    Data@itemmap = read.csv(p$path, as.is = TRUE)
    if (!(Config@itemID %in% names(Data@itemmap))) {
      warning(sprintf("%s is not included in itemmapFile", Config@itemID))
    }
  }
  p = check.file.path(Config@inputDirectory, Config@anchorFile)
  if (p$exists) {
    Data@anchor = read.csv(p$path, as.is = TRUE)
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

#' Check frequency table for unobserved response categories
#'
#' Checks frequency table for unobserved response categories.
#'
#' @param Config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param Data A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#'
#' @return Logical. \code{TRUE} if all categories are present. \code{FALSE} otherwise.
#'
#' @export
CheckFrequency = function(Config, Data = NULL){
  if (class(Config) != "PROsetta.Config") {
    stop("Config must be a class of PROsetta.Config")
  }
  if (is.null(Data)) {
    Data = LoadData(Config)
  } else if (class(Data) != "PROsetta.Data") {
    stop("Data must be a class of PROsetta.Data")
  }
  tmp = RunFrequency(Config, Data, checkFrequency = F)
  ni = dim(tmp)[1]
  nc = dim(tmp)[2]
  msg = c()
  if (sum(is.na(tmp)) > 0){
    for (i in 1:ni){
      nm = sum(is.na(tmp[i,]))
      ncats.observed = nc - nm
      if (sum(is.na(tmp[i,])) > 0){
        item.id = rownames(tmp[i,])
        idx = which(Data@itemmap[[Config@itemID]] == item.id)
        ncats.expected = Data@itemmap[idx,][['NCAT']]
        if (ncats.expected != ncats.observed){
          cats = colnames(tmp[i,])
          missingcats = which(is.na(tmp[i,]))
          cats[missingcats] = "missing"
          cats = paste0(cats, collapse = ", ")
          msg = c(msg, paste0("    ", item.id, " (", cats, ")"))
        }
      }
    }
  }
  if (length(msg) > 0){
    msg = c("The following items have one or more unobserved response categories:", msg)
    msg = c(msg, "Proceeding to RunCFA or RunCalibration can cause problems.")
    msg = paste0(msg, collapse = "\n")
    warning(msg)
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Obtain a frequency table
#'
#' Obtains a frequency table from the supplied configuration and the dataset.
#'
#' @param Config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param Data A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#' @param checkFrequency Logical. If \code{TRUE}, check the frequency table for missing response categories, and display warning message if any is missing.
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

RunFrequency = function(Config, Data = NULL, checkFrequency = T) {
  if (class(Config) != "PROsetta.Config") {
    stop("Config must be a class of PROsetta.Config")
  }
  if (is.null(Data)) {
    Data = LoadData(Config)
  } else if (class(Data) != "PROsetta.Data") {
    stop("Data must be a class of PROsetta.Data")
  }
  tmp = Data@response[Data@itemmap[[Config@itemID]]]
  tmp = apply(tmp, 2, table)
  if (class(tmp) == "list"){
    catnames = unique(do.call(c, lapply(tmp, names)))
    Freq = as.data.frame(matrix(NA, length(tmp), length(catnames)))
    colnames(Freq) = catnames
    rownames(Freq) = names(tmp)
    for (i in 1:length(tmp)){
      cats = names(tmp[[i]])
      Freq[i,cats] = tmp[[i]]
    }
  }
  if (class(tmp) == "matrix"){
    Freq = t(tmp)
  }
  if (checkFrequency) CheckFrequency(Config, Data)
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

RunDescriptive = function(Config, Data = NULL) {
  if (class(Config) != "PROsetta.Config") {
    stop("Config must be a class of PROsetta.Config")
  }
  if (is.null(Data)) {
    Data = LoadData(Config)
  } else if (class(Data) != "PROsetta.Data") {
    stop("Data must be a class of PROsetta.Data")
  }
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

RunClassical = function(Config, Data = NULL, omega = FALSE, ...) {
  if (class(Config) != "PROsetta.Config") {
    stop("Config must be a class of PROsetta.Config")
  }
  if (is.null(Data)) {
    Data = LoadData(Config)
  } else if (class(Data) != "PROsetta.Data") {
    stop("Data must be a class of PROsetta.Data")
  }
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

RunCFA = function(Config, Data = NULL, estimator = "WLSMV", std.lv = TRUE, ...) {
  if (class(Config) != "PROsetta.Config") {
    stop("Config must be a class of PROsetta.Config")
  }
  if (is.null(Data)) {
    Data = LoadData(Config)
  } else if (class(Data) != "PROsetta.Data") {
    stop("Data must be a class of PROsetta.Data")
  }
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
RunCalibration = function(Config, Data = NULL, ...) {
  if (class(Config) != "PROsetta.Config") {
    stop("Config must be a class of PROsetta.Config")
  }
  if (is.null(Data)) {
    Data = LoadData(Config)
  } else if (class(Data) != "PROsetta.Data") {
    stop("Data must be a class of PROsetta.Data")
  }
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

RunLinking = function(Config, Data = NULL, ...) {
  if (class(Config) != "PROsetta.Config") {
    stop("Config must be a class of PROsetta.Config")
  }
  if (is.null(Data)) {
    Data = LoadData(Config)
  } else if (class(Data) != "PROsetta.Data") {
    stop("Data must be a class of PROsetta.Data")
  }
  if (is.null(Data@anchor)) {
    stop("anchor cannot be NULL")
  }
  if (!Config@linkingMethod %in% c("MM","MS","HB","SL","LS")) {
    stop("Config@linkingMethod must be one of the following: \"MM\", \"MS\", \"HB\", \"SL\", \"LS\".")
  }
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
  rownames(out$pars@pars$From) = ID.new$ID
  rownames(out$pars@pars$To)   = ID.old$ID
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

RunEquateObserved = function(Config, Data = NULL, scaleTo = 1, scaleFrom = 2, type = "equipercentile", smooth = "loglinear", degrees = list(3, 1), boot = TRUE, reps = 100, ...) {
  if (class(Config) != "PROsetta.Config") {
    stop("Config must be a class of PROsetta.Config")
  }
  if (is.null(Data)) {
    Data = LoadData(Config)
  } else if (class(Data) != "PROsetta.Data") {
    stop("Data must be a class of PROsetta.Data")
  }
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

#' Run Scoring Table Generation
#'
#' Generates raw-score to scale-score crosswalk tables.
#'
#' @param Config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param Data A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#' @param Calibration An object returned from \code{\link{RunCalibration}} or \code{\link{RunLinking}}
#' @param priorMean Prior mean.
#' @param priorSD Prior standard deviation.
#' @param minTheta LL of theta grid.
#' @param maxTheta UL of theta grid.
#' @param inc Increment of theta grid.
#' @param minScore Minimum item score (0 or 1).
#' @param Tscore TRUE to convert theta to Tscore.
#'
#' @return A list containing crosswalk tables.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' inputData = LoadData(new.Config)
#' outCalib = RunCalibration(new.Config, inputData)
#' scoreTable = RunRSSS(outCalib)
#' outEquate = RunLinking(new.Config, inputData, technical = list(NCYCLES = 1000))
#' scoreTableLinking = RunRSSS(outEquate)
#' }

RunRSSS = function(Config, Data = NULL, Calibration, priorMean = 0.0, priorSD = 1.0, minTheta = -4.0, maxTheta = 4.0, inc = 0.01, minScore = 1, Tscore = TRUE) {
  if (class(Config) != "PROsetta.Config") {
    stop("Config must be a class of PROsetta.Config")
  }
  if (is.null(Data)) {
    Data = LoadData(Config)
  } else if (class(Data) != "PROsetta.Data") {
    stop("Data must be a class of PROsetta.Data")
  }
  if (is.null(attr(class(Calibration), "package"))) {
    item.par = Calibration$pars@pars$From
  } else if (isS4(Calibration) && attr(class(Calibration), "package") == "mirt") {
    item.par = mirt::coef(Calibration, IRTpars = TRUE, simplify = TRUE)$items
  }
  item.par.by.scale = split(data.frame(item.par), Data@itemmap[[Config@scaleID]])
  n.scale = length(item.par.by.scale)

  if (!all(minScore %in% c(0, 1))) {
    stop("minScore must contain only 0 or 1")
  }

  if (length(minScore) == 1) {
    if (n.scale > 1) {
      minScore = rep(minScore, n.scale + 1)
    }
  } else if (length(minScore) != n.scale + 1) {
    stop(sprintf("length of minScore must be either 1 or %i"), n.scale + 1)
  }
  
  rsss = function(ipar, base0) {
    theta = seq(minTheta, maxTheta, by = inc)
    nq = length(theta)
    NCAT = rowSums(!is.na(ipar))
    maxCat = max(NCAT)
    DISC = ipar[, 1]
    CB = ipar[, 2:maxCat]
    ni = dim(ipar)[1]
    pp = array(0, c(nq, ni, maxCat))
    for (i in 1:ni) {
      ps = matrix(0, nq, NCAT[i] + 1)
      ps[, 1] = 1
      ps[, NCAT[i] + 1] = 0
      for (k in 1:(NCAT[i] - 1)) {
        ps[, k + 1] = 1/(1 + exp(-DISC[i] * (theta - CB[i, k])))
      }
      for (k in 1:NCAT[i]) {
        pp[, i, k] = ps[, k] - ps[, k + 1]
      }
    }
    min.Raw.Score = 0 #minimum obtainable raw score
    max.Raw.Score = sum(NCAT) - ni #maximum obtainable raw score
    nScore = max.Raw.Score - min.Raw.Score + 1 #number of score points
    TCCinv = numeric(nScore) #initialize TCC scoring table
    Raw.Score = min.Raw.Score:max.Raw.Score #raw scores
    LH = matrix(0, nq, nScore) #initializing distribution of summed scores
    ncat = NCAT[1]
    maxScore = 0
    LH[, 1:ncat] = pp[, 1, 1:ncat]
    idx = ncat
    for (i in 2:ni) {
      ncat = NCAT[i]  #number of categories for item i
      maxScore = ncat - 1 #maximum score for item i
      score = 0:maxScore #score values for item i
      prob = pp[, i, 1:ncat] #category probabilities for item i
      pLH = matrix(0, nq, nScore) #place holder for LH
      for (k in 1:ncat) {
        for (h in 1:idx) {
          sco = Raw.Score[h] + score[k]
          position = which(Raw.Score == sco)
          pLH[, position] = pLH[, position] + LH[, h] * prob[, k]
        }
      }
      idx = idx + maxScore
      LH = pLH
    }
    Scale.Score = numeric(nScore) #score table for EAP
    SE = numeric(nScore) #SE for EAP
    prior = dnorm((theta - priorMean) / priorSD)
    posterior = LH * prior #posterior distribution
    den = colSums(posterior)
    den = matrix(rep(den, rep(nq, nScore)), nq, nScore)
    posterior = posterior / den
    for (j in 1:nScore) {
      Scale.Score[j] = sum(posterior[, j] * theta) / sum(posterior[, j]) #EAP
      SE[j] = sqrt(sum(posterior[, j] * (theta - Scale.Score[j])^2) / sum(posterior[, j])) #EAP
    }
    if (!base0) {
      Raw.Score = Raw.Score + ni
    }
    if (Tscore) {
      Scale.Score = round(Scale.Score * 10 + 50, 1)
      SE = round(SE * 10, 1)
    }
    rsss.table = data.frame(Raw = Raw.Score, Scale = Scale.Score, SE)
    return(rsss.table)
  }
  if (n.scale == 1) {
    score.table = rsss(item.par, minScore == 0)
    return(score.table)
  } else if (n.scale > 1) {
    score.table = vector(mode = "list", length = n.scale + 1)
    for (s in 1:n.scale) {
      score.table[[s]] = rsss(item.par.by.scale[[s]], minScore[s] == 0)
    }
    score.table[[n.scale + 1]] = rsss(item.par, minScore[n.scale + 1] == 0)
    names(score.table) = c(names(item.par.by.scale), "combined")
    return(score.table)
  }
}
