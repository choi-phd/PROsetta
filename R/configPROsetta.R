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
#'
#' \dontrun{
#' cfg <- new.config(
#'   responseFile = "data-raw/dat_axmasq_v2.csv",
#'   anchorFile = "data-raw/anchor_axmasq.csv",
#'   itemmapFile = "data-raw/imap_axmasq.csv",
#'   linkingMethod = "FIXEDPAR",
#'   guessID = TRUE
#' )
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

new.config <- function(studyName = "Study",
                       inputDirectory = getwd(), outputDirectory = getwd(),
                       itemID = "", personID = "", scaleID = "",
                       responseFile, itemmapFile, anchorFile,
                       linkingMethod = "FIXEDPAR",
                       guessID = F) {
  cfg <- new("PROsetta.Config",
                   inputDirectory  = inputDirectory,
                   outputDirectory = outputDirectory,
                   responseFile    = responseFile,
                   itemmapFile     = itemmapFile,
                   anchorFile      = anchorFile,
                   linkingMethod   = toupper(linkingMethod))
  if (guessID){
    p <- check.file.path(inputDirectory, responseFile)
    if (p$exists) {
      d <- read.csv(p$path, as.is = TRUE)
    }
    ids_response <- colnames(d)

    p <- check.file.path(inputDirectory, itemmapFile)
    if (p$exists) {
      d <- read.csv(p$path, as.is = TRUE)
    }
    ids_itemmap <- colnames(d)

    n_match <- rep(NA, dim(d)[2])
    for (j in 1:dim(d)[2]) {
      n_match[j] <- sum(ids_response %in% d[, j])
    }

    idx <- which(n_match == max(n_match))[1]
    cfg@itemID <- ids_itemmap[idx]
    cat(sprintf("ItemID guessed as  : %s\n", cfg@itemID))

    idx <- which(ids_response %in% d[,idx] == F)[1]
    cfg@personID <- ids_response[idx]
    cat(sprintf("PersonID guessed as: %s\n", cfg@personID))

    n_unique <- rep(NA, dim(d)[2])
    for (j in 1:dim(d)[2]) {
      n_unique[j] <- length(unique(d[, j]))
    }

    idx <- which((n_unique != max(n_unique)) & (n_unique != 1))[1]
    cfg@scaleID <- ids_itemmap[idx]
    cat(sprintf("ScaleID guessed as : %s\n", cfg@scaleID))

  } else {

    cfg@itemID   <- itemID
    cfg@personID <- personID
    cfg@scaleID  <- scaleID

  }

  return(cfg)
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

check.file.path <- function(abspath, path){

  p <- path
  if (file.exists(p)) {
    return(list(path = normalizePath(p), exists = TRUE))
  }

  p <- file.path(abspath, path)
  if (file.exists(p)) {
    return(list(path = normalizePath(p), exists = TRUE))
  }

  return(list(path = NULL, exists = FALSE))
}


#' Load data from supplied config
#'
#' Loads data from the supplied configuration.
#'
#' @param config A PROsetta.Config object. See \linkS4class{PROsetta.Config}.
#'
#' @return An S4-class object containing the loaded data.
#'
#' @export

LoadData <- function(config) {

  if (class(config) != "PROsetta.Config") {
    stop("config must be a class of PROsetta.Config")
  }

  data <- new("PROsetta.Data")

  p <- check.file.path(config@inputDirectory, config@responseFile)
  if (p$exists) {
    data@response <- read.csv(p$path, as.is = TRUE)
    if (!(config@personID %in% names(data@response))) {
      warning(sprintf("%s is not included in responseFile", config@personID))
    }
  }

  p <- check.file.path(config@inputDirectory, config@itemmapFile)
  if (p$exists) {
    data@itemmap <- read.csv(p$path, as.is = TRUE)
    if (!(config@itemID %in% names(data@itemmap))) {
      warning(sprintf("%s is not included in itemmapFile", config@itemID))
    }
  }

  p <- check.file.path(config@inputDirectory, config@anchorFile)
  if (p$exists) {
    data@anchor <- read.csv(p$path, as.is = TRUE)
    if (!(config@itemID %in% names(data@anchor))) {
      warning(sprintf("%s is not included in anchorFile", config@itemID))
    }
  }

  if (!is.null(data@itemmap) && !is.null(data@anchor)) {
    if (config@itemID %in% names(data@itemmap) && config@itemID %in% names(data@anchor)) {
      if (!all(data@anchor[[config@itemID]] %in% data@itemmap[[config@itemID]])) {
        stop(sprintf("%s in anchorFile contains items that are not in itemmapFile", config@itemID))
      }
    }
  }

  if (!is.null(data@itemmap) && !is.null(data@response)) {
    if (!all(data@itemmap[[config@itemID]] %in% names(data@response))) {
      stop(sprintf("%s in itemmapFile contains items that are not in responseFile", config@itemID))
    }
  }

  return(data)
}


#' Check frequency table for unobserved response categories
#'
#' Checks frequency table for unobserved response categories.
#'
#' @param config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param data A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#'
#' @return Logical. \code{TRUE} if all categories are present. \code{FALSE} otherwise.
#'
#' @export

CheckFrequency <- function(config, data){

  tmp <- RunFrequency(config, data, checkFrequency = F)
  ni  <- dim(tmp)[1]
  nc  <- dim(tmp)[2]
  msg <- c()

  if (sum(is.na(tmp)) > 0){

    for (i in 1:ni){

      nm        <- sum(is.na(tmp[i, ]))
      ncats_obs <- nc - nm

      if (nm > 0) {
        item_id   <- rownames(tmp[i,])
        idx       <- which(data@itemmap[[config@itemID]] == item_id)
        ncats_exp <- data@itemmap[idx, ][['NCAT']]

        if (ncats_exp != ncats_obs) {
          cats              <- colnames(tmp[i,])
          missingcats       <- which(is.na(tmp[i, ]))
          cats[missingcats] <- "missing"
          cats              <- paste0(cats, collapse = ", ")
          msg               <- c(msg, paste0("    ", item_id, " (", cats, ")"))
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
#' @param config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param data A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#' @param checkFrequency Logical. If \code{TRUE}, check the frequency table for missing response categories, and display warning message if any is missing.
#' @return A \code{data.frame} containing the frequency table of the dataset.
#'
#' @export
#'
#' @examples
#' freqTable <- RunFrequency(cfg_asq)

RunFrequency <- function(config, data = NULL, checkFrequency = TRUE) {

  if (class(config) != "PROsetta.Config") {
    stop("config must be a class of PROsetta.Config")
  }
  if (is.null(data)) {
    data <- LoadData(config)
  }
  if (class(data) != "PROsetta.Data") {
    stop("data must be a class of PROsetta.Data")
  }

  tmp <- data@response[data@itemmap[[config@itemID]]]
  tmp <- apply(tmp, 2, table)

  if (class(tmp) == "list") {
    catnames       <- unique(do.call(c, lapply(tmp, names)))
    freq           <- as.data.frame(matrix(NA, length(tmp), length(catnames)))
    colnames(freq) <- catnames
    rownames(freq) <- names(tmp)
    for (i in 1:length(tmp)) {
      cats         <- names(tmp[[i]])
      freq[i,cats] <- tmp[[i]]
    }
  }

  if (class(tmp) == "matrix") {
    freq <- t(tmp)
  }

  if (checkFrequency) {
    CheckFrequency(config, data)
  }

  return(freq)
}

#' Obtain a descriptive statistics table
#'
#' Obtains a table with descriptive statistics for each variable, from the supplied configuration and the dataset.
#'
#' @param config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param data (Optional) A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#'
#' @return A \code{data.frame} containing the descriptive statistics (mean, standard deviation, median, ...) of the variables in the dataset. These are calculated with \code{\link[psych]{describe}} in \href{https://CRAN.R-project.org/package=psych}{\code{psych}} package.
#'
#' @export
#'
#' @examples
#' descTable <- RunDescriptive(cfg_asq)

RunDescriptive <- function(config, data = NULL) {

    if (class(config) != "PROsetta.Config") {
    stop("config must be a class of PROsetta.Config")
  }
  if (is.null(data)) {
    data <- LoadData(config)
  }
  if (class(data) != "PROsetta.Data") {
    stop("data must be a class of PROsetta.Data")
  }

  desc <- psych::describe(data@response[data@itemmap[[config@itemID]]])[-1]
  return(desc)
}

#' Run a CTT-based reliability analysis
#'
#' Performs a Classial Test Theory (CTT) reliability analysis.
#'
#' @param config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param data (Optional) A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#' @param omega If TRUE, also obtains McDonald's omega using \code{\link[psych]{omega}} in \href{https://CRAN.R-project.org/package=psych}{\code{psych}} package.
#' @param ... Additional arguments to pass onto \code{\link[psych]{omega}}.
#'
#' @return The results of reliability analysis.
#'
#' @export
#'
#' @examples
#' classicalTable  <- RunClassical(cfg_asq)
#' classicalTable2 <- RunClassical(cfg_asq, omega = TRUE)  # also obtains omega

RunClassical <- function(config, data = NULL, omega = FALSE, ...) {

  if (class(config) != "PROsetta.Config") {
    stop("config must be a class of PROsetta.Config")
  }
  if (is.null(data)) {
    data <- LoadData(config)
  }
  if (class(data) != "PROsetta.Data") {
    stop("data must be a class of PROsetta.Data")
  }

  CIA <- psych::alpha(data@response[data@itemmap[[config@itemID]]])
  if (omega) {
    CIA[["Omega"]] <- psych::omega(data@response[data@itemmap[[config@itemID]]], ...)
  }

  return(CIA)
}


#' Run a confirmatory factor analysis
#'
#' Performs a one-factor confirmatory factor analysis (CFA) to test unidimensionality.
#'
#' @param config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param data (Optional) A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#' @param estimator The estimator to be used. Passed onto \code{\link[lavaan]{cfa}} in \href{https://CRAN.R-project.org/package=lavaan}{\code{lavaan}} package.
#' @param std.lv If \code{TRUE}, the metric of the latent variable is determined by fixing their (residual) variances to 1.0. If \code{FALSE}, the metric of each latent variable is determined by fixing the factor loading of the first indicator to 1.0. Passed onto \code{\link[lavaan]{cfa}}.
#' @param ... Additional arguments to pass onto \code{\link[lavaan]{cfa}}.
#'
#' @return A list containing the CFA results. The models are as follows:
#' \item{all}{A one-factor model where all items in the \code{itemmap} slot of \code{data} are loaded onto the factor.}
#' \item{anchor}{A one-factor model where the items in the \code{anchor} slot of \code{data} are loaded onto the factor.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' solution <- RunCFA(cfg_asq)
#' summary(solution$all, fit.measures = TRUE, standardized = TRUE)
#' summary(solution$anchor, fit.measures = TRUE, standardized = TRUE)
#' }

RunCFA <- function(config, data = NULL, estimator = "WLSMV", std.lv = TRUE, ...) {

  if (class(config) != "PROsetta.Config") {
    stop("config must be a class of PROsetta.Config")
  }
  if (is.null(data)) {
    data <- LoadData(config)
  }
  if (class(data) != "PROsetta.Data") {
    stop("data must be a class of PROsetta.Data")
  }

  all_items     <- data@itemmap[[config@itemID]]
  model_all     <- paste("Factor =~ ", paste0(all_items, collapse = " + "))
  model_all_fit <- lavaan::cfa(model_all, data@response, estimator = estimator, ordered = all_items, std.lv = std.lv, ...)
  out           <- list(all = model_all_fit)

  if (!is.null(data@anchor)) {
    anchor_items     <- data@anchor[[config@itemID]]
    model_anchor     <- paste("Factor =~ ", paste0(anchor_items, collapse = " + "))
    model_anchor_fit <- lavaan::cfa(model_anchor, data@response, estimator = estimator, ordered = anchor_items, std.lv = std.lv, ...)
    out$anchor       <- model_anchor_fit
  }

  return(out)
}

#' Run Calibration
#'
#' Performs item calibration for the response data based on the supplied anchor information.
#'
#' @param config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param Data A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#' @param ... Additional arguments to pass onto \code{\link[mirt]{mirt}} in \href{https://CRAN.R-project.org/package=mirt}{\code{mirt}} package.
#'
#' @return An object containing item calibration results. This object can be used in \code{\link[mirt:coef-method]{coef}}, \code{\link[mirt]{itemfit}}, \code{\link[mirt]{itemplot}} in \href{https://CRAN.R-project.org/package=mirt}{\code{mirt}} package to extract wanted information.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' solution <- RunCalibration(cfg_asq)
#' mirt::coef(solution, IRTpars = TRUE, simplify = TRUE)
#' mirt::itemfit(solution, empirical.plot = 1)
#' mirt::itemplot(solution, item = 1, type = "info")
#' mirt::itemfit(solution, "S_X2", na.rm = TRUE)
#'
#' cfg_asq2 <- cfg_asq
#' cfg_asq2@linkingMethod = "SL"
#' solution2 <- RunCalibration(cfg_asq2)
#' solution2 <- RunCalibration(cfg_asq2, technical = list(NCYCLES = 1000))
#' mirt::coef(solution2, IRTpars = TRUE, simplify = TRUE)
#' }
RunCalibration = function(config, data = NULL, ...) {

  if (class(config) != "PROsetta.Config") {
    stop("config must be a class of PROsetta.Config")
  }
  if (is.null(data)) {
    data <- LoadData(config)
  }
  if (class(data) != "PROsetta.Data") {
    stop("data must be a class of PROsetta.Data")
  }

  if (config@linkingMethod == "FIXEDPAR") {

    par_layout <- mirt::mirt(data@response[data@itemmap[[config@itemID]]], 1, itemtype = "graded", pars = "values")
    fixed      <- which(par_layout$item %in% data@anchor[[config@itemID]])
    par_layout[fixed, "est"] = FALSE
    par_layout[which(par_layout$class == "GroupPars"), "est"] = TRUE

    for (i in fixed) {

      item <- which(data@anchor[[config@itemID]] == par_layout$item[i])

      if (substr(par_layout$name[i], 1, 1) == "a") {
        par_layout[i, "value"] = data@anchor[item, "a"]
      } else {
        k <- as.numeric(gsub("[[:alpha:]]", "", par_layout$name[i]))
        par_layout[i, "value"] = -data@anchor[item, "a"] * data@anchor[item, paste0("cb", k)]
      }

    }

    calibration <- mirt::mirt(data@response[data@itemmap[[config@itemID]]], 1, itemtype = "graded", pars = par_layout, ...)

  } else {

    calibration <- mirt::mirt(data@response[data@itemmap[[config@itemID]]], 1, itemtype = "graded", ...)

  }
  # ipar = mirt::coef(calibration, IRTpars = TRUE, simplify = TRUE)$items
  return(calibration)
}

#' Run Scale Linking
#'
#' Performs scale linking and obtains a set of transformation coefficients.
#'
#' @param config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param data (Optional) A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#' @param ... Additional arguments to pass onto \code{\link[mirt]{mirt}} in \href{https://CRAN.R-project.org/package=mirt}{\code{mirt}} package.
#'
#' @return A list containing the scale linking results. These are obtained with \code{\link[plink]{plink-methods}} in \href{https://CRAN.R-project.org/package=plink}{\code{plink}} package.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cfg_asq2 <- cfg_asq
#' cfg_asq2@linkingMethod = "SL"
#' solution <- RunLinking(cfg_asq2, technical = list(NCYCLES = 1000))
#' solution$link@constants$SL
#' }

RunLinking <- function(config, data = NULL, ...) {

  if (class(config) != "PROsetta.Config") {
    stop("config must be a class of PROsetta.Config")
  }
  if (is.null(data)) {
    data <- LoadData(config)
  }
  if (class(data) != "PROsetta.Data") {
    stop("data must be a class of PROsetta.Data")
  }
  if (is.null(data@anchor)) {
    stop("anchor cannot be NULL")
  }
  if (!config@linkingMethod %in% c("MM","MS","HB","SL","LS")) {
    stop("config@linkingMethod must be one of the following: \"MM\", \"MS\", \"HB\", \"SL\", \"LS\".")
  }

  calibration = RunCalibration(config, data, ...)
  ipar      = mirt::coef(calibration, IRTpars = TRUE, simplify = TRUE)$items
  ni_all    = nrow(ipar)
  ni_anchor = nrow(data@anchor)
  maxCat    = max(data@anchor$NCAT)
  id_new    = data.frame(New = 1:ni_all, ID = data@itemmap[[config@itemID]])
  id_old    = data.frame(Old = 1:ni_anchor, ID = data@anchor[[config@itemID]])
  common    = merge(id_new, id_old, by = "ID", sort = FALSE)[c("New", "Old")]
  pars      = vector("list", 2)

  pars[[1]] = ipar
  pars[[2]] = data@anchor[c("a", paste0("cb", 1:(maxCat - 1)))]

  pm_all = as.poly.mod(ni_all, "grm", 1:ni_all)
  pm_anchor = as.poly.mod(ni_anchor, "grm", 1:ni_anchor)
  ncat = list(data@itemmap$NCAT, data@anchor$NCAT)
  out = plink::plink(as.irt.pars(pars, common, cat = ncat, list(pm_all, pm_anchor), grp.names=c("From","To")), rescale = config@linkingMethod, base.grp = 2)

  rownames(out$pars@pars$From) = id_new$ID
  rownames(out$pars@pars$To)   = id_old$ID

  return(out)
}

#' Run Test Equating
#'
#' Performs equipercentile test equating between two scales.
#'
#' @param config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param data A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#' @param scaleTo Numeric. The index of the target scale to equate to. This and \code{scaleFrom} below both reference to the information stored in the \code{itemmap} slot of \code{Data} argument. The \code{scaleID} slot of \code{config} argument needs to be specified as the name of the varible containing the scale IDs in the \code{itemmap} slot.
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
#' solution <- RunEquateObserved(cfg_asq, scaleTo = 1, scaleFrom = 2,
#'             type = "equipercentile", smooth = "loglinear")
#' }

RunEquateObserved = function(config, data = NULL, scaleTo = 1, scaleFrom = 2, type = "equipercentile", smooth = "loglinear", degrees = list(3, 1), boot = TRUE, reps = 100, ...) {

  if (class(config) != "PROsetta.Config") {
    stop("config must be a class of PROsetta.Config")
  }
  if (is.null(data)) {
    data <- LoadData(config)
  }
  if (class(data) != "PROsetta.Data") {
    stop("data must be a class of PROsetta.Data")
  }

  scaleID    <- data@itemmap[[config@scaleID]]
  scaleCode  <- unique(scaleID)
  itemsTo    <- which(scaleID %in% scaleTo)
  itemsFrom  <- which(scaleID %in% scaleFrom)
  scoresTo   <- rowSums(data@response[data@itemmap[[config@itemID]][itemsTo]])
  scoresFrom <- rowSums(data@response[data@itemmap[[config@itemID]][itemsFrom]])
  freqTo     <- equate::freqtab(data@response[data@itemmap[[config@itemID]]], items = itemsTo)
  freqFrom   <- equate::freqtab(data@response[data@itemmap[[config@itemID]]], items = itemsFrom)
  scoreStat  <- rbind(From = summary(freqFrom), To = summary(freqTo))

  #plot(x = freqTo, lwd = 2, xlab = "Score", ylab = "Count")
  #plot(x = sfreqTo, lwd = 2, xlab = "Score", ylab = "Count")

  if (smooth != "none") {
    freqTo   <- equate::presmoothing(freqTo,   smoothmethod = smooth, degrees = degrees)
    freqFrom <- equate::presmoothing(freqFrom, smoothmethod = smooth, degrees = degrees)
  }

  out <- equate::equate(freqFrom, freqTo, type = type, boot = boot, reps = reps, ...)
  return(out)
}

#' Run Scoring Table Generation
#'
#' Generates raw-score to scale-score crosswalk tables.
#'
#' @param config A PROsetta.Config object. See \code{\linkS4class{PROsetta.Config}}.
#' @param data (Optional) A PROsetta.Data object. See \code{\link{LoadData}} for loading a dataset.
#' @param calibration An object returned from \code{\link{RunCalibration}} or \code{\link{RunLinking}}
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
#' solution <- RunCalibration(cfg_asq, inputData)
#' scoreTable = RunRSSS(outCalib)
#' outEquate = RunLinking(new.Config, inputData, technical = list(NCYCLES = 1000))
#' scoreTableLinking = RunRSSS(outEquate)

RunRSSS <- function(config, data = NULL, calibration, priorMean = 0.0, priorSD = 1.0, minTheta = -4.0, maxTheta = 4.0, inc = 0.01, minScore = 1, Tscore = TRUE) {

  if (class(config) != "PROsetta.Config") {
    stop("config must be a class of PROsetta.Config")
  }
  if (is.null(data)) {
    data <- LoadData(config)
  }
  if (class(data) != "PROsetta.Data") {
    stop("data must be a class of PROsetta.Data")
  }
  if (is.null(attr(class(calibration), "package"))) {
    item_par <- calibration$pars@pars$From
  } else if (isS4(calibration) && attr(class(calibration), "package") == "mirt") {
    item_par <- mirt::coef(calibration, IRTpars = TRUE, simplify = TRUE)$items
  }

  item_par_by_scale <- split(data.frame(item_par), data@itemmap[[config@scaleID]])
  n_scale <- length(item_par_by_scale)

  rsss <- function(ipar) {

    theta  <- seq(minTheta, maxTheta, by = inc)
    nq     <- length(theta)
    NCAT   <- rowSums(!is.na(ipar))
    maxCat <- max(NCAT)
    DISC   <- ipar[, 1]
    CB     <- ipar[, 2:maxCat]
    ni     <- dim(ipar)[1]
    pp     <- array(0, c(nq, ni, maxCat))

    for (i in 1:ni) {

      ps                <- matrix(0, nq, NCAT[i] + 1)
      ps[, 1]           <- 1
      ps[, NCAT[i] + 1] <- 0

      for (k in 1:(NCAT[i] - 1)) {
        ps[, k + 1] <- 1/(1 + exp(-DISC[i] * (theta - CB[i, k])))
      }
      for (k in 1:NCAT[i]) {
        pp[, i, k] <- ps[, k] - ps[, k + 1]
      }

    }

    min_raw_score <- 0                                 # minimum obtainable raw score
    max_raw_score <- sum(NCAT) - ni                    # maximum obtainable raw score
    n_score       <- max_raw_score - min_raw_score + 1 # number of score points
    inv_TCC       <- numeric(n_score)                  # initialize TCC scoring table
    raw_score     <- min_raw_score:max_raw_score       # raw scores
    LH            <- matrix(0, nq, n_score)            # initialize distribution of summed scores

    ncat         <- NCAT[1]
    max_score    <- 0
    LH[, 1:ncat] <- pp[, 1, 1:ncat]

    idx <- ncat

    for (i in 2:ni) {

      ncat <- NCAT[i]                 # number of categories for item i
      max_score <- ncat - 1           # maximum score for item i
      score <- 0:max_score            # score values for item i
      prob  <- pp[, i, 1:ncat]        # category probabilities for item i
      pLH   <- matrix(0, nq, n_score) # place holder for LH

      for (k in 1:ncat) {
        for (h in 1:idx) {
          sco <- raw_score[h] + score[k]
          position <- which(raw_score == sco)
          pLH[, position] = pLH[, position] + LH[, h] * prob[, k]
        }
      }

      idx <- idx + max_score
      LH  <- pLH

    }

    scale_score <- numeric(n_score) #score table for EAP
    SE          <- numeric(n_score) #SE for EAP
    prior     <- dnorm((theta - priorMean) / priorSD)
    posterior <- LH * prior
    den       <- colSums(posterior)
    den       <- matrix(rep(den, rep(nq, n_score)), nq, n_score)
    posterior <- posterior / den

    for (j in 1:n_score) {
      scale_score[j] <- sum(posterior[, j] * theta) / sum(posterior[, j])                   # EAP
      SE[j] <- sqrt(sum(posterior[, j] * (theta - scale_score[j])^2) / sum(posterior[, j])) # EAP
    }

    if (minScore == 1) {
      raw_score <- raw_score + ni
    }
    if (Tscore) {
      scale_score <- round(scale_score * 10 + 50, 1)
      SE          <- round(SE * 10, 1)
    }

    rsss_table <- data.frame(Raw = raw_score, Scale = scale_score, SE)

    return(rsss_table)
  }

  if (n_scale == 1) {

    score_table <- rsss(item_par)
    return(score_table)

  } else if (n_scale > 1) {

    score_table <- vector(mode = "list", length = length(item_par_by_scale) + 1)

    for (s in 1:n_scale) {
      score_table[[s]] <- rsss(item_par_by_scale[[s]])
    }

    score_table[[n_scale + 1]] <- rsss(item_par)
    names(score_table) <- c(names(item_par_by_scale), "combined")

    return(score_table)
  }
}
