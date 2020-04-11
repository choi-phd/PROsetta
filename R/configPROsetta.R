#' @include import.R
NULL



#' An S4 class to represent \code{PROsetta} datasets.
#'
#' @rdname loadData

setClass("PROsetta_data",
  slots = c(
    response = "list",
    itemmap = "list",
    anchor = "list",
    item_id = "character",
    person_id = "character",
    scale_id = "character"
  ),
  prototype = list(
    response = NULL,
    itemmap = NULL,
    anchor = NULL,
    item_id = "",
    person_id = "",
    scale_id = ""
  ),
  validity = function(object) {

    msg_all <- c()
    if (!object@person_id %in% names(object@response)) {
      msg <- sprintf("column '%s' for person ID does not exist in response data", object@person_id)
      msg_all <- c(msg_all, msg)
    }
    if (!(object@item_id %in% names(object@itemmap))) {
      msg <- sprintf("column '%s' for item ID does not exist in item map", object@item_id)
      msg_all <- c(msg_all, msg)
    }
    if (!(object@item_id %in% names(object@anchor))) {
      msg <- sprintf("column '%s' for item ID does not exist in anchor data", object@item_id)
      msg_all <- c(msg_all, msg)
    }
    if (!is.null(object@itemmap) && !is.null(object@anchor)) {
      if (!all(object@anchor[[object@item_id]] %in% object@itemmap[[object@item_id]])) {
        msg <- sprintf("column '%s' in anchor data contains items that are not in itemmap_file", object@item_id)
        msg_all <- c(msg_all, msg)
      }
    }
    if (!is.null(object@itemmap) && !is.null(object@response)) {
      if (!all(object@itemmap[[object@item_id]] %in% names(object@response))) {
        msg <- sprintf("column '%s' in item map contains items that are not in response data", object@item_id)
        msg_all <- c(msg_all, msg)
      }
    }
    if (length(msg_all) > 0) {
      return(msg_all)
    }
    return(TRUE)
  }
)



#' Load data from supplied config
#'
#' Loads data from the supplied configuration.
#'
#' @param response A dataset containing case IDs and item responses. Can be a .csv file name or a data frame.
#' @param itemmap An item map containing item IDs and scale IDs. Can be a .csv file name or a data frame.
#' @param anchor Item parameters for anchor items. Can be a .csv file name or a data frame.
#' @param item_id The column name to look for item IDs. Automatically determined if not specified.
#' @param person_id The column name to look for case IDs. Automatically determined if not specified.
#' @param scale_id The column name to look for scale IDs. Automatically determined if not specified.
#' @param input_dir The directory to look for the files.
#'
#' @return A \code{\linkS4class{PROsetta_data}} object containing the loaded data.
#'
#' @export

loadData <- function(response, itemmap, anchor,
  item_id = NULL, person_id = NULL, scale_id = NULL, input_dir = getwd()) {

  if (inherits(response, "character")) {
    p <- check_fp(input_dir, response)
    if (!p$exists) stop(sprintf("'response': cannot find the specified file %s", p$path))
    response <- read.csv(p$path, as.is = TRUE)
  } else if (inherits(response, "matrix")) {
    response <- as.data.frame(response)
  } else if (!inherits(response, "data.frame")) {
    stop(sprintf("'response': unrecognized object type %s", class(response)))
  }

  if (inherits(itemmap, "character")) {
    p <- check_fp(input_dir, itemmap)
    if (!p$exists) stop(sprintf("'itemmap': cannot find the specified file %s", p$path))
    itemmap <- read.csv(p$path, as.is = TRUE)
  } else if (inherits(itemmap, "matrix")) {
    itemmap <- as.data.frame(itemmap)
  } else if (!inherits(itemmap, "data.frame")) {
    stop(sprintf("'itemmap': unrecognized object type %s", class(itemmap)))
  }

  if (inherits(anchor, "character")) {
    p <- check_fp(input_dir, anchor)
    if (!p$exists) stop(sprintf("'anchor': cannot find the specified file %s", p$path))
    anchor <- read.csv(p$path, as.is = TRUE)
  } else if (inherits(anchor, "matrix")) {
    anchor <- as.data.frame(anchor)
  } else if (!inherits(anchor, "data.frame")) {
    stop(sprintf("'anchor': unrecognized object type %s", class(anchor)))
  }


  names_response <- colnames(response)
  names_itemmap  <- colnames(itemmap)


  if ("reverse" %in% names_itemmap) {
    if (any(itemmap$reverse == 1)) {
      message("some items were marked as reversed items in 'reverse' column in item map")
      message("assuming the response data is already reverse coded")
    } else {
      message("'reverse' column is present in item map and no items were marked as reversed")
    }
  }


  # Guess IDs

  n_ids <- sum(!is.null(item_id), !is.null(person_id), !is.null(scale_id))

  if (n_ids < 3 & n_ids > 0) {
    stop("specify item_id, person_id, scale_id all three to override ID guessing.")
  }

  if (n_ids == 0) {

    n_match <- rep(NA, dim(itemmap)[2])
    for (j in 1:dim(itemmap)[2]) {
      n_match[j] <- sum(names_response %in% itemmap[, j])
    }
    idx <- which(n_match == max(n_match))[1]
    item_id <- names_itemmap[idx]
    cat(sprintf("item_id guessed as   : %s\n", item_id))

    idx <- which(names_response %in% itemmap[[item_id]] == F)[1]
    person_id <- names_response[idx]
    cat(sprintf("person_id guessed as : %s\n", person_id))

    n_unique <- rep(NA, dim(itemmap)[2])
    for (j in 1:dim(itemmap)[2]) {
      n_unique[j] <- length(unique(itemmap[, j]))
    }

    idx <- which((n_unique != max(n_unique)) & (n_unique != 1))[1]
    scale_id <- names_itemmap[idx]
    cat(sprintf("scale_id guessed as  : %s\n", scale_id))

  }


  # Create PROsetta_data object

  data <- new("PROsetta_data")
  data@response  <- response
  data@itemmap   <- itemmap
  data@anchor    <- anchor
  data@item_id   <- item_id
  data@person_id <- person_id
  data@scale_id  <- scale_id

  if (validObject(data)) {
    return(data)
  }
}

#' Check frequency table for unobserved response categories
#'
#' Checks frequency table for unobserved response categories.
#'
#' @param data A \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#'
#' @return \code{TRUE} if all categories are present. \code{FALSE} otherwise.
#'
#' @export

checkFrequency <- function(data) {

  if (!inherits(data, "PROsetta_data")) {
    stop("data must be a 'PROsetta_data' class object")
  }

  tmp <- runFrequency(data, check_frequency = FALSE)
  ni <- dim(tmp)[1]
  nc <- dim(tmp)[2]
  msg <- c()

  if (sum(is.na(tmp)) > 0) {
    for (i in 1:ni) {
      nm <- sum(is.na(tmp[i, ]))
      ncats_obs <- nc - nm

      if (nm > 0) {
        item_id <- rownames(tmp[i, ])
        idx <- which(data@itemmap[[data@item_id]] == item_id)
        ncats_exp <- data@itemmap[idx, ][["NCAT"]]

        if (ncats_exp != ncats_obs) {
          cats <- colnames(tmp[i, ])
          missingcats <- which(is.na(tmp[i, ]))
          cats[missingcats] <- "missing"
          cats <- paste0(cats, collapse = ", ")
          msg <- c(msg, paste0("    ", item_id, " (", cats, ")"))
        }
      }
    }
  }
  if (length(msg) > 0) {
    msg <- c("The following items have one or more unobserved response categories:", msg)
    msg <- c(msg, "Proceeding to runCFA or runCalibration may cause problems.")
    msg <- paste0(msg, collapse = "\n")
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
#' @param data A \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#' @param check_frequency Logical. If \code{TRUE}, check the frequency table for missing response categories, and display warning message if any is missing.
#'
#' @return A \code{data.frame} containing the frequency table of the dataset.
#'
#' @examples
#' out_freq <- runFrequency(data_asq)
#'
#' @export

runFrequency <- function(data, check_frequency = TRUE) {

  if (!inherits(data, "PROsetta_data")) {
    stop("data must be a 'PROsetta_data' class object")
  }

  tmp <- data@response[data@itemmap[[data@item_id]]]
  tmp <- apply(tmp, 2, table)

  if (inherits(tmp, "list")) {
    catnames <- unique(do.call(c, lapply(tmp, names)))
    freq <- as.data.frame(matrix(NA, length(tmp), length(catnames)))
    colnames(freq) <- catnames
    rownames(freq) <- names(tmp)
    for (i in 1:length(tmp)) {
      cats <- names(tmp[[i]])
      freq[i, cats] <- tmp[[i]]
    }
  }

  if (inherits(tmp, "matrix")) {
    freq <- t(tmp)
  }

  if (check_frequency) {
    checkFrequency(data)
  }

  return(freq)
}

#' Obtain a descriptive statistics table
#'
#' Obtains a table with descriptive statistics for each variable, from the supplied configuration and the dataset.
#'
#' @param data A PROsetta_data object. See \code{\link{loadData}} for loading a dataset.
#'
#' @return A \code{data.frame} containing the descriptive statistics (mean, standard deviation, median, ...) of the variables in the dataset. These are calculated with \code{\link[psych]{describe}} in \href{https://CRAN.R-project.org/package=psych}{\code{psych}} package.
#'
#' @examples
#' out_desc <- runDescriptive(data_asq)
#'
#' @export

runDescriptive <- function(data = NULL) {

  if (!inherits(data, "PROsetta_data")) {
    stop("data must be a 'PROsetta_data' class object")
  }

  desc <- psych::describe(data@response[data@itemmap[[data@item_id]]])[-1]
  return(desc)

}

#' Run a CTT-based reliability analysis
#'
#' Performs a Classial Test Theory (CTT) reliability analysis.
#'
#' @param data A PROsetta_data object. See \code{\link{loadData}} for loading a dataset.
#' @param omega If TRUE, also obtains McDonald's omega using \code{\link[psych]{omega}} in \href{https://CRAN.R-project.org/package=psych}{\code{psych}} package.
#' @param scalewise If TRUE, run analysis for each scale as well as for the combined scale. If FALSE (default), run analysis only for the combined scale.
#' @param ... Additional arguments to pass onto \code{\link[psych]{omega}}.
#'
#' @return The results of reliability analysis.
#'
#' @examples
#' out_alpha <- runClassical(data_asq)
#' out_omega <- runClassical(data_asq, omega = TRUE) # also obtains omega
#'
#' @export

runClassical <- function(data, omega = FALSE, scalewise = FALSE, ...) {

  if (!inherits(data, "PROsetta_data")) {
    stop("data must be a 'PROsetta_data' class object")
  }

  out_alpha = list()
  out_omega = list()

  if (scalewise) {
    for (scale_id in unique(data@itemmap[[data@scale_id]])) {
      itemmap <- subset(data@itemmap, data@itemmap[[data@scale_id]] == scale_id)
      items <- itemmap[[data@item_id]]
      out_alpha[[as.character(scale_id)]] <- psych::alpha(data@response[items])
      if (omega) {
        out_omega[[as.character(scale_id)]] <- psych::omega(data@response[items], ...)
      }
    }
  }

  items <- data@itemmap[[data@item_id]]
  out_alpha[['combined']] <- psych::alpha(data@response[items])
  if (omega) {
    out_omega[['combined']] <- psych::omega(data@response[items], ...)
  }

  return(list(
    alpha = out_alpha,
    omega = out_omega
  ))

}


#' Run a confirmatory factor analysis
#'
#' Performs a one-factor confirmatory factor analysis (CFA) to test unidimensionality.
#'
#' @param data A PROsetta_data object. See \code{\link{loadData}} for loading a dataset.
#' @param estimator The estimator to be used. Passed onto \code{\link[lavaan]{cfa}} in \href{https://CRAN.R-project.org/package=lavaan}{\code{lavaan}} package.
#' @param std.lv If \code{TRUE}, the metric of the latent variable is determined by fixing their (residual) variances to 1.0. If \code{FALSE}, the metric of each latent variable is determined by fixing the factor loading of the first indicator to 1.0. Passed onto \code{\link[lavaan]{cfa}}.
#' @param scalewise If TRUE, run analysis for each scale as well as for the combined scale. If FALSE (default), run analysis only for the combined scale.
#' @param ... Additional arguments to pass onto \code{\link[lavaan]{cfa}}.
#'
#' @return A list containing the CFA results. The models are as follows:
#' \item{all}{A one-factor model where all items in the \code{itemmap} slot of \code{data} are loaded onto the factor.}
#' \item{anchor}{A one-factor model where the items in the \code{anchor} slot of \code{data} are loaded onto the factor.}
#'
#' @examples
#' \donttest{
#' out_cfa <- runCFA(data_asq, scalewise = TRUE)
#' lavaan::summary(out_cfa$`1`       , fit.measures = TRUE, standardized = TRUE, estimates = FALSE)
#' lavaan::summary(out_cfa$`2`       , fit.measures = TRUE, standardized = TRUE, estimates = FALSE)
#' lavaan::summary(out_cfa$`combined`, fit.measures = TRUE, standardized = TRUE, estimates = FALSE)
#' }
#' @export

runCFA <- function(data, estimator = "WLSMV", std.lv = TRUE, scalewise = FALSE, ...) {

  if (!inherits(data, "PROsetta_data")) {
    stop("data must be a 'PROsetta_data' class object")
  }

  out <- list()

  if (scalewise) {
    for (scale_id in unique(data@itemmap[[data@scale_id]])) {
      itemmap <- subset(data@itemmap, data@itemmap[[data@scale_id]] == scale_id)
      items <- itemmap[[data@item_id]]
      model <- paste("Factor =~ ", paste0(items, collapse = " + "))
      model_fit <- lavaan::cfa(model, data@response, estimator = estimator, ordered = items, std.lv = std.lv, ...)
      out[[as.character(scale_id)]] <- model_fit
    }
  }

  itemmap <- data@itemmap
  items <- itemmap[[data@item_id]]
  model <- paste("Factor =~ ", paste0(items, collapse = " + "))
  model_fit <- lavaan::cfa(model, data@response, estimator = estimator, ordered = items, std.lv = std.lv, ...)
  out[['combined']] <- model_fit

  return(out)
}

#' Run Calibration
#'
#' Performs item calibration for the response data based on the supplied anchor information.
#'
#' @param data A PROsetta_data object. See \code{\link{loadData}} for loading a dataset.
#' @param fixedpar If TRUE (default), perform fixed parameter calibration using anchor data.
#' @param ignore_nonconv If TRUE, return results even when calibration did not converge. Defaults to FALSE.
#' @param ... Additional arguments to pass onto \code{\link[mirt]{mirt}} in \href{https://CRAN.R-project.org/package=mirt}{\code{mirt}} package.
#'
#' @return An object containing item calibration results. This object can be used in \code{\link[mirt:coef-method]{coef}}, \code{\link[mirt]{itemfit}}, \code{\link[mirt]{itemplot}} in \href{https://CRAN.R-project.org/package=mirt}{\code{mirt}} package to extract wanted information.
#'
#' @examples
#' \donttest{
#' out_calib <- runCalibration(data_asq) # errors
#' out_calib <- runCalibration(data_asq, technical = list(NCYCLES = 1000))
#'
#' mirt::coef(out_calib, IRTpars = TRUE, simplify = TRUE)
#' mirt::itemfit(out_calib, empirical.plot = 1)
#' mirt::itemplot(out_calib, item = 1, type = "info")
#' mirt::itemfit(out_calib, "S_X2", na.rm = TRUE)
#' }
#' @export

runCalibration <- function(data, fixedpar = FALSE, ignore_nonconv = FALSE, ...) {

  if (!inherits(data, "PROsetta_data")) {
    stop("data must be a 'PROsetta_data' class object")
  }

  resp_data <- data@response[data@itemmap[[data@item_id]]]
  ni <- dim(resp_data)[2]

  message(sprintf("response data has %i items", ni))

  if (fixedpar) {

    par_layout <- mirt::mirt(resp_data, 1, itemtype = "graded", pars = "values")
    fixed <- which(par_layout$item %in% data@anchor[[data@item_id]])
    ni_fixed <- length(unique(par_layout[fixed, "item"]))
    message(sprintf("performing fixed parameter calibration, fixing %i items from anchor data", ni_fixed))

    par_layout[fixed, "est"] <- FALSE
    par_layout[which(par_layout$class == "GroupPars"), "est"] <- TRUE

    for (i in fixed) {
      item <- which(data@anchor[[data@item_id]] == par_layout$item[i])

      if (substr(par_layout$name[i], 1, 1) == "a") {
        par_layout[i, "value"] <- data@anchor[item, "a"]
      } else {
        k <- as.numeric(gsub("[[:alpha:]]", "", par_layout$name[i]))
        par_layout[i, "value"] <- -data@anchor[item, "a"] * data@anchor[item, paste0("cb", k)]
      }
    }

    calibration <- mirt::mirt(resp_data, 1, itemtype = "graded", pars = par_layout, ...)
  } else {
    message("performing free calibration of all items, ignoring anchor data")
    calibration <- mirt::mirt(resp_data, 1, itemtype = "graded", ...)
  }

  if (calibration@OptimInfo$iter == calibration@Options$NCYCLES) {
    msg = sprintf("calibration did not converge: increase iteration limit by adjusting the 'technical' argument, e.g., technical = list(NCYCLES = %i)", calibration@Options$NCYCLES + 500)
    if (ignore_nonconv) {
      warning(msg)
    } else {
      stop(msg)
    }
  }

  return(calibration)
}

#' Run Scale Linking
#'
#' Performs scale linking and obtains a set of transformation coefficients.
#'
#' @param data A \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#' @param method The type of linking to perform. Accepts "MM" for mean-mean, "MS" for mean-sigma, "HB" for Haebara, "SL" for Stocking-Lord, "FIXEDPAR" for fixed parameter calibration.
#' @param ... Additional arguments to pass onto \code{\link[mirt]{mirt}} in \href{https://CRAN.R-project.org/package=mirt}{\code{mirt}} package.
#'
#' @return A list containing the scale linking results. These are obtained with \code{\link[plink]{plink-methods}} in \href{https://CRAN.R-project.org/package=plink}{\code{plink}} package.
#'
#' @examples
#' \donttest{
#' out_link <- runLinking(data_asq, "SL", technical = list(NCYCLES = 1000))
#' out_link$constants   # transformation constants
#' out_link$ipar_linked # item parameters linked to anchor
#' out_link <- runLinking(data_asq, "FIXEDPAR")
#' out_link$ipar_linked # item parameters linked to anchor
#' }
#' @export

runLinking <- function(data, method, ...) {

  if (!inherits(data, "PROsetta_data")) {
    stop("data must be a 'PROsetta_data' class object")
  }
  if (is.null(data@anchor)) {
    stop("anchor must be supplied to perform scale linking")
  }
  if (!method %in% c("MM", "MS", "HB", "SL", "FIXEDPAR")) {
    stop(sprintf("unrecognized value in 'method': %s", method))
  }

  if (method == "FIXEDPAR") {
    do_fixedpar <- TRUE
  } else {
    do_fixedpar <- FALSE
  }

  calibration <- runCalibration(data, fixedpar = do_fixedpar, ...)

  ipar      <- mirt::coef(calibration, IRTpars = TRUE, simplify = TRUE)$items
  ni_all    <- nrow(ipar)
  ni_anchor <- nrow(data@anchor)
  max_cat   <- max(data@anchor$NCAT)

  id_new <- data.frame(New = 1:ni_all   , ID = data@itemmap[[data@item_id]])
  id_old <- data.frame(Old = 1:ni_anchor, ID = data@anchor[[data@item_id]])
  common <- merge(id_new, id_old, by = "ID", sort = FALSE)[c("New", "Old")]
  pars <- vector("list", 2)
  pars[[1]] <- ipar
  pars[[2]] <- data@anchor[c("a", paste0("cb", 1:(max_cat - 1)))]

  if (!do_fixedpar) {
    message(sprintf("now performing linear transformation to match anchor with %s method", method))
    pm_all    <- plink::as.poly.mod(ni_all   , "grm", 1:ni_all)
    pm_anchor <- plink::as.poly.mod(ni_anchor, "grm", 1:ni_anchor)
    ncat <- list(data@itemmap$NCAT, data@anchor$NCAT)
    plink_pars <- plink::as.irt.pars(
      pars, common, cat = ncat,
      list(pm_all, pm_anchor),
      grp.names = c("From", "To")
    )
    out <- plink::plink(plink_pars, rescale = method, base.grp = 2)
    out$constants <- out$link@constants[[method]]
    out$ipar_linked <- out$pars@pars$From
    out$ipar_anchor <- out$pars@pars$To
  } else {
    out <- list()
    out$constants <- NA
    out$ipar_linked <- pars[[1]]
    out$ipar_anchor <- pars[[2]]
  }

  out$method           <- method
  rownames(out$ipar_linked) <- id_new$ID
  rownames(out$ipar_anchor) <- id_old$ID
  colnames(out$ipar_linked) <- colnames(ipar)
  colnames(out$ipar_anchor) <- colnames(ipar)

  return(out)
}

#' Run Test Equating
#'
#' Performs equipercentile test equating between two scales. A concordance table is produced, mapping the observed raw scores from one scale to the scores from another scale.
#'
#' @param data A PROsetta_data object. See \code{\link{loadData}} for loading a dataset.
#' @param scale_from Numeric. The index of the scale in need of test equating.
#' @param scale_to Numeric. The index of the target scale to equate to. This and \code{scale_from} below both reference to the information stored in the \code{itemmap} slot of \code{Data} argument. The \code{scale_id} slot of \code{config} argument needs to be specified as the name of the varible containing the scale IDs in the \code{itemmap} slot.
#' @param type_to The type of score to use as the target scale frequency table. Accepts 'raw' (default) and 'tscore'. 'tscore' requires RSSS table to be supplied.
#' @param rsss If 'type_to' = 'tscore', the RSSS table to use to map each raw score level onto t-score. See \code{\link{runRSSS}}.
#' @param eq_type The type of equating to be passed onto \code{\link[equate]{equate}} in \href{https://CRAN.R-project.org/package=equate}{\code{equate}} package.
#' @param smooth The type of smoothing method to be passed onto \code{\link[equate]{presmoothing}} in \href{https://CRAN.R-project.org/package=equate}{\code{equate}} package.
#' @param degrees The degrees of smoothing to be passed onto \code{\link[equate]{presmoothing}}.
#' @param boot Logical. Performs bootstrapping if \code{TRUE}.
#' @param reps Numeric. The number of replications in bootsrapping.
#' @param ... Other arguments to pass onto \code{\link[equate]{equate}}.
#'
#' @return An \code{equate} object containing the test equating result. The printed summary statistics indicate the distributional properties of the two supplied scales and the equated scale. The rows titled \code{x} and \code{y} correspond to the scales specified in \code{scale_from} and \code{scale_to} respectively. The row titled \code{yx} corresponds to the \code{scale_from} scale transformed to \code{scale_to}. See \code{\link[equate]{equate}} for details.
#'
#' @examples
#' out_eq_raw <- runEquateObserved(data_asq,
#'   scale_to = 1, scale_from = 2,
#'   eq_type = "equipercentile", smooth = "loglinear"
#' )
#' out_eq_raw$concordance
#'
#' out_link <- runLinking(data_asq, method = "FIXEDPAR")
#' out_rsss <- runRSSS(data_asq, out_link)
#' out_eq_tscore <- runEquateObserved(data_asq,
#'   scale_to = 1, scale_from = 2,
#'   type_to = "tscore", rsss = out_rsss,
#'   eq_type = "equipercentile", smooth = "loglinear"
#' )
#' out_eq_tscore$concordance
#'
#' @export

runEquateObserved <- function(data, scale_from = 2, scale_to = 1, type_to = "raw", rsss = NULL, eq_type = "equipercentile", smooth = "loglinear", degrees = list(3, 1), boot = TRUE, reps = 100, ...) {

  if (!inherits(data, "PROsetta_data")) {
    stop("data must be a 'PROsetta_data' class object")
  }

  message("runEquateObserved requires complete data, attempting to remove cases")
  data <- getCompleteData(data)

  scale_id       <- data@itemmap[[data@scale_id]]
  scale_code     <- unique(scale_id)
  items_from     <- which(scale_id %in% scale_from) # Items that need to be equated
  items_to       <- which(scale_id %in% scale_to)   # Reference items
  itemnames      <- data@itemmap[[data@item_id]]
  itemnames_from <- itemnames[items_from]
  itemnames_to   <- itemnames[items_to]
  scores_from    <- rowSums(data@response[itemnames_from])
  scores_to      <- rowSums(data@response[itemnames_to])
  freq_from      <- equate::freqtab(scores_from)
  freq_to        <- equate::freqtab(scores_to)


  # scale_from

  if (smooth != "none") {
    message(sprintf("performing %s presmoothing on scale %i (scale_from) distribution", smooth, scale_from))
    freq_from <- equate::presmoothing(freq_from, smoothmethod = smooth, degrees = degrees)
  }


  # scale_to

  if (type_to == "tscore") {
    if (!is.null(rsss)) {
      message(sprintf("mapping scale %i (scale_to) raw scores to t-scores using supplied rsss", scale_to))
      tmp <- as.data.frame(freq_to)
      tmp <- merge(
        tmp, rsss[[as.character(scale_to)]],
        by.x = "total", by.y = sprintf("raw_%i", scale_to))
      tmp <- tmp[, c("t_score", "count")]
      freq_to <- equate::as.freqtab(tmp)
    } else {
      stop("type_to = 'tscore' requires rsss to be able to map raw scores to t-scores")
    }
  }
  if (smooth != "none") {
    message(sprintf("performing %s presmoothing on scale %i (scale_to) distribution", smooth, scale_to))
    freq_to   <- equate::presmoothing(freq_to  , smoothmethod = smooth, degrees = degrees)
  }

  score_stat     <- rbind(From = summary(freq_from), To = summary(freq_to))

  out <- equate::equate(freq_from, freq_to, type = eq_type, boot = boot, reps = reps, ...)

  names(out$concordance)[1:2] <- c(
    sprintf("raw_%i"  , scale_from),
    sprintf("equiv_%i", scale_to))
  return(out)

}

#' Run Crosswalk Table Generation
#'
#' Generates raw-score to standard-score crosswalk tables from supplied calibrated item parameters.
#'
#' @param data A \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#' @param ipar_linked An object returned from \code{\link{runCalibration}} or \code{\link{runLinking}}.
#' @param prior_mean Prior mean.
#' @param prior_sd Prior standard deviation.
#' @param min_theta The lower limit of theta grid.
#' @param max_theta The upper limit of theta grid.
#' @param inc The increment to use in theta grid.
#' @param min_score Minimum item score (0 or 1).
#'
#' @return A list containing crosswalk tables.
#'
#' @examples
#' out_link    <- runLinking(data_asq, method = "FIXEDPAR")
#' score_table <- runRSSS(data_asq, out_link)
#'
#' @export

runRSSS <- function(data, ipar_linked, prior_mean = 0.0, prior_sd = 1.0, min_theta = -4.0, max_theta = 4.0, inc = 0.01, min_score = 1) {

  if (!inherits(data, "PROsetta_data")) {
    stop("data must be a 'PROsetta_data' class object")
  }
  if (is.null(attr(class(ipar_linked), "package"))) {
    item_par <- ipar_linked$ipar_linked
  } else if (isS4(ipar_linked) && attr(class(ipar_linked), "package") == "mirt") {
    item_par <- mirt::coef(ipar_linked, IRTpars = TRUE, simplify = TRUE)$items
  }

  item_par_by_scale <- split(data.frame(item_par), data@itemmap[[data@scale_id]])
  n_scale <- length(item_par_by_scale)

  if (!all(min_score %in% c(0, 1))) {
    stop("min_score must contain only 0 or 1")
  }
  if (length(min_score) == 1) {
    if (n_scale > 1) {
      min_score <- rep(min_score, n_scale + 1)
    }
  } else if (length(min_score) != n_scale + 1) {
    stop(sprintf("length of min_score must be either 1 or %i", n_scale + 1))
  }


  rsss <- function(ipar, is_minscore_0) {

    theta_grid <- seq(min_theta, max_theta, inc)
    pp         <- prep_prob(ipar, "grm", theta_grid)

    ni   <- dim(ipar)[1]
    nq   <- length(theta_grid)
    ncat <- apply(ipar, 1, function(x) sum(!is.na(x)))

    min_raw_score <- 0                                 # minimum obtainable raw score
    max_raw_score <- sum(ncat) - ni                    # maximum obtainable raw score
    raw_score     <- min_raw_score:max_raw_score       # raw scores
    n_score       <- length(raw_score)                 # number of score levels
    inv_tcc       <- numeric(n_score)                  # initialize TCC scoring table
    lh            <- matrix(0, nq, n_score)            # initialize distribution of summed scores

    ncat_i    <- ncat[1]
    max_score <- 0
    lh[, 1:ncat_i] <- pp[, 1, 1:ncat_i]
    idx <- ncat_i

    for (i in 2:ni) {
      ncat_i    <- ncat[i]                # number of categories for item i
      max_score <- ncat_i - 1             # maximum score for item i
      score     <- 0:max_score            # score values for item i
      prob      <- pp[, i, 1:ncat_i]      # category probabilities for item i
      plh       <- matrix(0, nq, n_score) # place holder for lh
      for (k in 1:ncat_i) {
        for (h in 1:idx) {
          sco <- raw_score[h] + score[k]
          position <- which(raw_score == sco)
          plh[, position] <- plh[, position] + lh[, h] * prob[, k]
        }
      }
      idx <- idx + max_score
      lh <- plh
    }

    theta_score <- numeric(n_score) # score table for EAP
    theta_se    <- numeric(n_score) # SE for EAP

    prior       <- gen_prior(theta_grid, "normal", prior_mean, prior_sd)
    posterior   <- lh * prior
    den         <- colSums(posterior)
    den         <- matrix(rep(den, rep(nq, n_score)), nq, n_score)
    posterior   <- posterior / den

    for (j in 1:n_score) {
      theta_score[j] <- sum(posterior[, j] * theta_grid) / sum(posterior[, j])                   # EAP
      theta_se[j] <- sqrt(sum(posterior[, j] * (theta_grid - theta_score[j])^2) / sum(posterior[, j])) # EAP
    }

    if (!is_minscore_0) {
      raw_score <- raw_score + ni
    }

    t_score <- theta_score * 10 + 50
    t_se    <- theta_se * 10

    rsss_table <- data.frame(
      raw_sum     = raw_score,
      t_score     = t_score,
      t_se        = t_se,
      theta_score = theta_score,
      theta_se    = theta_se
    )

    return(rsss_table)
  }

  is_minscore_0 = F

  if (n_scale == 1) {
    score_table <- rsss(item_par, min_score == 0)
    return(score_table)
  } else if (n_scale > 1) {
    score_table <- vector(mode = "list", length = n_scale + 1)

    for (s in 1:(n_scale + 1)) {
      if (s != n_scale + 1) {
        ipar <- item_par_by_scale[[s]]
      } else {
        ipar <- item_par
      }
      score_table[[s]] <- rsss(ipar, min_score[s] == 0)
      colnames(score_table[[s]])[1] <- sprintf("raw_%i", s)
    }

    for (s in 1:(n_scale + 1)) {
      for (d in 1:(n_scale + 1)) {
        n_theta <- length(score_table[[s]]$theta_score)
        e_theta <- rep(NA, n_theta)
        if (d != n_scale + 1) {
          ipar <- item_par_by_scale[[d]]
        } else {
          ipar <- item_par
        }
        for (i in 1:n_theta) {
          e_theta[i] <- calc_escore(ipar, "grm", score_table[[s]]$theta_score[i], min_score[d] == 0)
        }
        if (d != n_scale + 1) {
          e_name <- sprintf("escore_%i", d)
        } else {
          e_name <- sprintf("escore_combined")
        }
        score_table[[s]][[e_name]] <- e_theta
      }
    }

    names(score_table) <- c(names(item_par_by_scale), "combined")

    return(score_table)
  }
}
