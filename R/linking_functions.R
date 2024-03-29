#' @include loading_functions.R
NULL

#' Run Calibration
#'
#' \code{\link{runCalibration}} is a function for performing item parameter calibration on the response data.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#' @param dimensions the number of dimensions to use. Must be 1 or 2.
#' If 1, use one underlying dimension for all instruments combined.
#' If 2, use each dimension separately for the anchor instrument and the developing instrument. Covariance between dimensions is freely estimated. (default = \code{1})
#' @param fix_method the type of constraints to impose. (default = \code{free})
#' \itemize{
#'   \item{\code{item} for fixed parameter calibration using anchor item parameters}
#'   \item{\code{theta} for using the mean and the variance obtained from a unidimensional calibration of anchor items}
#'   \item{\code{free} for free calibration}
#' }
#' @param fixedpar this argument exists for backward compatibility. \code{TRUE} is equivalent to \code{fix_method = "item"}, and \code{FALSE} is equivalent to \code{fix_method = "free"}.
#' @param ignore_nonconv if \code{TRUE}, return results even when calibration does not converge. If \code{FALSE}, raise an error when calibration does not converge. (default = \code{FALSE})
#' @param verbose if \code{TRUE}, print status messages. (default = \code{FALSE})
#' @param ... additional arguments to pass onto \code{\link[mirt]{mirt}} in \href{https://CRAN.R-project.org/package=mirt}{'mirt'} package.
#'
#' @return \code{\link{runCalibration}} returns a \code{\linkS4class{SingleGroupClass}} object containing item calibration results.
#'
#' This object can be used in \code{\link[mirt:coef-method]{coef}}, \code{\link[mirt]{itemfit}}, \code{\link[mirt]{itemplot}} in \href{https://CRAN.R-project.org/package=mirt}{'mirt'} package to extract wanted information.
#'
#' @examples
#' \dontrun{
#' out_calib <- runCalibration(data_asq) # errors
#' }
#' \donttest{
#' out_calib <- runCalibration(data_asq, technical = list(NCYCLES = 1000))
#'
#' mirt::coef(out_calib, IRTpars = TRUE, simplify = TRUE)
#' mirt::itemfit(out_calib, empirical.plot = 1)
#' mirt::itemplot(out_calib, item = 1, type = "info")
#' mirt::itemfit(out_calib, "S_X2", na.rm = TRUE)
#' }
#' @export
runCalibration <- function(
  data, dimensions = 1,
  fix_method = "free", fixedpar = NULL,
  ignore_nonconv = FALSE,
  verbose = FALSE,
  ...) {

  if (!missing("fixedpar")) {
    if (fixedpar == TRUE) {
      fix_method <- "item"
    }
    if (fixedpar == FALSE) {
      fix_method <- "free"
    }
  }

  validateData(data)

  resp_data   <- getResponse(data)
  ni          <- dim(resp_data)[2]
  n_obs       <- dim(resp_data)[1]
  printLog(
    "validation",
    sprintf("response data has %s items * %s observations", ni, n_obs),
    verbose
  )

  if (toupper(fix_method) == "ITEM") {

    printLog(
      "config",
      sprintf(
        "performing %sD fixed parameter calibration, using anchor data",
        dimensions
      ),
      verbose
    )

    bound_cov   <- FALSE
    layout      <- makeParameterLayout(data, dimensions, bound_cov)
    layout      <- applyConstraintsToLayout(layout, data, verbose)
    model_specs <- makeCalibrationModel(data, dimensions, bound_cov)
    item_models <- data@itemmap[, data@model_id]
    item_models <- sanitizeItemModel(item_models, "mirt")
    calibration <- mirt::mirt(resp_data, model_specs, itemtype = item_models, pars = layout, ...)

  } else if (toupper(fix_method) == "THETA") {

    # used for calibrated projection
    # not used in 1D cases

    anchor_dim <- getAnchorDimension(data)
    printLog(
      "CPFIXEDDIM",
      sprintf(
        "first obtain mean(theta_%s) and var(theta_%s)",
        anchor_dim, anchor_dim
      ),
      verbose
    )

    # Step 1. Perform 1D calibration on anchor data only, constraining item parameters to anchor values
    # The goal of this step is to obtain the latent mean and SD of the anchor dimension
    data_anchor <- data
    anchor_dim  <- getAnchorDimension(data)
    data_anchor@response <- getResponse(data_anchor, scale_id = anchor_dim, person_id = TRUE)
    data_anchor@itemmap  <- subset(
      data_anchor@itemmap,
      data_anchor@itemmap$item_id %in% getItemNames(data_anchor, scale_id = anchor_dim)
    )
    linked_parameters_1d <- runLinking(data_anchor, method = "FIXEDPAR")

    printLog(
      "CPFIXEDDIM",
      sprintf("mean(theta_%s) : %s", anchor_dim, linked_parameters_1d$mu_sigma$mu),
      verbose
    )
    printLog(
      "CPFIXEDDIM",
      sprintf("var(theta_%s)  : %s", anchor_dim, linked_parameters_1d$mu_sigma$sigma),
      verbose
    )

    # Step 2. Constrain anchor dimension using 1D results
    layout <- makeParameterLayout(data, dimensions, bound_cov = FALSE)

    idx_mean <- which(
      layout$class == "GroupPars" &
      layout$name == sprintf("MEAN_%s", anchor_dim)
    )
    layout[idx_mean, ]$value <- linked_parameters_1d$mu_sigma$mu
    layout[idx_mean, ]$est   <- FALSE
    idx_var <- which(
      layout$class == "GroupPars" &
      layout$name == sprintf("COV_%s%s", anchor_dim, anchor_dim)
    )
    layout[idx_var, ]$value  <- linked_parameters_1d$mu_sigma$sigma
    layout[idx_var, ]$est    <- FALSE

    printLog(
      "CPFIXEDDIM",
      sprintf(
        "mean(theta_%s) and var(theta_%s) applied as constraints",
        anchor_dim, anchor_dim
      ),
      verbose
    )

    # Step 3. Fit a 2D model
    printLog(
      "CPFIXEDDIM",
      sprintf(
        "performing %sD free calibration of all items, using theta constraints",
        dimensions
      ),
      verbose
    )
    model_specs <- makeCalibrationModel(data, dimensions, bound_cov = FALSE)
    calibration <- mirt::mirt(resp_data, model_specs, itemtype = "graded", pars = layout, ...)

  } else if (toupper(fix_method) == "FREE") {

    printLog(
      "config",
      sprintf("performing %sD free calibration of all items, ignoring anchor data", dimensions),
      verbose
    )

    # Free calibration uses standardized factors
    # so it makes sense to bound covariance (which is just correlation here) to be below 1
    bound_cov   <- TRUE
    layout      <- makeParameterLayout(data, dimensions, bound_cov)
    model_specs <- makeCalibrationModel(data, dimensions, bound_cov)
    item_models <- data@itemmap[, data@model_id]
    item_models <- sanitizeItemModel(item_models, "mirt")
    calibration <- mirt::mirt(resp_data, model_specs, itemtype = item_models, pars = layout, ...)

  }

  if (calibration@OptimInfo$iter == calibration@Options$NCYCLES) {
    msg <- sprintf("calibration did not converge: increase iteration limit by adjusting the 'technical' argument, e.g., technical = list(NCYCLES = %i)", calibration@Options$NCYCLES + 500)
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
#' \code{\link{runLinking}} is a function for obtaining item parameters from the response data in the metric of supplied anchor item parameters.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#' @param method the linking method to use. Accepts:
#' \itemize{
#'   \item{\code{MM} for mean-mean method}
#'   \item{\code{MS} for mean-sigma method}
#'   \item{\code{HB} for Haebara method}
#'   \item{\code{SL} for Stocking-Lord method}
#'   \item{\code{FIXEDPAR} for fixed parameter calibration}
#'   \item{\code{CP} for calibrated projection using fixed parameter calibration on the anchor dimension}
#'   \item{\code{CPLA} for linear approximation of calibrated projection. This is identical to 'CP' but uses approximation in \code{\link{runRSSS}}}
#'   \item{\code{CPFIXEDDIM} for calibrated projection using mean and variance constraints on the anchor dimension}
#' }
#' Linear transformation methods (i.e., MM, MS, HB, SL) are performed with \code{\link[plink:plink-methods]{plink}} in \href{https://CRAN.R-project.org/package=plink}{'plink'} package.
#'
#' @param verbose if \code{TRUE}, print status messages. (default = \code{FALSE})
#' @param ... additional arguments to pass onto \code{\link[mirt]{mirt}} in \href{https://CRAN.R-project.org/package=mirt}{'mirt'} package.
#'
#' @return \code{\link{runLinking}} returns a \code{\link{list}} containing the scale linking results.
#' \itemize{
#'   \item{\code{constants}} linear transformation constants. Only available when linear transformation methods were used (i.e., MM, MS, HB, SL).
#'   \item{\code{ipar_linked}} item parameters calibrated to the response data, and linked to the metric of anchor item parameters.
#'   \item{\code{ipar_anchor}} anchor item parameters used in linking.
#' }
#' @examples
#' \donttest{
#' out_link <- runLinking(data_asq, "SL", technical = list(NCYCLES = 1000))
#' out_link$constants   # transformation constants
#' out_link$ipar_linked # item parameters linked to anchor
#' out_link <- runLinking(data_asq, "FIXEDPAR")
#' out_link$ipar_linked # item parameters linked to anchor
#' }
#' @export
runLinking <- function(data, method, verbose = FALSE, ...) {

  validateData(data)

  if (is.null(data@anchor)) {
    stop("argument 'data': @anchor must be supplied for runLinking()")
  }
  if (!method %in% c("MM", "MS", "HB", "SL", "FIXEDPAR", "CP", "CPLA", "CPFIXEDDIM")) {
    stop(sprintf("argument 'method': unrecognized value '%s' (accepts 'MM', 'MS', 'HB', 'SL', 'FIXEDPAR', 'CP', 'CPLA', 'CPFIXEDDIM')", method))
  }

  if (method %in% c("MM", "MS", "HB", "SL")) {
    dimensions <- 1
    fix_method <- "free"
  }
  if (method == "FIXEDPAR") {
    dimensions <- 1
    fix_method <- "item"
  }
  if (method %in% c("CP", "CPLA")) {
    dimensions <- 2
    fix_method <- "item"
  }
  if (method == "CPFIXEDDIM") {
    dimensions <- 2
    fix_method <- "theta"
  }

  calibration <- runCalibration(data, dimensions = dimensions, fix_method = fix_method, verbose = verbose, ...)

  if (dimensions == 1) {

    fit       <- mirt::coef(calibration, IRTpars = TRUE, simplify = TRUE)
    ipar      <- fit$items
    ni_all    <- nrow(ipar)
    ni_anchor <- nrow(data@anchor)
    ipar_anchor     <- extractAnchorParameters(data, as = "AB")
    n_cats_anchor   <- detectNCategories(ipar_anchor)
    max_cats_anchor <- max(n_cats_anchor)
    id_new <- data.frame(New = 1:ni_all   , ID = data@itemmap[[data@item_id]])
    id_old <- data.frame(Old = 1:ni_anchor, ID = data@anchor[[data@item_id]])
    common <- merge(id_new, id_old, by = "ID", sort = FALSE)[c("New", "Old")]

    if (method %in% c("MM", "MS", "HB", "SL")) {

      printLog(
        "metric",
        "applying linear transformation on item parameters to match the metric of anchor data parameters",
        verbose
      )
      printLog(
        "metric",
        sprintf("linear transformation method is %s", method),
        verbose
      )

      pm_all    <- make_plink_poly_mod(data@itemmap[, data@model_id])
      pm_anchor <- make_plink_poly_mod(data@anchor[, data@model_id])
      n_cats <- list(
        getColumn(data@itemmap, "ncat"),
        n_cats_anchor
      )

      pars <- vector("list", 2)
      pars[[1]] <- ipar
      pars[[2]] <- data@anchor[c("a", paste0("cb", 1:(max_cats_anchor - 1)))]

      plink_pars <- plink::as.irt.pars(
        pars, common, cat = n_cats,
        list(pm_all, pm_anchor),
        grp.names = c("From", "To")
      )

      o <- plink::plink(plink_pars, rescale = method, base.grp = 2)
      o$constants <- o$link@constants[[method]]
      o$ipar_linked <- extractLinkedParameters(o, as = "AB", data)
      o$ipar_anchor <- extractAnchorParameters(data, as = "AB")

    }

    if (method == "FIXEDPAR") {

      o <- list()
      o$constants <- NA
      o$ipar_linked <- extractLinkedParameters(calibration, as = "AB", data)
      o$ipar_anchor <- extractAnchorParameters(data, as = "AB")
      o$mu_sigma    <- extractMuSigma(calibration)

    }

    o$method <- method

    return(o)

  }

  if (dimensions == 2) {

    o <- list()
    o$constants   <- NA
    o$ipar_linked <- extractLinkedParameters(calibration, as = "AD", data)
    o$ipar_anchor <- extractAnchorParameters(data, as = "AD")
    o$mu_sigma    <- extractMuSigma(calibration)
    o$method      <- method

    return(o)

  }

}

#' Run Test Equating
#'
#' \code{\link{runEquateObserved}} is a function for performing equipercentile equating between two scales.
#' \code{\link{runEquateObserved}} also produces a concordance table, mapping the observed raw scores from one scale to the scores from another scale.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#' @param scale_from the scale ID of the input scale. References to \code{itemmap} in \code{data} argument. (default = \code{2})
#' @param scale_to the scale ID of the target scale to equate to. References to \code{itemmap} in \code{data} argument. (default = \code{1})
#' @param type_to the type of score to use in the target scale frequency table. Accepts \code{raw}, \code{tscore}, and \code{theta}. \code{tscore} and \code{theta} require argument \code{rsss} to be supplied. (default = \code{raw})
#' @param rsss the RSSS table to use to map each raw score level onto a t-score or a theta. See \code{\link{runRSSS}}.
#' @param eq_type the type of equating to be passed onto \code{\link[equate]{equate}} in \href{https://CRAN.R-project.org/package=equate}{'equate'} package. (default = \code{equipercentile})
#' @param smooth the type of smoothing method to be passed onto \code{\link[equate]{presmoothing}} in \href{https://CRAN.R-project.org/package=equate}{'equate'} package. (default = \code{loglinear})
#' @param degrees the degrees of smoothing to be passed onto \code{\link[equate]{presmoothing}}. (default = \code{list(3, 1)})
#' @param boot performs bootstrapping if \code{TRUE}. (default = \code{TRUE})
#' @param reps the number of replications to perform in bootstrapping. (default = \code{100})
#' @param verbose if \code{TRUE}, print status messages. (default = \code{FALSE})
#' @param ... other arguments to pass onto \code{\link[equate]{equate}}.
#'
#' @return \code{\link{runEquateObserved}} returns an \code{\link{equate}} object containing the test equating result.
#'
#' The printed summary statistics indicate the distributional properties of the two supplied scales and the equated scale.
#' \itemize{
#'   \item{\code{x}} corresponds to \code{scale_from}.
#'   \item{\code{y}} corresponds to \code{scale_to}.
#'   \item{\code{yx}} corresponds to \code{scale_from} after equating to \code{scale_to}.
#' }
#' See \code{\link[equate]{equate}} for details.
#'
#' The concordance table is stored in \code{concordance} slot.
#'
#' @examples
#' out_eq_raw <- runEquateObserved(data_asq,
#'   scale_to = 1, scale_from = 2,
#'   eq_type = "equipercentile", smooth = "loglinear"
#' )
#' out_eq_raw$concordance
#'
#' \donttest{
#' out_link <- runLinking(data_asq, method = "FIXEDPAR")
#' out_rsss <- runRSSS(data_asq, out_link)
#' out_eq_tscore <- runEquateObserved(data_asq,
#'   scale_to = 1, scale_from = 2,
#'   type_to = "tscore", rsss = out_rsss,
#'   eq_type = "equipercentile", smooth = "loglinear"
#' )
#' out_eq_tscore$concordance
#' }
#' @export
runEquateObserved <- function(
  data,
  scale_from = 2, scale_to = 1, type_to = "raw",
  rsss = NULL, eq_type = "equipercentile",
  smooth = "loglinear", degrees = list(3, 1),
  boot = TRUE, reps = 100,
  verbose = FALSE, ...) {

  validateData(data)

  printLog(
    "sanitize",
    "runEquateObserved() requires complete data, attempting to remove incomplete cases",
    verbose
  )
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
    printLog(
      "smooth",
      sprintf("performing %s presmoothing on scale %i (scale_from) distribution", smooth, scale_from),
      verbose
    )
    freq_from <- equate::presmoothing(freq_from, smoothmethod = smooth, degrees = degrees)
  }

  # scale_to

  if (type_to == "tscore") {
    if (!is.null(rsss)) {
      printLog(
        "map",
        sprintf("mapping scale %i (scale_to) raw scores to t-scores using supplied rsss", scale_to),
        verbose
      )
      tmp <- as.data.frame(freq_to)
      tmp <- merge(
        tmp, rsss[[as.character(scale_to)]],
        by.x = "total", by.y = sprintf("raw_%i", scale_to))
      tmp <- tmp[, c("tscore", "count")]
      freq_to <- equate::as.freqtab(tmp)
    } else {
      stop("argument 'type_to': 'tscore' requires argument 'rsss' to be supplied to be able to map raw scores to t-scores")
    }
  }
  if (type_to == "theta") {
    if (!is.null(rsss)) {
      printLog(
        "map",
        sprintf("mapping scale %i (scale_to) raw scores to theta using supplied rsss", scale_to),
        verbose
      )
      tmp <- as.data.frame(freq_to)
      tmp <- merge(
        tmp, rsss[[as.character(scale_to)]],
        by.x = "total", by.y = sprintf("raw_%i", scale_to))
      tmp <- tmp[, c("eap", "count")]
      freq_to <- equate::as.freqtab(tmp)
    } else {
      stop("argument 'type_to': 'theta' requires argument 'rsss' to be supplied to be able to map raw scores to theta")
    }
  }
  if (smooth != "none") {
    printLog(
      "smooth",
      sprintf("performing %s presmoothing on scale %i (scale_to) distribution", smooth, scale_to),
      verbose
    )
    freq_to   <- equate::presmoothing(freq_to  , smoothmethod = smooth, degrees = degrees)
  }

  score_stat <- rbind(From = summary(freq_from), To = summary(freq_to))

  out <- equate::equate(freq_from, freq_to, type = eq_type, boot = boot, reps = reps, ...)

  names(out$concordance)[1:4] <- c(
    sprintf("raw_%i"       , scale_from),
    sprintf("%s_%i"        , type_to, scale_to),
    sprintf("%s_%i_se"     , type_to, scale_to),
    sprintf("%s_%i_se_boot", type_to, scale_to)
  )
  return(out)

}

#' Compute Crosswalk Tables
#'
#' \code{\link{runRSSS}} is a function for generating raw-score to standard-score crosswalk tables from supplied calibrated item parameters.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#' @param ipar_linked an object returned from \code{\link{runLinking}} or \code{\link{runCalibration}}.
#' @param prior_mean prior mean. (default = \code{0.0})
#' @param prior_sd prior standard deviation. (default = \code{1.0})
#' @param min_theta the lower limit of theta quadratures for numerical integration. (default = \code{-4})
#' @param max_theta the upper limit of theta quadratures for numerical integration. (default = \code{4})
#' @param inc the increment between theta quadratures for numerical integration. (default = \code{0.05})
#' @param min_score minimum item score (0 or 1) for each scale (1, 2, and combined). If a single value is supplied, the value is applied to all scales. (default = \code{1})
#'
#' @return \code{\link{runRSSS}} returns a \code{\link{list}} containing crosswalk tables.
#'
#' @examples
#' \donttest{
#' out_link    <- runLinking(data_asq, method = "FIXEDPAR")
#' score_table <- runRSSS(data_asq, out_link)
#' }
#'
#' @export
runRSSS <- function(
  data, ipar_linked, prior_mean = 0.0, prior_sd = 1.0,
  min_theta = -4.0, max_theta = 4.0, inc = 0.05, min_score = 1
) {

  validateData(data)

  if (is.null(attr(class(ipar_linked), "package"))) {

    item_par    <- ipar_linked$ipar_linked
    mu_sigma    <- ipar_linked$mu_sigma
    link_method <- ipar_linked$method

  } else if (isS4(ipar_linked) && attr(class(ipar_linked), "package") == "mirt") {

    item_par    <- mirt::coef(ipar_linked, IRTpars = FALSE, simplify = TRUE)$items
    mu_sigma    <- extractMuSigma(ipar_linked)
    link_method <- "FREE"

  }

  dimensions <- detectDimensions(item_par)
  ipar_type  <- detectParameterization(item_par)

  if (dimensions == 1 & ipar_type == "ad") {
    item_par <- convertADtoAB(item_par)
  }
  if (dimensions == 2 & ipar_type == "ab") {
    item_par <- convertABtoAD(item_par)
  }

  if (link_method == "CPLA") {
    item_par[, 1] <- rowSums(item_par[, 1:dimensions])
    item_par <- item_par[, -2]
    item_par <- convertADtoAB(item_par)
    dimensions <- 1
  }

  if (dimensions == 1) {
    prior_mu_sigma <- list()
    prior_mu_sigma$mu    <- 0
    prior_mu_sigma$sigma <- matrix(1, 1, 1)
  }
  if (dimensions == 2) {
    prior_mu_sigma <- mu_sigma
  }

  item_par_by_scale <- split(data.frame(item_par), data@itemmap[[data@scale_id]])
  n_scale <- length(item_par_by_scale)
  item_par_by_scale$combined <- item_par

  if (!all(min_score %in% c(0, 1))) {
    stop("argument 'min_score': must contain only 0 or 1")
  }
  if (length(min_score) == 1) {
    if (n_scale > 1) {
      min_score <- rep(min_score, n_scale + 1)
    }
  } else if (length(min_score) != n_scale + 1) {
    stop(sprintf("argument 'min_score': length(min_score) must be either 1 or %i", n_scale + 1))
  }

  theta_grid <- getThetaGrid(dimensions, min_theta, max_theta, inc)

  # the last item_par_by_scale is the combined scale

  if (n_scale == 1) {
    score_table <- getRSSS(item_par_by_scale[[n_scale + 1]], data@model_id, theta_grid, min_score == 0, prior_mu_sigma)
    return(score_table)
  } else if (n_scale > 1) {
    score_table <- vector(mode = "list", length = n_scale + 1)

    for (s in 1:(n_scale + 1)) {
      score_table[[s]] <- getRSSS(item_par_by_scale[[s]], data@model_id, theta_grid, min_score[s] == 0, prior_mu_sigma)
      colnames(score_table[[s]])[1] <- sprintf("raw_%i", s)
    }

    names(score_table) <- names(item_par_by_scale)

    if (dimensions == 1 & link_method != "CPLA") {
      score_table <- appendEscore(score_table, n_scale, item_par_by_scale, data@model_id, min_score)
    }
    if (dimensions == 1 & link_method == "CPLA") {
      score_table <- appendCPLA(score_table, n_scale, ipar_linked$mu_sigma)
    }

    return(score_table)

  }
}
