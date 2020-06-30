#' @include configPROsetta.R
NULL

#' Run Calibration
#'
#' \code{\link{runCalibration}} is a function to perform item calibration on the response data.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#' @param fixedpar if \code{TRUE} (default), perform fixed parameter calibration using anchor data.
#' @param ignore_nonconv if \code{TRUE}, return results even when calibration did not converge. Defaults to \code{FALSE}.
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
runCalibration <- function(data, fixedpar = FALSE, ignore_nonconv = FALSE, ...) {

  validate_data(data)

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
    message("performing free calibration of all items, ignoring anchor data", appendLF = TRUE)
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
#' \code{\link{runLinking}} is a function to obtain item parameters from the response data, and perform scale linking onto the metric of supplied anchor item parameters.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#' @param method the type of linking to perform. Accepts:
#' \itemize{
#'   \item{\code{MM} for mean-mean}
#'   \item{\code{MS} for mean-sigma}
#'   \item{\code{HB} for Haebara method}
#'   \item{\code{SL} for Stocking-Lord method}
#'   \item{\code{FIXEDPAR} for fixed parameter calibration}
#' }
#' Linear transformation methods are performed with \code{\link[plink:plink-methods]{plink}} in \href{https://CRAN.R-project.org/package=plink}{'plink'} package.
#'
#' @param ... additional arguments to pass onto \code{\link[mirt]{mirt}} in \href{https://CRAN.R-project.org/package=mirt}{'mirt'} package.
#'
#' @return \code{\link{runLinking}} returns a \code{\link{list}} containing the scale linking results.
#' \itemize{
#'   \item{\code{constants}} linear transformation constants. \code{NA} if \code{method} argument was \code{FIXEDPAR}.
#'   \item{\code{ipar_linked}} item parameters calibrated to the response data, and linked to the anchor item parameters.
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
runLinking <- function(data, method, ...) {

  validate_data(data)

  if (is.null(data@anchor)) {
    stop("argument 'data': @anchor must be supplied for runLinking()")
  }
  if (!method %in% c("MM", "MS", "HB", "SL", "FIXEDPAR")) {
    stop(sprintf("argument 'method': unrecognized value '%s' (accepts 'MM', 'MS', 'HB', 'SL', 'FIXEDPAR')", method))
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
  max_cat   <- max(get_col(data@anchor, "ncat"))

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
    ncat <- list(
      get_col(data@itemmap, "ncat"),
      get_col(data@anchor, "ncat")
    )
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

  out$method                <- method
  rownames(out$ipar_linked) <- id_new$ID
  rownames(out$ipar_anchor) <- id_old$ID
  colnames(out$ipar_linked) <- colnames(ipar)
  colnames(out$ipar_anchor) <- colnames(ipar)

  return(out)
}

#' Run Test Equating
#'
#' \code{\link{runEquateObserved}} is a function to perform equipercentile test equating between two scales. A concordance table is produced, mapping the observed raw scores from one scale to the scores from another scale.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#' @param scale_from the scale ID of the input scale. References to \code{itemmap} in \code{data} argument.
#' @param scale_to the scale ID of the target scale to equate to. References to \code{itemmap} in \code{data} argument.
#' @param type_to the type of score to use in the target scale frequency table. Accepts \code{raw}, \code{tscore}, and \code{theta}. \code{tscore} and \code{theta} require argument \code{rsss} to be supplied. (default = \code{raw})
#' @param rsss the RSSS table to use to map each raw score level onto a t-score or a theta. See \code{\link{runRSSS}}.
#' @param eq_type the type of equating to be passed onto \code{\link[equate]{equate}} in \href{https://CRAN.R-project.org/package=equate}{'equate'} package.
#' @param smooth the type of smoothing method to be passed onto \code{\link[equate]{presmoothing}} in \href{https://CRAN.R-project.org/package=equate}{'equate'} package.
#' @param degrees the degrees of smoothing to be passed onto \code{\link[equate]{presmoothing}}.
#' @param boot performs bootstrapping if \code{TRUE}.
#' @param reps the number of replications to perform in bootsrapping.
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
runEquateObserved <- function(data, scale_from = 2, scale_to = 1, type_to = "raw", rsss = NULL, eq_type = "equipercentile", smooth = "loglinear", degrees = list(3, 1), boot = TRUE, reps = 100, ...) {

  validate_data(data)

  message("runEquateObserved requires complete data, attempting to remove cases", appendLF = TRUE)
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
      tmp <- tmp[, c("tscore", "count")]
      freq_to <- equate::as.freqtab(tmp)
    } else {
      stop("argument 'type_to': 'tscore' requires argument 'rsss' to be supplied to be able to map raw scores to t-scores")
    }
  }
  if (type_to == "theta") {
    if (!is.null(rsss)) {
      message(sprintf("mapping scale %i (scale_to) raw scores to theta using supplied rsss", scale_to))
      tmp <- as.data.frame(freq_to)
      tmp <- merge(
        tmp, rsss[[as.character(scale_to)]],
        by.x = "total", by.y = sprintf("raw_%i", scale_to))
      tmp <- tmp[, c("theta", "count")]
      freq_to <- equate::as.freqtab(tmp)
    } else {
      stop("argument 'type_to': 'theta' requires argument 'rsss' to be supplied to be able to map raw scores to theta")
    }
  }
  if (smooth != "none") {
    message(sprintf("performing %s presmoothing on scale %i (scale_to) distribution", smooth, scale_to))
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

#' Run Crosswalk Table Generation
#'
#' \code{\link{runRSSS}} is a function to generate raw-score to standard-score crosswalk tables from supplied calibrated item parameters.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#' @param ipar_linked an object returned from \code{\link{runLinking}} or \code{\link{runCalibration}}.
#' @param prior_mean prior mean.
#' @param prior_sd prior standard deviation.
#' @param min_theta the lower limit of theta grid.
#' @param max_theta the upper limit of theta grid.
#' @param inc the increment to use in theta grid.
#' @param min_score minimum item score (0 or 1).
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
runRSSS <- function(data, ipar_linked, prior_mean = 0.0, prior_sd = 1.0, min_theta = -4.0, max_theta = 4.0, inc = 0.01, min_score = 1) {

  validate_data(data)

  if (is.null(attr(class(ipar_linked), "package"))) {
    item_par <- ipar_linked$ipar_linked
  } else if (isS4(ipar_linked) && attr(class(ipar_linked), "package") == "mirt") {
    item_par <- mirt::coef(ipar_linked, IRTpars = TRUE, simplify = TRUE)$items
  }

  item_par_by_scale <- split(data.frame(item_par), data@itemmap[[data@scale_id]])
  n_scale <- length(item_par_by_scale)

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

    theta       <- numeric(n_score) # score table for EAP
    theta_se    <- numeric(n_score) # SE for EAP

    prior       <- gen_prior(theta_grid, "normal", prior_mean, prior_sd)
    posterior   <- lh * prior
    den         <- colSums(posterior)
    den         <- matrix(rep(den, rep(nq, n_score)), nq, n_score)
    posterior   <- posterior / den

    for (j in 1:n_score) {
      theta[j] <- sum(posterior[, j] * theta_grid) / sum(posterior[, j])                         # EAP
      theta_se[j] <- sqrt(sum(posterior[, j] * (theta_grid - theta[j])^2) / sum(posterior[, j])) # EAP
    }

    if (!is_minscore_0) {
      raw_score <- raw_score + ni
    }

    tscore    <- round(theta    * 10 + 50, 1)
    tscore_se <- round(theta_se * 10, 1)

    rsss_table <- data.frame(
      sum_score   = raw_score,
      tscore      = tscore,
      tscore_se   = tscore_se,
      eap         = theta,
      eap_se      = theta_se
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
        n_theta <- length(score_table[[s]]$eap)
        e_theta <- rep(NA, n_theta)
        if (d != n_scale + 1) {
          ipar <- item_par_by_scale[[d]]
        } else {
          ipar <- item_par
        }
        for (i in 1:n_theta) {
          e_theta[i] <- calc_escore(ipar, "grm", score_table[[s]]$eap[i], min_score[d] == 0)
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
