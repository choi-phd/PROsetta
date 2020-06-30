#' @include configPROsetta.R
NULL

#' Get complete data
#'
#' \code{\link{getCompleteData}} is a helper function to perform casewise deletion of missing values.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object.
#' @param scale the index of the scale to perform casewise deletion. Leave empty or set to "combined" to perform on all scales.
#'
#' @export
getCompleteData <- function(data, scale = NULL) {

  validate_data(data)

  if (is.null(scale)) {
    scale <- "combined"
  }

  if (scale == "combined") {
    items <- data@itemmap[[data@item_id]]
    scale_text <- sprintf("all scales")
  } else {
    idx   <- data@itemmap[[data@scale_id]] == scale
    items <- data@itemmap[[data@item_id]][idx]
    scale_text <- sprintf("Scale %s", scale)
  }

  resp_with_missing_values <- apply(is.na(data@response[, items]), 1, any)
  n_resp <- sum(resp_with_missing_values)

  if (any(resp_with_missing_values)) {
    data@response <- data@response[!resp_with_missing_values, ]
    message(sprintf("getCompleteData: filtered %s cases with missing responses in %s", n_resp, scale_text))
  } else {
    message(sprintf("getCompleteData: no cases were removed, all %i responses are complete in %s", dim(data@response)[1], scale_text))
  }
  return(data)
}

#' Obtain EAP estimates
#'
#' \code{\link{getTheta}} is a helper function to calculate EAP estimates.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object.
#' @param ipar a \code{\link{data.frame}} containing item parameters.
#' @param scale the index of the scale to use. Set to 'combined' to use the combined scale.
#' @param model the item model to use. Accepts 'grm' or 'gpcm'.
#' @param theta_grid the theta grid to use in calculating EAP estimates.
#' @param prior_dist the type of prior distribution. Accepts '\code{normal}' or '\code{logistic}'.
#' @param prior_mean mean of the prior distribution.
#' @param prior_sd SD of the prior distribution.
#'
#' @return \code{\link{getTheta}} returns a \code{\link{list}} containing EAP estimates.
#'
#' @export
getTheta <- function(
  data, ipar, scale = "combined", model = "grm",
  theta_grid = seq(-4, 4, .1),
  prior_dist = "normal",
  prior_mean = 0.0,
  prior_sd = 1.0) {

  resp_data <- getCompleteData(data, scale)@response

  if (scale != "combined") {
    idx      <- data@itemmap[[data@scale_id]] == scale
    item_map <- subset(data@itemmap, idx)
    item_idx <- item_map[[data@item_id]]
  } else {
    item_map <- data@itemmap
    item_idx <- item_map[[data@item_id]]
  }

  person_id <- resp_data[data@person_id]
  resp_data <- resp_data[item_idx]
  ipar      <- ipar[item_idx, ]

  prior <- gen_prior(theta_grid, prior_dist, prior_mean, prior_sd)
  pp    <- prep_prob(ipar, model, theta_grid)
  eap   <- calc_eap(theta_grid, prior, pp, resp_data)
  eap   <- cbind(person_id, eap)


	out <- list()
	out$theta    <- eap
	out$item_idx <- item_idx
	out$ipar     <- ipar

	return(out)

}

#' Calculate expected scores at theta
#'
#' \code{\link{getEscore}} is a helper function to calculate expected scores at supplied thetas.
#'
#' @param ipar item parameters.
#' @param model item model to use.
#' @param theta theta values.
#' @param is_minscore_0 if \code{TRUE} the score begins from 0 instead of 1.
#'
#' @return \code{\link{getEscore}} returns a vector of expected scores.
#'
#' @export
getEscore <- function(ipar, model, theta, is_minscore_0) {

  e <- rep(NA, length(theta))
  for (i in 1:length(theta)) {
    e[i] <- calc_escore(ipar, model, theta[i], is_minscore_0)
  }
  return(e)

}

#' Calculate raw sum scores of a scale
#'
#' \code{\link{getScaleSum}} is a helper function to calculate raw sum scores of a scale.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object.
#' @param scale_idx the index of the scale to obtain the raw sum scores.
#'
#' @export
getScaleSum <- function(data, scale_idx) {
  person_id <- data@response[[data@person_id]]
  item_id <- subset(data@itemmap[[data@item_id]], (data@itemmap[[data@scale_id]] == scale_idx))
  raw_sum <- rowSums(data@response[item_id])
  raw_sum <- data.frame(person_id, raw_sum)
  colnames(raw_sum) <- c(data@person_id, sprintf("raw_%i", scale_idx))
  return(raw_sum)
}

#' Compare two sets of scores
#'
#' \code{\link{compareScores}} is a helper function to compare two sets of scores.
#'
#' @param left scores on the left side of comparison.
#' @param right scores on the right side of comparison. This is subtracted from 'left'.
#' @param type type of comparisons to include. Accepts '\code{corr}', "\code{mean}', '\code{sd}', '\code{rmsd}'. Defaults to all four.
#'
#' @return \code{\link{compareScores}} returns a \code{\link{data.frame}} containing the comparison results.
#'
#' @export
compareScores <- function(left, right, type = c("corr", "mean", "sd", "rmsd", "mad")) {

  out <- list()
  if ("corr" %in% type) {
    out$corr <- cor(left, right)
  }
  if ("mean" %in% type) {
    out$mean <- mean(left - right)
  }
  if ("sd" %in% type) {
    out$sd <- sqrt(mean((left - right)**2) - mean(left - right)**2)
  }
  if ("rmsd" %in% type) {
    out$rmsd <- sqrt(mean((left - right)**2))
  }
  if ("mad" %in% type) {
    out$mad <- mean(abs(left - right))
  }

  return(as.data.frame(out))

}
