#' @include configPROsetta.R
NULL



#' Get complete data
#'
#' Perform casewise deletion of missing values.
#'
#' @param data A \code{\linkS4class{PROsetta_data}} object.
#'
#' @export

getCompleteData <- function(data) {
  if (!inherits(data, "PROsetta_data")) {
    stop("unrecognized object class: %s", class(data))
  }
  resp_with_missing_values <- apply(is.na(data@response), 1, any)
  n_resp <- sum(resp_with_missing_values)
  if (any(resp_with_missing_values)) {
    data@response <- data@response[!resp_with_missing_values, ]
    message(sprintf("filtered %s cases with one or more missing responses", n_resp))
  } else {
    message(sprintf("no cases were removed, all %i responses were complete", dim(data@response)[1]))
  }
  return(data)
}

#' Obtain EAP estimates
#'
#' Obtain EAP estimates
#'
#' @param data A \code{\linkS4class{PROsetta_data}} object.
#' @param ipar A data frame containing item parameters.
#' @param scale The index of the scale to use. Set to 'combined' to use the combined scale.
#' @param model The item model to use. Accepts 'grm' or 'gpcm'.
#' @param theta_grid The theta grid to use in calculating EAP estimates.
#' @param prior_dist The type of prior distribution. Accepts 'normal' or 'logistic'.
#' @param prior_mean Mean of the prior distribution.
#' @param prior_sd SD of the prior distribution.
#'
#' @export

getTheta <- function(
  data, ipar, scale = "combined", model = "grm",
  theta_grid = seq(-4, 4, .1),
  prior_dist = "normal",
  prior_mean = 0.0,
  prior_sd = 1.0) {

  resp_data <- getCompleteData(data)@response

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



#' Computed expected scores at thetas using item parameters
#'
#' Computed expected scores at thetas using item parameters
#'
#' @param ipar Item parameters
#' @param model Item model to use
#' @param theta Theta values
#' @param is_minscore_0 If TRUE the score begins from 0 instead of 1.
#'
#' @export

getEscore <- function(ipar, model, theta, is_minscore_0) {

  e <- rep(NA, length(theta))
  for (i in 1:length(theta)) {
    e[i] <- calc_escore(ipar, model, theta[i], is_minscore_0)
  }

  return(e)

}

#' Compute raw sum scores of the specified scale
#'
#' Compute raw sum scores of the specified scale
#'
#' @param data A \code{\linkS4class{PROsetta_data}} object.
#' @param scale_idx The index of the scacle to obtain the raw sum scores.
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
#' Compare two sets of scores.
#'
#' @param left Scores on the left side of comparison.
#' @param right Scores on the right side of comparison. This is subtracted from 'left'.
#' @param type Type of comparisons to include. Accepts 'corr', "mean', 'sd', 'rmsd'. Defaults to all four.
#'
#' @export

compareScores <- function(left, right, type = c("corr", "mean", "sd", "rmsd")) {

  out <- list()
  if ("corr" %in% type) {
    out$corr <- cor(left, right)
  }
  if ("mean" %in% type) {
    out$mean <- mean(left - right)
  }
  if ("sd" %in% type) {
    out$sd   <- sd(left - right)
  }
  if ("rmsd" %in% type) {
    out$rmsd <- sqrt(mean((left - right)**2))
  }

  return(as.data.frame(out))

}
