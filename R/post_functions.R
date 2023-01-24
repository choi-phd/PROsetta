#' @include loading_functions.R
NULL

#' Obtain theta estimates
#'
#' \code{\link{getTheta}} is a helper function for obtaining theta estimates.
#' Estimates are obtained using an \emph{expected a posteriori} (EAP) method.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object.
#' @param ipar a \code{\link{data.frame}} containing item parameters.
#' @param scale the index of the scale to use. \code{combined} refers to the combined scale. (default = \code{combined})
#' @param model the item model to use. Accepts \code{grm} or \code{gpcm}. (default = \code{grm})
#' @param theta_grid the theta grid to use for numerical integration. (default = \code{seq(-4, 4, .1)})
#' @param prior_dist the type of prior distribution. Accepts \code{normal} or \code{logistic}. (default = \code{normal})
#' @param prior_mean mean of the prior distribution. (default = \code{0.0})
#' @param prior_sd SD of the prior distribution. (default = \code{1.0})
#'
#' @return \code{\link{getTheta}} returns a \code{\link{list}} containing EAP estimates.
#'
#' @examples
#' x <- runLinking(data_asq, method = "FIXEDPAR")
#'
#' o <- getTheta(data_asq, x$ipar_linked, scale = 1)
#' o$theta
#' o$item_idx
#'
#' o <- getTheta(data_asq, x$ipar_linked, scale = 2)
#' o$theta
#' o$item_idx
#'
#' o <- getTheta(data_asq, x$ipar_linked, scale = "combined")
#' o$theta
#' o$item_idx
#'
#' @export
getTheta <- function(
  data, ipar, scale = "combined", model = "grm",
  theta_grid = seq(-4, 4, .1),
  prior_dist = "normal",
  prior_mean = 0.0,
  prior_sd = 1.0
) {

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

  if (is.vector(theta_grid)) {
    theta_grid <- as.matrix(theta_grid)
  }
  prior_mu_sigma       <- list()
  prior_mu_sigma$mu    <- prior_mean
  prior_mu_sigma$sigma <- prior_sd ** 2

  prior <- generatePriorDensity(theta_grid, prior_dist, prior_mu_sigma)
  pp    <- computeResponseProbability(ipar, model, theta_grid)
  eap   <- getEAP(theta_grid, prior, pp, resp_data)
  eap   <- cbind(person_id, eap)

	out <- list()
	out$theta    <- eap
	out$item_idx <- item_idx
	out$ipar     <- ipar

	return(out)

}

#' Calculate expected scores at theta
#'
#' \code{\link{getEscore}} is a helper function for obtaining expected scores at supplied thetas.
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
    e[i] <- getEscoreTheta(ipar, model, theta[i], is_minscore_0)
  }
  return(e)

}

#' Calculate raw sum scores of a scale
#'
#' \code{\link{getScaleSum}} is a helper function for calculating instrument-wise raw sum scores from response data.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object.
#' @param scale_idx the instrument index to obtain the raw sum scores.
#'
#' @examples
#' getScaleSum(data_asq, 1)
#' getScaleSum(data_asq, 2)
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
#' \code{\link{compareScores}} is a helper function for comparing two sets of scores.
#'
#' @param left scores on the left side of comparison.
#' @param right scores on the right side of comparison. This is subtracted from 'left'.
#' @param type type of comparisons to include. Accepts \code{corr}, \code{mean}, \code{sd}, \code{rmsd}, \code{mad}. Defaults to all types.
#'
#' @return \code{\link{compareScores}} returns a \code{\link{data.frame}} containing the comparison results.
#'
#' @examples
#' set.seed(1)
#' true_theta <- rnorm(100)
#' theta_est <- true_theta + rnorm(100, 0, 0.3)
#' compareScores(theta_est, true_theta)
#'
#' @export
compareScores <- function(
  left, right, type = c("corr", "mean", "sd", "rmsd", "mad")
) {

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
