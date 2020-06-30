#' @include post_functions.R
NULL

#' @noRd
check_fp <- function(fp, f) {
  p <- f
  if (file.exists(p)) {
    return(list(path = normalizePath(p), exists = TRUE))
  }
  p <- file.path(fp, f)
  if (file.exists(p)) {
    return(list(path = normalizePath(p), exists = TRUE))
  }
  return(list(path = normalizePath(p), exists = FALSE))
}

#' @noRd
validate_data <- function(d) {
  if (!inherits(d, "PROsetta_data")) {
    stop("argument 'data': must be a 'PROsetta_data' class object")
  }
}

#' @noRd
get_col <- function(d, cn) {
  idx <- which(tolower(names(d)) == cn)
  return(d[, idx])
}

#' @noRd
gen_prior <- function(theta_grid, dist_type, prior_mean, prior_sd) {

  if (dist_type == "normal") {
    prior <- dnorm((theta_grid - prior_mean) / prior_sd)
  } else if (dist_type == "logistic") {
    num   <- exp((theta_grid - prior_mean) / prior_sd)
    denom <- (1 + num)^2
    prior <- num / denom
  } else if (dist_type == "unif") {
    prior <- rep(1, length(theta_grid))
  } else {
    stop(sprintf("argument 'dist_type': unrecognized value '%s'", dist_type))
  }

  return(prior)
}

#' @noRd
prep_prob <- function(ipar, model, theta_grid) {

  # returns theta * item * probability array

  ni      <- nrow(ipar)
  max_cat <- dim(ipar)[2]
  nq      <- length(theta_grid)

  par_disc <- ipar[, 'a']
  par_cb   <- ipar[, paste0("b", 1:(max_cat - 1))]

  ncat <- apply(par_cb, 1, function(x) sum(!is.na(x)) + 1)

  pp <- array(0, c(nq, ni, max_cat))

  if (model == "grm") {
    for (i in 1:ni) {
      ps <- matrix(0, nq, ncat[i] + 1)
      ps[, 1] <- 1
      ps[, ncat[i] + 1] <- 0
      for (k in 1:(ncat[i] - 1)) {
        ps[, k + 1] <- 1 / (1 + exp(-par_disc[i] * (theta_grid - par_cb[i, k])))
      }
      for (k in 1:ncat[i]) {
        pp[, i, k] <- ps[, k] - ps[, k + 1];
      }
    }
  } else if (model == "gpcm") {
    for (i in 1:ni) {
      cb <- unlist(par_cb[i, ])
      cb <- c(0, cb)
      zz <- matrix(0, nq, ncat[i])
      sdsum <- 0
      den <- rep(0, nq)

      for (k in 1:ncat[i]) {
        sdsum <- sdsum + cb[k]
        zz[, k] <- exp(par_disc[i] * (k * theta_grid - sdsum))
        den <- den + zz[, k]
      }
      for (k in 1:ncat[i]) {
        pp[, i, k] <- zz[, k] / den
      }
    }
  } else {
    stop(sprintf("argument 'model': unrecognized value '%s'", model))
  }

  return(pp)

}

#' @noRd
calc_escore = function(ipar, model, theta, is_minscore_0) {
  pp <- prep_prob(ipar, "grm", theta)[1, , ]
  ni <- dim(pp)[1]
  nc <- dim(pp)[2]
  w  <- matrix(rep(1:nc - 1, ni), ni, nc, byrow = TRUE)
  e  <- sum(pp * w)
  if (!is_minscore_0) {
    e <- e + ni
  }
  return(e)
}

#' @noRd
calc_eap = function(theta_grid, prior, pp, resp_data) {

  n  <- dim(resp_data)[1]
  ni <- dim(resp_data)[2]
  nq <- length(theta_grid)
  posterior <- matrix(rep(prior, n), n, nq, byrow = TRUE)

  for (i in 1:ni) {
    resp <- matrix(resp_data[, i], n, 1)
    prob <- t(pp[, i, resp])
    prob[is.na(prob)] <- 1.0
    posterior <- posterior*prob
  }

  theta_eap <- as.vector(
    posterior %*% theta_grid /
    apply(posterior, 1, sum)
  )

  t1 <- matrix(theta_grid, n, nq, byrow = TRUE)
  t2 <- matrix(theta_eap,  n, nq)

  theta_se <- as.vector(
    sqrt(
      rowSums(posterior * (t1 - t2)^2)/
        rowSums(posterior)
    ))

  return(data.frame(
    theta_eap = theta_eap,
    theta_se = theta_se))
}
