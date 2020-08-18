#' @include post_functions.R
NULL

#' @noRd
checkFilePath <- function(fp, f) {
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
validateData <- function(d) {
  if (!inherits(d, "PROsetta_data")) {
    stop("argument 'data': must be a 'PROsetta_data' class object")
  }
}

#' @noRd
getResponse <- function(d) {

  item_id   <- d@itemmap[, d@item_id]
  resp_data <- d@response[, item_id]

  return(resp_data)

}

#' @noRd
getModel <- function(d, dimensions, bound_cov) {

  resp_data  <- getResponse(d)
  model_text <- c()
  scale_id   <- unique(d@itemmap[, d@scale_id])

  if (dimensions == 1) {

    item_id <- d@itemmap[, d@item_id]
    item_idx <- c()

    for (j in item_id) {
      item_idx <- c(item_idx, which(names(resp_data) == j))
    }

    model_text <- c(
      model_text,
      sprintf("F1 = %s", paste0(item_idx, collapse = ","))
    )

    m <- mirt.model(model_text)

    return(m)

  }

  if (dimensions == 2) {

    for (i in scale_id) {
      item_idx <- which(d@itemmap[, d@scale_id] == i)
      item_id  <- d@itemmap[item_idx, d@item_id]
      item_idx <- c()
      for (j in item_id) {
        item_idx <- c(item_idx, which(names(resp_data) == j))
      }

      model_text <- c(
        model_text,
        sprintf("F%s = %s", i, paste0(item_idx, collapse = ",")))
    }

    model_text <- c(
      model_text,
      sprintf("COV = %s", paste0(sprintf("F%s", scale_id), collapse = "*")))

    if (bound_cov) {
      model_text <- c(
        model_text,
        "UBOUND = (GROUP, COV_21, .999)")
    }

    model_text <- paste0(model_text, collapse = "\n")
    m <- mirt.model(model_text)

    return(m)

  }

}

#' @noRd
getParLayout <- function(d, dimensions, bound_cov) {

  resp_data  <- getResponse(d)
  m          <- getModel(d, dimensions, bound_cov)
  par_layout <- mirt(resp_data, m, itemtype = "graded", pars = "values")

  return(par_layout)

}

#' @noRd
getAnchorPar <- function(d) {

  idx <- c()
  for (j in 1:dim(d@anchor)[2]) {
    if (inherits(d@anchor[, j], "numeric")) {
      if (all(d@anchor[, j] != round(d@anchor[, j]), na.rm = TRUE)) {
        idx <- c(idx, j)
      }
    }
  }

  tmp <- d@anchor[, idx]

  idx <- c(
    grep("^a", names(tmp)),
    grep("^b", names(tmp)),
    grep("^c", names(tmp)),
    grep("^d", names(tmp)),
    grep("^a[1-9]", names(tmp)),
    grep("^cb[1-9]", names(tmp))
  )
  ipar <- tmp[, unique(idx)]

  message(sprintf("anchor has %s items * %s parameters = %s parameters", dim(ipar)[1], dim(ipar)[2], prod(dim(ipar))))

  return(ipar)

}

#' @noRd
getColumn <- function(d, cn) {
  idx <- which(tolower(names(d)) == cn)
  return(d[, idx])
}

#' @noRd
genPrior <- function(theta_grid, dist_type, prior_mean, prior_sd) {

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
getProb <- function(ipar, model, theta_grid) {

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
getEscoreTheta = function(ipar, model, theta, is_minscore_0) {
  pp <- getProb(ipar, "grm", theta)[1, , ]
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
getEAP = function(theta_grid, prior, pp, resp_data) {

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
