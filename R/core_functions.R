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
detectNCategories <- function(ipar) {
  nd <- detectDimensions(ipar)
  n_cats <- apply(ipar, 1, function(x) sum(!is.na(x)) - nd + 1)
  return(n_cats)
}

#' (internal) construct a model
#'
#' \code{\link{makeCalibrationModel}} is an internal function for constructing a model.
#'
#' @param d a \code{\linkS4class{PROsetta_data}} object.
#' @param dimensions the number of dimensions to use in the model. Must be \code{1} or \code{2}.
#' If \code{1}, all instruments are modeled to be from one dimension.
#' If \code{2}, each instrument is modeled to be from its own dimension (i.e., calibrated projection is used).
#' @param bound_cov only used when \code{dimensions} is \code{2}.
#' If \code{TRUE}, then constrain the between-dimension covariance to be \code{< .999}.
#'
#' @return \code{\link{makeCalibrationModel}} returns a \code{\link[mirt]{mirt.model}} object.
#'
#' @examples
#' PROsetta:::makeCalibrationModel(data_asq, 1, FALSE)
#' PROsetta:::makeCalibrationModel(data_asq, 1, TRUE)
#' PROsetta:::makeCalibrationModel(data_asq, 2, FALSE)
#' PROsetta:::makeCalibrationModel(data_asq, 2, TRUE)
#'
#' @keywords internal
makeCalibrationModel <- function(d, dimensions, bound_cov) {

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
makeParameterLayout <- function(d, dimensions, bound_cov) {

  resp_data <- getResponse(d)
  m         <- makeCalibrationModel(d, dimensions, bound_cov)
  layout    <- mirt(resp_data, m, itemtype = "graded", pars = "values")

  return(layout)

}

#' @noRd
filterItemParameters <- function(ipar) {

  idx <- c()

  for (j in 1:dim(ipar)[2]) {
    if (inherits(ipar[, j], "numeric")) {
      if (any(ipar[, j] != round(ipar[, j]), na.rm = TRUE)) {
        idx <- c(idx, j)
      }
    }
  }

  ipar <- ipar[, idx]

  idx <- c(
    grep("^a", names(ipar)),
    grep("^b", names(ipar)),
    grep("^c", names(ipar)),
    grep("^d", names(ipar)),
    grep("^a[1-9]", names(ipar)),
    grep("^cb[1-9]", names(ipar))
  )

  ipar <- ipar[, unique(idx)]

  return(ipar)

}

#' @noRd
extractAnchorParameters <- function(d, as_AD) {

  ipar <- filterItemParameters(d@anchor)
  rownames(ipar) <- d@anchor[, d@item_id]

  if (as_AD) {
    ipar              <- convertABtoAD(ipar)
    anchor_dim        <- getAnchorDimension(d)
    colnames(ipar)[1] <- sprintf("a%s", anchor_dim)
  }

  return(ipar)

}

#' @noRd
convertABtoAD <- function(ipar) {

  p_type <- detectParameterization(ipar)

  if (p_type == "ab") {

    dimensions <- detectDimensions(ipar)

    ipar_a <- ipar[, 1:dimensions, drop = FALSE]
    ipar_b <- ipar[, (dimensions + 1):dim(ipar)[2], drop = FALSE]

    ipar_d <- ipar_b * NA
    for (k in 1:dim(ipar_b)[2]) {
      ipar_d[, k] <- -ipar_a * ipar_b[, k]
    }
    colnames(ipar_d) <- sprintf("d%s", 1:dim(ipar_d)[2])

    ipar_ad <- cbind(ipar_a, ipar_d)

    return(ipar_ad)

  }

  stop("unrecognized parameterization: cannot find cb* or b* columns")

}

#' @noRd
convertADtoAB <- function(ipar) {

  p_type <- detectParameterization(ipar)

  if (p_type == "ad") {

    dimensions <- detectDimensions(ipar)

    ipar_a <- ipar[, 1:dimensions, drop = FALSE]
    ipar_d <- ipar[, (dimensions + 1):dim(ipar)[2], drop = FALSE]

    ipar_b <- ipar_d * NA
    for (k in 1:dim(ipar_d)[2]) {
      ipar_b[, k] <- -ipar_d[, k] / ipar_a
    }
    colnames(ipar_b) <- sprintf("b%s", 1:dim(ipar_b)[2])

    ipar_ab <- cbind(ipar_a, ipar_b)

    return(ipar_ab)

  }

  stop("unrecognized parameterization: cannot find d* columns")

}

#' @noRd
getAnchorDimension <- function(d) {

  anchor_items <- d@anchor[, d@item_id]
  anchor_idx   <- which(d@itemmap[, d@item_id] %in% anchor_items)
  anchor_scale <- unique(d@itemmap[anchor_idx, d@scale_id])
  if (length(anchor_scale) == 1) {
    return(anchor_scale)
  }
  stop("anchor items correspond to more than one scale")

}

#' @noRd
applyConstraintsToLayout <- function(layout, d, verbose) {

  if (any("a2" %in% layout$name)) {
    dimensions <- 2
  } else {
    dimensions <- 1
  }

  anchor_dim <- getAnchorDimension(d)

  printLog(
    "constraints",
    sprintf(
      "anchor instrument ID is %s",
      anchor_dim
    ),
    verbose
  )

  ipar_anchor <- extractAnchorParameters(d, as_AD = TRUE)

  printLog(
    "constraints",
    sprintf(
      "anchor has %s items * %s parameters = %s parameters",
      dim(ipar_anchor)[1], dim(ipar_anchor)[2],
      prod(dim(ipar_anchor))
    ),
    verbose
  )

  if (dimensions == 1 & (!"a1" %in% names(ipar_anchor))) {
    # if using a 1D model and the anchor dimension is not 1
    a_par_name <- sprintf("a%s", getAnchorDimension(d))
    a_par_idx  <- which(names(ipar_anchor) == a_par_name)
    names(ipar_anchor)[a_par_idx] <- "a1"
  }
  par_to_fix     <- which(layout$item %in% rownames(ipar_anchor))
  n_items_to_fix <- length(unique(layout$item[par_to_fix]))
  layout$est[par_to_fix] <- FALSE

  for (i in 1:dim(ipar_anchor)[1]) {
    item_name <- rownames(ipar_anchor)[i]
    for (j in 1:dim(ipar_anchor)[2]) {
      par_name <- colnames(ipar_anchor)[j]
      idx <-
        layout$item == item_name &
        layout$name == par_name
      if (length(which(idx)) == 0) {
        stop(sprintf("@anchor: %s %s does not correspond to layout", item_name, par_name))
      }
      if (length(which(idx)) > 2) {
        stop(sprintf("@anchor: %s %s has multiple matches in layout", item_name, par_name))
      }
      layout[idx, "value"] <- ipar_anchor[i, j]
    }
  }

  printLog(
    "constraints",
    sprintf("anchor parameters applied as constraints"),
    verbose
  )

  if (dimensions == 1) {

    par_to_free <- which(layout$class == "GroupPars")
    layout[par_to_free, "est"] <- TRUE

    printLog(
      "constraints",
      "freely estimate mean(theta) and var(theta) to capture difference compared to anchor sample",
      verbose
    )

    return(layout)

  }

  if (dimensions == 2) {

    # Freely estimate mean and variance of anchor dimension
    # to capture the difference relative to anchor
    anchor_dim  <- getAnchorDimension(d)
    par_to_free <- which(
      layout$class == "GroupPars" &
      layout$name %in% c(
        sprintf("MEAN_%s", anchor_dim),
        sprintf("COV_%s%s", anchor_dim, anchor_dim)
      )
    )

    layout[par_to_free, "est"] <- TRUE

    printLog(
      "constraints",
      sprintf(
        "freely estimate mean(theta_%s) and var(theta_%s) to capture difference compared to anchor sample",
        anchor_dim, anchor_dim
      ),
      verbose
    )

    return(layout)

  }

}

#' Compute a Crosswalk Table
#'
#' \code{\link{getRSSS}} is a function for generating a raw-score to standard-score crosswalk table.
#'
#' @param ipar an item parameter matrix for graded response items. Accepts both a/b and a/d format parameters. Accepts multidimensional item parameters.
#' @param theta_grid the theta grid to use for numerical integration.
#' @param is_minscore_0 if \code{TRUE}, the score of each item begins from 0.
#' if \code{FALSE}, the score of each item begins from 1.
#' @param prior_mu_sigma a named list containing prior distribution parameters. All values must be in the theta metric.
#' \itemize{
#'   \item{\code{mu} the prior means}
#'   \item{\code{sigma} the covariance matrix}
#'   \item{\code{sd} the prior standard deviations}
#'   \item{\code{corr} the correlation matrix}
#' }
#'
#' @examples
#' \donttest{
#' ## Free calibration without using anchor
#'
#' o <- runCalibration(data_asq, technical = list(NCYCLES = 1000))
#'
#' ipar <- mirt::coef(o, IRTpars = TRUE, simplify = TRUE)$items
#' items <- getItemNames(data_asq, 2)
#'
#' getRSSS(
#'   ipar = ipar[items, ],
#'   theta_grid = seq(-4, 4, .1),
#'   is_minscore_0 = TRUE,
#'   prior_mu_sigma = list(mu = 0, sigma = 1)
#' )
#' }
#' @export
getRSSS <- function(ipar, theta_grid, is_minscore_0, prior_mu_sigma) {

  if (is.vector(theta_grid)) {
    theta_grid <- matrix(theta_grid)
  }

  if (any(prior_mu_sigma$mu == 50)) {
    message("using theta = 50.0 as prior mean.. (this is very extreme)")
  }

  dimensions <- detectDimensions(ipar)
  n_cats <- detectNCategories(ipar)

  pp <- computeResponseProbability(ipar, "grm", theta_grid)
  L  <- LWrecursion(pp, n_cats, theta_grid, is_minscore_0)
  o  <- LtoEAP(L, theta_grid, prior_mu_sigma)

  theta     <- lapply(o, function(x) x$EAP)
  theta     <- do.call(rbind, theta)
  theta_se  <- lapply(o, function(x) sqrt(diag(x$COV)))
  theta_se  <- do.call(rbind, theta_se)
  tscore    <- round(theta * 10 + 50, 1)
  tscore_se <- round(theta_se * 10, 1)

  if (dimensions > 1) {
    colnames(theta)     <- sprintf("eap_dim%s", 1:dimensions)
    colnames(theta_se)  <- sprintf("eap_se_dim%s", 1:dimensions)
    colnames(tscore)    <- sprintf("tscore_dim%s", 1:dimensions)
    colnames(tscore_se) <- sprintf("tscore_se_dim%s", 1:dimensions)
  } else {
    colnames(theta)     <- sprintf("eap")
    colnames(theta_se)  <- sprintf("eap_se")
    colnames(tscore)    <- sprintf("tscore")
    colnames(tscore_se) <- sprintf("tscore_se")
  }

  rsss_table <- data.frame(
    sum_score = as.numeric(colnames(L))
  )
  rsss_table <- cbind(
    rsss_table,
    tscore, tscore_se,
    theta, theta_se
  )

  return(rsss_table)

}

#' @noRd
appendEscore <- function(score_table, n_scale, item_par_by_scale, min_score) {

  for (s in 1:(n_scale + 1)) {
    for (d in 1:(n_scale + 1)) {
      n_theta <- length(score_table[[s]]$eap)
      e_theta <- rep(NA, n_theta)
      for (i in 1:n_theta) {
        e_theta[i] <- getEscoreTheta(item_par_by_scale[[d]], "grm", score_table[[s]]$eap[i], min_score[d] == 0)
      }
      e_name <- sprintf("escore_%s", names(score_table)[d])
      score_table[[s]][[e_name]] <- e_theta
    }
  }

  return(score_table)

}

#' @noRd
getColumn <- function(d, cn) {
  idx <- which(tolower(names(d)) == cn)
  return(d[, idx])
}

#' @noRd
generatePriorDensity <- function(theta_grid, dist_type, prior_mu_sigma) {

  if (dist_type == "normal") {
    prior <- dmvn(theta_grid, prior_mu_sigma$mu, prior_mu_sigma$sigma)
  } else if (dist_type == "logistic") {
    if (dim(theta_grid)[2] == 1) {
      num   <- exp((theta_grid - prior_mu_sigma$mu) / sqrt(prior_mu_sigma$sigma))
      denom <- (1 + num)^2
      prior <- num / denom
    }
  } else if (dist_type == "unif") {
    if (dim(theta_grid)[2] == 1) {
      prior <- rep(1, length(theta_grid))
    }
  } else {
    stop(sprintf("argument 'dist_type': unrecognized value '%s'", dist_type))
  }

  return(prior)
}

#' (internal) detect parameterization type
#'
#' \code{\link{detectParameterization}} is an internal function for detecting the type of parameterization used in a set of item parameters.
#'
#' @param ipar a \code{\link{data.frame}} containing item parameters.
#'
#' @return \code{\link{detectParameterization}} returns \code{ab} or \code{ad}.
#'
#' @examples
#' PROsetta:::detectParameterization(data_asq@anchor) # ab
#'
#' @keywords internal
detectParameterization <- function(ipar) {
  if ("b1" %in% colnames(ipar)) {
    return("ab")
  }
  if ("cb1" %in% colnames(ipar)) {
    return("ab")
  }
  if ("d1" %in% colnames(ipar)) {
    return("ad")
  }
}

#' (internal) compute response probability
#'
#' \code{\link{computeResponseProbability}} is an internal function for computing response probability from a set of item parameters.
#'
#' @param ipar a \code{\link{data.frame}} containing item parameters.
#' @param model the item model to use. Accepts \code{grm} or {gpcm}.
#' @param theta_grid theta values to compute probability values at.
#'
#' @return \code{\link{computeResponseProbability}} returns an item-wise list of probability matrices.
#'
#' @examples
#' ipar <- PROsetta:::extractAnchorParameters(data_asq, FALSE)
#' theta_q <- seq(-4, 4, .1)
#' p <- PROsetta:::computeResponseProbability(ipar, "grm", theta_q)
#'
#' plot(
#'   0, 0, type = "n", xlim = c(-4, 4), ylim = c(0, 1),
#'   xlab = "Theta", ylab = "Response probability"
#' )
#' lines(theta_q, p[[1]][, 1])
#' lines(theta_q, p[[1]][, 2])
#' lines(theta_q, p[[1]][, 3])
#' lines(theta_q, p[[1]][, 4])
#' lines(theta_q, p[[1]][, 5])
#'
#' @keywords internal
computeResponseProbability <- function(ipar, model, theta_grid) {

  if (is.vector(theta_grid)) {
    theta_grid <- matrix(theta_grid)
  }

  # returns item-wise list of theta * category matrix

  dimensions <- detectDimensions(ipar)
  ni         <- nrow(ipar)
  n_cats     <- detectNCategories(ipar)
  max_cats   <- max(n_cats)
  nq         <- nrow(theta_grid)

  p_type     <- detectParameterization(ipar)

  if (dimensions == 1) {

    # a/b parameterization
    if (p_type == "ab") {
      par_a <- ipar[, 1:dimensions]
      par_b <- ipar[, dimensions + 1:(max_cats - 1), drop = FALSE]
      par_b <- as.matrix(par_b)
    }

    pp <- list()
    for (i in 1:ni) {
      pp[[i]] <- matrix(NA, nq, n_cats[i])
    }

    if (model == "grm") {

      for (i in 1:ni) {

        pp[[i]] <- array_p_gr(
          theta_grid,
          par_a[i],
          par_b[i, 1:(n_cats[i] - 1)]
        )

      }

      return(pp)
    }

    if (model == "gpcm") {

      for (i in 1:ni) {

        pp[[i]] <- array_p_gpc(
          theta_grid,
          par_a[i],
          par_b[i, 1:(n_cats[i] - 1)]
        )

      }

      return(pp)

    }

  }

  if (dimensions == 2) {

    # a/d parameterization

    par_a <- ipar[, 1:dimensions, drop = FALSE]
    par_d <- ipar[, dimensions + 1:(max_cats - 1), drop = FALSE]
    par_a <- as.matrix(par_a)
    par_d <- as.matrix(par_d)

    n_cats <- apply(par_d, 1, function(x) {
      sum(!is.na(x)) + 1
    })
    max_cats <- max(n_cats)

    pp <- list()
    for (i in 1:ni) {
      pp[[i]] <- matrix(NA, nq, max_cats)
    }

    if (model == "grm") {

      for (i in 1:ni) {

        pp[[i]] <- array_p_m_gr(
          theta_grid,
          par_a[i, , drop = FALSE],
          par_d[i, 1:(n_cats[i] - 1), drop = FALSE]
        )

      }

      return(pp)

    }

    if (model == "gpcm") {

      for (i in 1:ni) {

        pp[[i]] <- array_p_m_gpc(
          theta_grid,
          par_a[i, , drop = FALSE],
          par_d[i, 1:(n_cats[i] - 1), drop = FALSE]
        )

      }

    }

  }

  stop(sprintf("argument 'model': unrecognized value '%s'", model))

}

#' @noRd
getEscoreTheta <- function(ipar, model, theta, is_minscore_0) {

  pp <- computeResponseProbability(ipar, "grm", theta)

  e  <- lapply(pp, function(x) {
    sum(x * 0:(length(x) - 1))
  })

  e  <- do.call(c, e)
  ni <- length(e)
  e  <- sum(e)

  if (!is_minscore_0) {
    e <- e + ni
  }

  return(e)

}

#' @noRd
getEAP <- function(theta_grid, prior, pp, resp_data) {

  n  <- dim(resp_data)[1]
  ni <- dim(resp_data)[2]
  nq <- length(theta_grid)
  posterior <- matrix(rep(prior, n), n, nq, byrow = TRUE)

  for (i in 1:ni) {
    resp <- matrix(resp_data[, i], n, 1)
    prob <- t(pp[[i]][, resp])
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

#' @noRd
extractMuSigma <- function(calib) {

  pars  <- mirt::coef(calib, simplify = TRUE)
  mu    <- pars$means
  sigma <- pars$cov
  nd    <- length(mu)

  sd_sigma <- sqrt(diag(sigma))
  sd_inv   <- 1 / sd_sigma
  corr     <- sigma
  corr[]   <- sd_inv * sigma * rep(sd_inv, each = nd)
  corr[cbind(1:nd, 1:nd)] <- 1

  o <- list(
    mu    = mu,
    sigma = sigma,
    sd    = sd_sigma,
    corr  = corr
  )

  return(o)

}

#' @noRd
detectDimensions <- function(item_par_matrix) {
  if ("a2" %in% colnames(item_par_matrix)) {
    return(2)
  } else {
    return(1)
  }
}

#' @noRd
getThetaGrid <- function(dimensions, min_theta, max_theta, inc) {

  theta_grid <- seq(min_theta, max_theta, inc)
  if (dimensions == 1) {
    theta_grid <- expand.grid(theta_grid)
    theta_grid <- as.matrix(theta_grid)
    return(theta_grid)
  }
  if (dimensions == 2) {
    theta_grid <- expand.grid(theta_grid, theta_grid)
    theta_grid <- as.matrix(theta_grid)
    return(theta_grid)
  }

  stop(sprintf("unexpected value %s in 'dimensions': must be 1 or 2", dimensions))

}

#' @noRd
LWrecursion <- function(prob_list, n_cats, theta_grid, is_minscore_0) {

  ni <- length(prob_list)
  nq <- dim(theta_grid)[1]

  min_raw_score <- 0                                 # minimum obtainable raw score
  max_raw_score <- sum(n_cats) - ni                  # maximum obtainable raw score
  raw_score     <- min_raw_score:max_raw_score       # raw scores
  n_score       <- length(raw_score)                 # number of score levels
  inv_tcc       <- numeric(n_score)                  # initialize TCC scoring table
  lh            <- matrix(0, nq, n_score)            # initialize distribution of summed scores

  for (i in 1:ni) {

    if (i == 1) {

      n_cats_i  <- n_cats[1]
      max_score <- 0
      lh[, 1:n_cats_i] <- prob_list[[1]][, 1:n_cats_i]
      idx <- n_cats_i

    }

    if (i > 1) {

      n_cats_i  <- n_cats[i]                    # number of categories for item i
      max_score <- n_cats_i - 1                 # maximum score for item i
      score     <- 0:max_score                  # score values for item i
      prob      <- prob_list[[i]][, 1:n_cats_i] # category probabilities for item i
      plh       <- matrix(0, nq, n_score)       # place holder for lh

      for (k in 1:n_cats_i) {
        for (h in 1:idx) {
          sco <- raw_score[h] + score[k]
          position <- which(raw_score == sco)
          plh[, position] <- plh[, position] + lh[, h] * prob[, k]
        }
      }

      idx <- idx + max_score
      lh <- plh

    }

  }

  if (!is_minscore_0) {
    raw_score <- raw_score + ni
  }

  colnames(lh) <- raw_score

  return(lh)

}

#' @noRd
LtoEAP <- function(L, theta_grid, prior_mu_sigma) {

  dimensions <- dim(theta_grid)[2]
  nq         <- dim(theta_grid)[1]

  prior      <- generatePriorDensity(theta_grid, "normal", prior_mu_sigma)
  n_score    <- dim(L)[2]
  o <- LtoEAP_cpp(L, theta_grid, prior)

  return(o)

}

#' @noRd
getBetaFromMuSigma <- function(mu_sigma, source_dim, target_dim) {

  # get linear coefficients for CPLA

  mu    <- mu_sigma$mu
  sigma <- mu_sigma$sigma

  rho   <- cov2cor(sigma)[1, 2]

  sd_source <- sqrt(diag(sigma)[source_dim])
  sd_target <- sqrt(diag(sigma)[target_dim])

  beta_1 <- rho * sd_target / sd_source

  mu_source <- mu[source_dim]
  mu_target <- mu[target_dim]

  beta_0 <- mu_target - (beta_1 * mu_source)

  o <- list()
  o$beta_0 <- beta_0
  o$beta_1 <- beta_1

  return(o)

}

#' @noRd
appendCPLA <- function(score_table, n_scale, mu_sigma) {

  for (source_dim in 1:n_scale) {

    target_dim   <- setdiff(1:n_scale, source_dim)

    beta_CPLA    <- getBetaFromMuSigma(mu_sigma, source_dim, target_dim)
    rho_CPLA     <- mu_sigma$corr[source_dim, target_dim]

    sigma_target <- mu_sigma$sigma[target_dim, target_dim]
    V_residual   <- (1 - (rho_CPLA ** 2)) * sigma_target

    theta <- cbind(
      score_table[[source_dim]]$eap,
      beta_CPLA$beta_0 + beta_CPLA$beta_1 * score_table[[source_dim]]$eap
    )
    theta_se <- cbind(
      score_table[[source_dim]]$eap_se,
      sqrt(
        ((beta_CPLA$beta_1 ** 2) * (score_table[[source_dim]]$eap_se ** 2)) + V_residual
      )
    )

    tscore    <- round(theta * 10 + 50, 1)
    tscore_se <- round(theta_se * 10, 1)

    n_scores <- dim(theta)[1]
    betas <- cbind(
      rep(beta_CPLA$beta_0, n_scores),
      rep(beta_CPLA$beta_1, n_scores)
    )

    colnames(theta)     <- sprintf("eap_dim%s", c(source_dim, target_dim))
    colnames(theta_se)  <- sprintf("eap_se_dim%s", c(source_dim, target_dim))
    colnames(tscore)    <- sprintf("tscore_dim%s", c(source_dim, target_dim))
    colnames(tscore_se) <- sprintf("tscore_se_dim%s", c(source_dim, target_dim))
    colnames(betas)     <- sprintf("beta_%s", c(0, 1))

    theta     <- theta[, sort(colnames(theta))]
    theta_se  <- theta_se[, sort(colnames(theta_se))]
    tscore    <- tscore[, sort(colnames(tscore))]
    tscore_se <- tscore_se[, sort(colnames(tscore_se))]

    raw_name <- sprintf("raw_%s", source_dim)

    o <- cbind(
      score_table[[source_dim]][, raw_name],
      tscore, tscore_se,
      theta, theta_se,
      betas
    )
    o <- as.data.frame(o)

    colnames(o)[1] <- raw_name

    score_table[[source_dim]] <- o

  }

  return(score_table)

}

#' @noRd
sanitizeData <- function(x) {
  for (v in colnames(x)) {
    if (inherits(x[[v]], "factor")) {
      x[[v]] <- as.character(x[[v]])
    }
  }
  return(x)
}

#' @noRd
printLog <- function(txt1, txt2, verbose) {
  if (verbose) {
    message(sprintf("%-12s| %s", txt1, txt2))
    flush.console()
  }
}
