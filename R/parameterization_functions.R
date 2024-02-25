#' @include core_functions.R
NULL

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

#' @noRd
convertABtoAD <- function(ipar, item_id, model_id) {

  p_type <- detectParameterization(ipar)

  if (p_type == "ab") {

    dimensions <- detectDimensions(ipar)

    a_columns <- unique(c(
      grep("^a$"      , names(ipar), value = TRUE),
      grep("^a[1-9]$" , names(ipar), value = TRUE)
    ))
    b_columns <- unique(c(
      grep("^b$"      , names(ipar), value = TRUE),
      grep("^b[1-9]$" , names(ipar), value = TRUE),
      grep("^cb$"     , names(ipar), value = TRUE),
      grep("^cb[1-9]$", names(ipar), value = TRUE)
    ))

    ipar_a <- ipar[, a_columns, drop = FALSE]
    ipar_b <- ipar[, b_columns, drop = FALSE]

    ipar_d <- ipar_b * NA

    for (i in 1:nrow(ipar)) {
      if (ipar[i, model_id] == "GR") {
        for (k in 1:ncol(ipar_b)) {
          ipar_d[i, k] <- -ipar_a[i, ] * ipar_b[i, k]
        }
        next
      }
      if (ipar[i, model_id] == "GPC") {
        for (k in 1:ncol(ipar_b)) {
          ipar_d[i, k] <- -ipar_a[i, ] * sum(ipar_b[i, 1:k])
        }
        next
      }
    }

    colnames(ipar_d) <- sprintf("d%s", 1:dim(ipar_d)[2])

    item_ids  <- data.frame(ipar[, item_id])
    model_ids <- data.frame(ipar[, model_id])
    names(item_ids)  <- item_id
    names(model_ids) <- model_id

    ipar_ad <- cbind(
      item_ids,
      model_ids,
      ipar_a, ipar_d
    )

    return(ipar_ad)

  }

  stop("unrecognized parameterization: cannot find cb* or b* columns")

}

#' @noRd
convertADtoAB <- function(ipar, item_id, model_id) {

  p_type <- detectParameterization(ipar)

  if (p_type == "ad") {

    dimensions <- detectDimensions(ipar)
    if (dimensions != 1) {
      stop("conversion from AD to AB requires the input to be in one dimension, but the number of dimension is %s", dimensions)
    }

    a_columns <- unique(c(
      grep("^a$", names(ipar), value = TRUE),
      grep("^a[1-9]$", names(ipar), value = TRUE)
    ))
    d_columns <- unique(c(
      grep("^d$", names(ipar), value = TRUE),
      grep("^d[1-9]$", names(ipar), value = TRUE)
    ))

    ipar_a <- ipar[, a_columns, drop = FALSE]
    ipar_d <- ipar[, d_columns, drop = FALSE]

    ipar_b <- ipar_d * NA

    for (i in 1:nrow(ipar)) {
      if (ipar[i, model_id] == "GR") {
        for (k in 1:ncol(ipar_b)) {
          ipar_b[i, k] <- -ipar_d[i, k] / ipar_a[i, ]
        }
        next
      }
      if (ipar[i, model_id] == "GPC") {
        for (k in 1:ncol(ipar_b)) {
          ipar_b[i, k] <- -ipar_d[i, k] / ipar_a[i, ]
        }
        ipar_b[i, ] <- diff(c(0, as.numeric(ipar_b[i, ])))
        next
      }
    }

    colnames(ipar_b) <- sprintf("b%s", 1:dim(ipar_b)[2])

    item_ids  <- data.frame(ipar[, item_id])
    model_ids <- data.frame(ipar[, model_id])
    names(item_ids)  <- item_id
    names(model_ids) <- model_id

    ipar_ab <- cbind(
      item_ids,
      model_ids,
      ipar_a, ipar_b
    )

    return(ipar_ab)

  }

  stop("unrecognized parameterization: cannot find d* columns")

}
