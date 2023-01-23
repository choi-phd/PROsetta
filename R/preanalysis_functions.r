#' @include loading_functions.R
NULL

#' Check frequency table for unobserved response categories
#'
#' \code{\link{checkFrequency}} is a descriptive function for checking whether all response categories in a frequency table have a frequency of at least 1.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#'
#' @return \code{\link{checkFrequency}} returns \code{TRUE} if all response categories have a frequency of at least 1, and \code{FALSE} if not.
#'
#' @examples
#' checkFrequency(data_asq) # TRUE
#'
#' \dontrun{
#' data_asq@response$EDANX01[data_asq@response$EDANX01 == 4] <- 3
#' checkFrequency(data_asq) # FALSE
#' }
#'
#' @export
checkFrequency <- function(data) {

  validateData(data)

  tmp <- runFrequency(data, check_frequency = FALSE)
  ni <- dim(tmp)[1]
  nc <- dim(tmp)[2]
  msg <- c()

  if (sum(is.na(tmp)) > 0) {
    for (i in 1:ni) {
      nm <- sum(is.na(tmp[i, ]))
      n_cats_obs <- nc - nm

      if (nm > 0) {
        item_id   <- rownames(tmp[i, ])
        idx       <- which(data@itemmap[[data@item_id]] == item_id)
        n_cats_exp <- getColumn(data@itemmap, "ncat")[idx]

        if (n_cats_exp != n_cats_obs) {
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
    msg <- c(msg, "Proceeding to runCFA() or runCalibration() may cause problems.")
    msg <- paste0(msg, collapse = "\n")
    warning(msg)
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Obtain a frequency table
#'
#' \code{\link{runFrequency}} is a descriptive function for obtaining a frequency table from the dataset.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#' @param check_frequency if \code{TRUE}, check the frequency table for missing response categories, and display warning message if any is missing. (default = \code{TRUE})
#'
#' @return \code{\link{runFrequency}} returns a \code{\link{data.frame}} containing the frequency table.
#'
#' @examples
#' freq_asq <- runFrequency(data_asq)
#' freq_dep <- runFrequency(data_dep)
#'
#' @export
runFrequency <- function(data, check_frequency = TRUE) {

  validateData(data)

  resp_data <- getResponse(data)
  tmp <- apply(resp_data, 2, table)

  if (inherits(tmp, "list")) {
    catnames <- unique(do.call(c, lapply(tmp, names)))
    catnames <- sort(as.numeric(catnames))
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
#' \code{\link{runDescriptive}} is a descriptive function for obtaining descriptive statistics for each item in the dataset.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#'
#' @return \code{\link{runDescriptive}} returns a \code{\link{data.frame}} containing descriptive statistics (mean, standard deviation, median, ...) of the items in the dataset. These are calculated with \code{\link[psych]{describe}} in '\href{https://CRAN.R-project.org/package=psych}{psych}' package.
#'
#' @examples
#' out_desc <- runDescriptive(data_asq)
#'
#' @export
runDescriptive <- function(data = NULL) {

  validateData(data)

  resp_data <- getResponse(data)
  desc      <- psych::describe(resp_data)
  desc$vars <- NULL

  return(desc)

}

#' Run CTT-based reliability analysis
#'
#' \code{\link{runClassical}} is a function for performing a Classical Test Theory (CTT) based reliability analysis.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#' @param omega if \code{TRUE}, also obtain McDonald's omega using \code{\link[psych]{omega}} in \href{https://CRAN.R-project.org/package=psych}{\code{psych}} package. (default = \code{FALSE})
#' @param scalewise if \code{TRUE}, run analysis for each instrument as well as for the combined instrument. If \code{FALSE}, run analysis only for the combined instrument. (default = \code{TRUE})
#' @param ... additional arguments to pass onto \code{\link[psych]{omega}}.
#'
#' @return \code{\link{runClassical}} returns a \code{\link{list}} containing reliability analysis results.
#'
#' @examples
#' out_alpha <- runClassical(data_asq)
#' out_omega <- runClassical(data_asq, omega = TRUE) # also obtain omega
#'
#' @export
runClassical <- function(data, omega = FALSE, scalewise = TRUE, ...) {

  validateData(data)

  out_alpha <- list()
  out_omega <- list()

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
  out_alpha[["combined"]] <- psych::alpha(data@response[items])
  if (omega) {
    out_omega[["combined"]] <- psych::omega(data@response[items], ...)
  }

  return(list(
    alpha = out_alpha,
    omega = out_omega
  ))

}

#' Run a confirmatory factor analysis
#'
#' \code{\link{runCFA}} is a function for performing a one-factor confirmatory factor analysis (CFA) to test unidimensionality.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#' @param estimator the estimator to be used. Passed onto \code{\link[lavaan]{cfa}} in \href{https://CRAN.R-project.org/package=lavaan}{'lavaan'} package. (default = \code{WLSMV})
#' @param std.lv if \code{TRUE}, the metric of the latent variable is determined by fixing their (residual) variances to 1.0.
#' If \code{FALSE}, the metric of each latent variable is determined by fixing the factor loading of the first indicator to 1.0.
#' Passed onto \code{\link[lavaan]{cfa}}. (default = \code{TRUE})
#' @param scalewise if \code{TRUE}, run analysis for each instrument as well as for the combined instrument.
#' If \code{FALSE}, run analysis only for the combined instrument. (default = \code{FALSE})
#' @param ... additional arguments to pass onto \code{\link[lavaan]{cfa}}.
#'
#' @return \code{\link{runCFA}} returns a list containing the CFA results.
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

  validateData(data)

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
  out[["combined"]] <- model_fit

  return(out)

}
