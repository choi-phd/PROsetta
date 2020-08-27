#' @include import.R
NULL

#' Load data from supplied config
#'
#' \code{\link{loadData}} is a data loading function to create a \code{\linkS4class{PROsetta_data}} object, for scale linking/equating with 'PROsetta' package.
#'
#' @param response response data containing case IDs and item responses. This can be a \code{.csv} filename or a \code{\link{data.frame}} object.
#' @param itemmap an item map containing item IDs and scale IDs. This can be a \code{.csv} filename or a \code{\link{data.frame}} object.
#' @param anchor anchor data containing item parameters for anchor items. This can be a \code{.csv} filename or a \code{\link{data.frame}} object.
#' @param item_id the column name to look for item IDs. Automatically determined if not specified.
#' @param person_id the column name to look for case IDs. Automatically determined if not specified.
#' @param scale_id the column name to look for scale IDs. Automatically determined if not specified.
#' @param input_dir the directory to look for the files.
#'
#' @return \code{\link{loadData}} returns a \code{\linkS4class{PROsetta_data}} object containing the loaded data.
#'
#' @name loadData
NULL

#' @rdname loadData
setClass("PROsetta_data",
  slots = c(
    response = "list",
    itemmap = "list",
    anchor = "list",
    item_id = "character",
    person_id = "character",
    scale_id = "character"
  ),
  prototype = list(
    response = NULL,
    itemmap = NULL,
    anchor = NULL,
    item_id = "",
    person_id = "",
    scale_id = ""
  ),
  validity = function(object) {

    msg_all <- c()
    if (!object@person_id %in% names(object@response)) {
      msg <- sprintf("@response: cannot find column '%s' from @person_id", object@person_id)
      msg_all <- c(msg_all, msg)
    }
    if (!(object@item_id %in% names(object@itemmap))) {
      msg <- sprintf("@itemmap: cannot find column '%s' from @item_id", object@item_id)
      msg_all <- c(msg_all, msg)
    }
    if (!(object@item_id %in% names(object@anchor))) {
      msg <- sprintf("@anchor: cannot find column '%s' from @item_id", object@item_id)
      msg_all <- c(msg_all, msg)
    }
    if (!(object@scale_id %in% names(object@itemmap))) {
      msg <- sprintf("@itemmap: cannot find column '%s' from @scale_id", object@scale_id)
      msg_all <- c(msg_all, msg)
    }
    if (!is.null(object@itemmap) && !is.null(object@anchor)) {
      if (!all(object@anchor[[object@item_id]] %in% object@itemmap[[object@item_id]])) {
        msg <- sprintf("@anchor: column '%s' contains extra items not in @itemmap", object@item_id)
        msg_all <- c(msg_all, msg)
      }
    }
    if (!is.null(object@itemmap) && !is.null(object@response)) {
      if (!all(object@itemmap[[object@item_id]] %in% names(object@response))) {
        msg <- sprintf("@itemmap: column '%s' contains extra items not in @response", object@item_id)
        msg_all <- c(msg_all, msg)
      }
    }
    if (length(msg_all) > 0) {
      return(msg_all)
    }
    return(TRUE)
  }
)

#' @rdname loadData
#' @export
loadData <- function(response, itemmap, anchor,
  item_id = NULL, person_id = NULL, scale_id = NULL, input_dir = getwd()) {

  if (inherits(response, "character")) {
    p <- checkFilePath(input_dir, response)
    if (!p$exists) stop(sprintf("argument 'response': cannot find the specified file %s", p$path))
    response <- read.csv(p$path, as.is = TRUE)
  } else if (inherits(response, "matrix")) {
    response <- as.data.frame(response)
  } else if (!inherits(response, "data.frame")) {
    stop(sprintf("argument 'response': unrecognized object class %s", class(response)))
  }

  if (inherits(itemmap, "character")) {
    p <- checkFilePath(input_dir, itemmap)
    if (!p$exists) stop(sprintf("argument 'itemmap': cannot find the specified file %s", p$path))
    itemmap <- read.csv(p$path, as.is = TRUE)
  } else if (inherits(itemmap, "matrix")) {
    itemmap <- as.data.frame(itemmap)
  } else if (!inherits(itemmap, "data.frame")) {
    stop(sprintf("argument 'itemmap': unrecognized object class %s", class(itemmap)))
  }

  if (inherits(anchor, "character")) {
    p <- checkFilePath(input_dir, anchor)
    if (!p$exists) stop(sprintf("argument 'anchor': cannot find the specified file %s", p$path))
    anchor <- read.csv(p$path, as.is = TRUE)
  } else if (inherits(anchor, "matrix")) {
    anchor <- as.data.frame(anchor)
  } else if (!inherits(anchor, "data.frame")) {
    stop(sprintf("argument 'anchor': unrecognized object class %s", class(anchor)))
  }


  colnames(itemmap) <- tolower(colnames(itemmap))

  names_response <- colnames(response)
  names_itemmap  <- colnames(itemmap)


  if ("reverse" %in% tolower(names_itemmap)) {
    if (any(itemmap$reverse == 1)) {
      message("some items are marked as reversed items in 'reverse' column in item map")
      message("assuming the response data is already reverse coded")
    } else {
      message("'reverse' column is present in item map, and no items are marked as reversed")
    }
  }


  # Guess IDs

  n_ids <- sum(!is.null(item_id), !is.null(person_id), !is.null(scale_id))

  if (n_ids < 3 & n_ids > 0) {
    stop("supply 'item_id', 'person_id', 'scale_id' all three simultaneously to override ID guessing.")
  }

  if (n_ids == 0) {

    n_match <- rep(NA, dim(itemmap)[2])
    for (j in 1:dim(itemmap)[2]) {
      n_match[j] <- sum(names_response %in% itemmap[, j])
    }
    idx <- which(n_match == max(n_match))[1]
    item_id <- names_itemmap[idx]
    cat(sprintf("item_id guessed as   : %s\n", item_id))

    idx <- which(names_response %in% itemmap[[item_id]] == F)[1]
    person_id <- names_response[idx]
    cat(sprintf("person_id guessed as : %s\n", person_id))

    n_unique <- rep(NA, dim(itemmap)[2])
    for (j in 1:dim(itemmap)[2]) {
      n_unique[j] <- length(unique(itemmap[, j]))
    }

    idx <- which((n_unique != max(n_unique)) & (n_unique != 1))[1]
    if (is.na(idx)) {
      idx <- which(n_unique == 1)[1]
    }
    scale_id <- names_itemmap[idx]
    cat(sprintf("scale_id guessed as  : %s\n", scale_id))

  }


  # Create PROsetta_data object

  data <- new("PROsetta_data")
  data@response  <- response
  data@itemmap   <- itemmap
  data@anchor    <- anchor
  data@item_id   <- item_id
  data@person_id <- person_id
  data@scale_id  <- scale_id

  if (validObject(data)) {
    return(data)
  }
}

#' Check frequency table for unobserved response categories
#'
#' \code{\link{checkFrequency}} is a descriptive function to check whether all response categories in a frequency table have a frequency of at least 1.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#'
#' @return If all response categories have a frequency of at least 1, the value is \code{TRUE}.
#'
#' Otherwise, the value is \code{FALSE}.
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
      ncats_obs <- nc - nm

      if (nm > 0) {
        item_id   <- rownames(tmp[i, ])
        idx       <- which(data@itemmap[[data@item_id]] == item_id)
        ncats_exp <- getColumn(data@itemmap, "ncat")[idx]

        if (ncats_exp != ncats_obs) {
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
    msg <- c(msg, "Proceeding to runCFA or runCalibration may cause problems.")
    msg <- paste0(msg, collapse = "\n")
    warning(msg)
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Obtain a frequency table
#'
#' \code{\link{runFrequency}} is a descriptive function to obtain a frequency table from the dataset.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#' @param check_frequency Logical. If \code{TRUE}, check the frequency table for missing response categories, and display warning message if any is missing. (default = \code{TRUE})
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
    catnames <- sort(unique(do.call(c, lapply(tmp, names))))
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
#' \code{\link{runDescriptive}} is a descriptive function to obtain descriptive statistics for each item in the dataset.
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
#' \code{\link{runClassical}} is a function to perform Classial Test Theory (CTT) based reliability analysis.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#' @param omega if \code{TRUE}, also obtain McDonald's omega using \code{\link[psych]{omega}} in \href{https://CRAN.R-project.org/package=psych}{\code{psych}} package. (default = \code{FALSE})
#' @param scalewise if \code{TRUE}, run analysis for each scale as well as for the combined scale. If \code{FALSE}, run analysis only for the combined scale. (default = \code{TRUE})
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

  out_alpha = list()
  out_omega = list()

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
  out_alpha[['combined']] <- psych::alpha(data@response[items])
  if (omega) {
    out_omega[['combined']] <- psych::omega(data@response[items], ...)
  }

  return(list(
    alpha = out_alpha,
    omega = out_omega
  ))

}

#' Run a confirmatory factor analysis
#'
#' \code{\link{runCFA}} is a function to perform a one-factor confirmatory factor analysis (CFA) to test unidimensionality.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object. See \code{\link{loadData}} for loading a dataset.
#' @param estimator the estimator to be used. Passed onto \code{\link[lavaan]{cfa}} in \href{https://CRAN.R-project.org/package=lavaan}{'lavaan'} package. (default = \code{WLSMV})
#' @param std.lv if \code{TRUE}, the metric of the latent variable is determined by fixing their (residual) variances to 1.0. If \code{FALSE}, the metric of each latent variable is determined by fixing the factor loading of the first indicator to 1.0. Passed onto \code{\link[lavaan]{cfa}}. (default = \code{TRUE})
#' @param scalewise if \code{TRUE}, run analysis for each scale as well as for the combined scale. If \code{FALSE}, run analysis only for the combined scale. (default = \code{FALSE})
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
  out[['combined']] <- model_fit

  return(out)
}
