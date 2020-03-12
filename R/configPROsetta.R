## Written by Seung W. Choi (schoi@austin.utexas.edu)

#' @import psych
#' @import mirt
#' @import plink
#' @importFrom methods new
#' @importFrom utils read.csv
#' @importFrom stats dnorm
NULL

#' An S4 class to represent configurations for reading datasets and writing outputs.
#'
#' @slot study_name Character. A user-specified name of the study.
#' @slot input_directory Character. The path of the input directory for reading dataset.
#' @slot output_directory Character. The path of the output directory for writing outputs. A new folder is created at the path if it does not exist.
#' @slot item_id Character. The name of the variable representing item IDs in \code{anchor_file} and \code{itemmap_file}.
#' @slot person_id Character. The name of the variable representing person IDs in \code{response_file}.
#' @slot scale_id Character. The name of the variable representing scale IDs in \code{itemmap_file}.
#' @slot response_file Character. The filename of a dataset containing IDs and the responses of the items.
#' @slot itemmap_file Character. The filename for an item map.
#' @slot anchor_file Character. The filename of a dataset containing the parameters of the items.
#' @slot linking_method Character. The linking method to use. Accepts "\code{MM}", "\code{MS}", "\code{HB}", "\code{SL}", "\code{FIXEDPAR}", or "\code{NONE}".
#'
#' @rdname PROsetta_config
#' @examples
#' \dontshow{
#' f1 <- tempfile()
#' f2 <- tempfile()
#' f3 <- tempfile()
#' write.csv(response_asq, f1, row.names = FALSE)
#' write.csv(itemmap_asq, f2, row.names = FALSE)
#' write.csv(anchor_asq, f3, row.names = FALSE)
#' cfg <- createConfig(response_file = f1, itemmap_file = f2, anchor_file = f3,
#'   linking_method = "FIXEDPAR")
#' }
#' \donttest{
#' write.csv(response_asq, "response.csv", row.names = FALSE)
#' write.csv(itemmap_asq, "itemmap.csv", row.names = FALSE)
#' write.csv(anchor_asq, "anchor.csv", row.names = FALSE)
#' cfg <- createConfig(
#'   response_file = "response.csv",
#'   itemmap_file = "itemmap.csv",
#'   anchor_file = "anchor.csv",
#'   linking_method = "FIXEDPAR"
#' )
#' }
#' \dontshow{
#'   file.remove(f1)
#'   file.remove(f2)
#'   file.remove(f3)
#' }
#' @export

setClass("PROsetta_config",
  slots = c(
    study_name = "character",
    input_directory = "character",
    output_directory = "character",
    item_id = "character",
    person_id = "character",
    scale_id = "character",
    response_file = "character",
    itemmap_file = "character",
    anchor_file = "character",
    linking_method = "character"
  ),
  prototype = list(
    study_name = "PROsetta",
    input_directory = getwd(),
    output_directory = getwd(),
    item_id = "item_id",
    person_id = "person_id",
    scale_id = "scale_id",
    response_file = "",
    itemmap_file = "",
    anchor_file = "",
    linking_method = "NONE"
  ),
  validity = function(object) {
    if (!dir.exists(object@input_directory)) {
      stop(paste("cannot find the specified path in input_directory :", object@input_directory))
    }
    if (object@output_directory != "") {
      if (!dir.exists(object@output_directory)) dir.create(object@output_directory, recursive = TRUE)
    }
    if (object@response_file != "") {
      p <- checkFilePath(object@input_directory, object@response_file)
      if (!p$exists) stop(paste("cannot find the specified file in response_file :", p$path))
    }
    if (object@itemmap_file != "") {
      p <- checkFilePath(object@input_directory, object@itemmap_file)
      if (!p$exists) stop(paste("cannot find the specified file in itemmap_file :", p$path))
    }
    if (object@linking_method != "NONE" && object@anchor_file != "") {
      p <- checkFilePath(object@input_directory, object@anchor_file)
      if (!p$exists) stop(paste("cannot find the specified file in anchor_file :", p$path))
    }
    if (!object@linking_method %in% c("MM", "MS", "HB", "SL", "FIXEDPAR", "NONE")) {
      stop(sprintf("unrecognized option in @linking_method : %s", object@linking_method))
    }

    return(TRUE)
  }
)

#' Creates a config object for PROsetta
#'
#' Creates a config object for PROsetta
#'
#' @param study_name Character. A user-specified name of the study.
#' @param input_directory Character. The path of the input directory for reading dataset. Defaults to \code{getwd()}.
#' @param output_directory Character. The path of the output directory for writing outputs. Defaults to \code{getwd()}. A new folder is created at the path if it does not exist.
#' @param item_id Character. The name of the variable representing item IDs in \code{anchor_file} and \code{itemmap_file}. Guessed if left blank.
#' @param person_id Character. The name of the variable representing person IDs in \code{response_file}. Guessed if left blank.
#' @param scale_id Character. The name of the variable representing scale IDs in \code{itemmap_file}. Guessed if left blank.
#' @param response_file Character. The filename of a dataset containing IDs and the responses of the items.
#' @param itemmap_file Character. The filename for an item map.
#' @param anchor_file Character. The filename of a dataset containing the parameters of the items.
#' @param linking_method Character. The linking method to use. Accepts "\code{MM}", "\code{MS}", "\code{HB}", "\code{SL}", "\code{FIXEDPAR}", or "\code{NONE}".
#' @return A \code{\linkS4class{PROsetta_config}} object.
#'
#' @rdname PROsetta_config
#' @export

createConfig <- function(study_name = "Study",
                         input_directory = getwd(), output_directory = getwd(),
                         item_id = NULL, person_id = NULL, scale_id = NULL,
                         response_file, itemmap_file, anchor_file,
                         linking_method = "FIXEDPAR") {
  cfg <- new("PROsetta_config",
    input_directory = input_directory,
    output_directory = output_directory,
    response_file = response_file,
    itemmap_file = itemmap_file,
    anchor_file = anchor_file,
    linking_method = toupper(linking_method)
  )

  n_ids <- sum(!is.null(item_id), !is.null(person_id), !is.null(scale_id))

  if (n_ids < 3) {

    if (n_ids > 0) {
      warning("specify item_id, person_id, scale_id all three to override guessing.")
    }

    p <- checkFilePath(input_directory, response_file)
    if (p$exists) {
      d <- read.csv(p$path, as.is = TRUE)
    }
    ids_response <- colnames(d)

    p <- checkFilePath(input_directory, itemmap_file)
    if (p$exists) {
      d <- read.csv(p$path, as.is = TRUE)
    }
    ids_itemmap <- colnames(d)

    n_match <- rep(NA, dim(d)[2])
    for (j in 1:dim(d)[2]) {
      n_match[j] <- sum(ids_response %in% d[, j])
    }

    idx <- which(n_match == max(n_match))[1]
    cfg@item_id <- ids_itemmap[idx]
    cat(sprintf("item_id guessed as  : %s\n", cfg@item_id))

    idx <- which(ids_response %in% d[, idx] == F)[1]
    cfg@person_id <- ids_response[idx]
    cat(sprintf("person_id guessed as: %s\n", cfg@person_id))

    n_unique <- rep(NA, dim(d)[2])
    for (j in 1:dim(d)[2]) {
      n_unique[j] <- length(unique(d[, j]))
    }

    idx <- which((n_unique != max(n_unique)) & (n_unique != 1))[1]
    cfg@scale_id <- ids_itemmap[idx]
    cat(sprintf("scale_id guessed as : %s\n", cfg@scale_id))
  } else {
    cfg@item_id <- item_id
    cfg@person_id <- person_id
    cfg@scale_id <- scale_id
  }

  return(cfg)
}

#' An S4 class to represent \code{PROsetta} datasets.
#'
#' @slot response A list containing IDs and the responses of the items.
#' @slot itemmap A list containing an item map.
#' @slot anchor A list containing the parameters of the items.
#'
#' @export

setClass("PROsetta_data",
  slots = c(
    response = "list",
    itemmap = "list",
    anchor = "list"
  ),
  prototype = list(
    response = NULL,
    itemmap = NULL,
    anchor = NULL
  ),
  validity = function(object) {
    return(TRUE)
  }
)

#' Check file path
#'
#' Check file path to see if it exists.
#'
#' @param abspath Absolute path of an input directory.
#' @param path Absolute or relative path of a file.
#'
#' @return a list containing normalized file path and whether it exists.

checkFilePath <- function(abspath, path) {
  p <- path
  if (file.exists(p)) {
    return(list(path = normalizePath(p), exists = TRUE))
  }

  p <- file.path(abspath, path)
  if (file.exists(p)) {
    return(list(path = normalizePath(p), exists = TRUE))
  }

  return(list(path = NULL, exists = FALSE))
}


#' Load data from supplied config
#'
#' Loads data from the supplied configuration.
#'
#' @param config A PROsetta_config object. See \linkS4class{PROsetta_config}.
#'
#' @return An S4-class object containing the loaded data.
#'
#' @export

loadData <- function(config) {

  if (class(config) != "PROsetta_config") {
    stop("config must be a 'PROsetta_config' class object")
  }

  data <- new("PROsetta_data")

  p <- checkFilePath(config@input_directory, config@response_file)
  if (p$exists) {
    data@response <- read.csv(p$path, as.is = TRUE)
    if (!(config@person_id %in% names(data@response))) {
      warning(sprintf("column '%s' does not exist in response_file", config@person_id))
    }
  }

  p <- checkFilePath(config@input_directory, config@itemmap_file)
  if (p$exists) {
    data@itemmap <- read.csv(p$path, as.is = TRUE)
    if (!(config@item_id %in% names(data@itemmap))) {
      warning(sprintf("column '%s' does not exist in itemmap_file", config@item_id))
    }
  }

  p <- checkFilePath(config@input_directory, config@anchor_file)
  if (p$exists) {
    data@anchor <- read.csv(p$path, as.is = TRUE)
    if (!(config@item_id %in% names(data@anchor))) {
      warning(sprintf("column '%s' does not exist in anchor_file", config@item_id))
    }
  }

  if (!is.null(data@itemmap) && !is.null(data@anchor)) {
    if (config@item_id %in% names(data@itemmap) && config@item_id %in% names(data@anchor)) {
      if (!all(data@anchor[[config@item_id]] %in% data@itemmap[[config@item_id]])) {
        stop(sprintf("column '%s' in anchor_file contains items that are not in itemmap_file", config@item_id))
      }
    }
  }

  if (!is.null(data@itemmap) && !is.null(data@response)) {
    if (!all(data@itemmap[[config@item_id]] %in% names(data@response))) {
      stop(sprintf("%s in itemmap_file contains items that are not in response_file", config@item_id))
    }
  }

  resp_with_missing_values <- apply(is.na(data@response), 1, any)
  if (any(resp_with_missing_values)) {
    n_resp <- sum(resp_with_missing_values)
    data@response <- data@response[!resp_with_missing_values, ]
    message(sprintf("%s response rows were removed because they had missing values", n_resp))
  }

  return(data)
}


#' Check frequency table for unobserved response categories
#'
#' Checks frequency table for unobserved response categories.
#'
#' @param config A PROsetta_config object. See \code{\linkS4class{PROsetta_config}}.
#' @param data A PROsetta_data object. See \code{\link{loadData}} for loading a dataset.
#'
#' @return Logical. \code{TRUE} if all categories are present. \code{FALSE} otherwise.
#'
#' @export

checkFrequency <- function(config, data = NULL) {

  if (class(config) != "PROsetta_config") {
    stop("config must be a 'PROsetta_config' class object")
  }
  if (is.null(data)) {
    data <- loadData(config)
  } else if (class(data) != "PROsetta_data") {
    stop("data must be a 'PROsetta_data' class object")
  }

  tmp <- runFrequency(config, data, check_frequency = FALSE)
  ni <- dim(tmp)[1]
  nc <- dim(tmp)[2]
  msg <- c()

  if (sum(is.na(tmp)) > 0) {
    for (i in 1:ni) {
      nm <- sum(is.na(tmp[i, ]))
      ncats_obs <- nc - nm

      if (nm > 0) {
        item_id <- rownames(tmp[i, ])
        idx <- which(data@itemmap[[config@item_id]] == item_id)
        ncats_exp <- data@itemmap[idx, ][["NCAT"]]

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
#' Obtains a frequency table from the supplied configuration and the dataset.
#'
#' @param config A PROsetta_config object. See \code{\linkS4class{PROsetta_config}}.
#' @param data A PROsetta_data object. See \code{\link{loadData}} for loading a dataset.
#' @param check_frequency Logical. If \code{TRUE}, check the frequency table for missing response categories, and display warning message if any is missing.
#' @return A \code{data.frame} containing the frequency table of the dataset.
#'
#' @examples
#' \dontshow{
#' f1 <- tempfile()
#' f2 <- tempfile()
#' f3 <- tempfile()
#' write.csv(response_asq, f1, row.names = FALSE)
#' write.csv(itemmap_asq, f2, row.names = FALSE)
#' write.csv(anchor_asq, f3, row.names = FALSE)
#' cfg <- createConfig(response_file = f1, itemmap_file = f2, anchor_file = f3)
#' }
#' \donttest{
#' write.csv(response_asq, "response.csv", row.names = FALSE)
#' write.csv(itemmap_asq, "itemmap.csv", row.names = FALSE)
#' write.csv(anchor_asq, "anchor.csv", row.names = FALSE)
#' cfg <- createConfig(
#'   response_file = "response.csv",
#'   itemmap_file = "itemmap.csv",
#'   anchor_file = "anchor.csv")
#' }
#' freqTable <- runFrequency(cfg)
#' \dontshow{
#'   file.remove(f1)
#'   file.remove(f2)
#'   file.remove(f3)
#' }
#' @export

runFrequency <- function(config, check_frequency = TRUE, data = NULL) {
  if (class(config) != "PROsetta_config") {
    stop("config must be a 'PROsetta_config' class object")
  }
  if (is.null(data)) {
    data <- loadData(config)
  } else if (class(data) != "PROsetta_data") {
    stop("data must be a 'PROsetta_data' class object")
  }

  tmp <- data@response[data@itemmap[[config@item_id]]]
  tmp <- apply(tmp, 2, table)

  if (class(tmp) == "list") {
    catnames <- unique(do.call(c, lapply(tmp, names)))
    freq <- as.data.frame(matrix(NA, length(tmp), length(catnames)))
    colnames(freq) <- catnames
    rownames(freq) <- names(tmp)
    for (i in 1:length(tmp)) {
      cats <- names(tmp[[i]])
      freq[i, cats] <- tmp[[i]]
    }
  }

  if (class(tmp) == "matrix") {
    freq <- t(tmp)
  }

  if (check_frequency) {
    checkFrequency(config, data)
  }

  return(freq)
}

#' Obtain a descriptive statistics table
#'
#' Obtains a table with descriptive statistics for each variable, from the supplied configuration and the dataset.
#'
#' @param config A PROsetta_config object. See \code{\linkS4class{PROsetta_config}}.
#' @param data (Optional) A PROsetta_data object. See \code{\link{loadData}} for loading a dataset.
#'
#' @return A \code{data.frame} containing the descriptive statistics (mean, standard deviation, median, ...) of the variables in the dataset. These are calculated with \code{\link[psych]{describe}} in \href{https://CRAN.R-project.org/package=psych}{\code{psych}} package.
#'
#' @examples
#' \dontshow{
#' f1 <- tempfile()
#' f2 <- tempfile()
#' f3 <- tempfile()
#' write.csv(response_asq, f1, row.names = FALSE)
#' write.csv(itemmap_asq, f2, row.names = FALSE)
#' write.csv(anchor_asq, f3, row.names = FALSE)
#' cfg <- createConfig(response_file = f1, itemmap_file = f2, anchor_file = f3)
#' }
#' \donttest{
#' write.csv(response_asq, "response.csv", row.names = FALSE)
#' write.csv(itemmap_asq, "itemmap.csv", row.names = FALSE)
#' write.csv(anchor_asq, "anchor.csv", row.names = FALSE)
#' cfg <- createConfig(
#'   response_file = "response.csv",
#'   itemmap_file = "itemmap.csv",
#'   anchor_file = "anchor.csv")
#' }
#' descTable <- runDescriptive(cfg)
#' \dontshow{
#'   file.remove(f1)
#'   file.remove(f2)
#'   file.remove(f3)
#' }
#' @export

runDescriptive <- function(config, data = NULL) {
  if (class(config) != "PROsetta_config") {
    stop("config must be a 'PROsetta_config' class object")
  }
  if (is.null(data)) {
    data <- loadData(config)
  } else if (class(data) != "PROsetta_data") {
    stop("data must be a 'PROsetta_data' class object")
  }

  desc <- psych::describe(data@response[data@itemmap[[config@item_id]]])[-1]
  return(desc)
}

#' Run a CTT-based reliability analysis
#'
#' Performs a Classial Test Theory (CTT) reliability analysis.
#'
#' @param config A PROsetta_config object. See \code{\linkS4class{PROsetta_config}}.
#' @param data (Optional) A PROsetta_data object. See \code{\link{loadData}} for loading a dataset.
#' @param omega If TRUE, also obtains McDonald's omega using \code{\link[psych]{omega}} in \href{https://CRAN.R-project.org/package=psych}{\code{psych}} package.
#' @param scalewise If TRUE, run analysis for each scale as well as for the combined scale. If FALSE (default), run analysis only for the combined scale.
#' @param ... Additional arguments to pass onto \code{\link[psych]{omega}}.
#'
#' @return The results of reliability analysis.
#'
#' @examples
#' \dontshow{
#' f1 <- tempfile()
#' f2 <- tempfile()
#' f3 <- tempfile()
#' write.csv(response_asq, f1, row.names = FALSE)
#' write.csv(itemmap_asq, f2, row.names = FALSE)
#' write.csv(anchor_asq, f3, row.names = FALSE)
#' cfg <- createConfig(response_file = f1, itemmap_file = f2, anchor_file = f3)
#' }
#' \donttest{
#' write.csv(response_asq, "response.csv", row.names = FALSE)
#' write.csv(itemmap_asq, "itemmap.csv", row.names = FALSE)
#' write.csv(anchor_asq, "anchor.csv", row.names = FALSE)
#' cfg <- createConfig(
#'   response_file = "response.csv",
#'   itemmap_file = "itemmap.csv",
#'   anchor_file = "anchor.csv")
#' }
#' classicalTable <- runClassical(cfg)
#' classicalTable2 <- runClassical(cfg, omega = TRUE) # also obtains omega
#' \dontshow{
#'   file.remove(f1)
#'   file.remove(f2)
#'   file.remove(f3)
#' }
#' @export

runClassical <- function(config, data = NULL, omega = FALSE, scalewise = FALSE, ...) {

  if (class(config) != "PROsetta_config") {
    stop("config must be a 'PROsetta_config' class object")
  }
  if (is.null(data)) {
    data <- loadData(config)
  } else if (class(data) != "PROsetta_data") {
    stop("data must be a 'PROsetta_data' class object")
  }

  out_alpha = list()
  out_omega = list()

  if (scalewise) {
    for (scale_id in unique(data@itemmap[[config@scale_id]])) {
      itemmap <- subset(data@itemmap, data@itemmap[[config@scale_id]] == scale_id)
      items <- itemmap[[config@item_id]]
      out_alpha[[as.character(scale_id)]] <- psych::alpha(data@response[items])
      if (omega) {
        out_omega[[as.character(scale_id)]] <- psych::omega(data@response[items], ...)
      }
    }
  }

  items <- data@itemmap[[config@item_id]]
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
#' Performs a one-factor confirmatory factor analysis (CFA) to test unidimensionality.
#'
#' @param config A PROsetta_config object. See \code{\linkS4class{PROsetta_config}}.
#' @param data (Optional) A PROsetta_data object. See \code{\link{loadData}} for loading a dataset.
#' @param estimator The estimator to be used. Passed onto \code{\link[lavaan]{cfa}} in \href{https://CRAN.R-project.org/package=lavaan}{\code{lavaan}} package.
#' @param std.lv If \code{TRUE}, the metric of the latent variable is determined by fixing their (residual) variances to 1.0. If \code{FALSE}, the metric of each latent variable is determined by fixing the factor loading of the first indicator to 1.0. Passed onto \code{\link[lavaan]{cfa}}.
#' @param scalewise If TRUE, run analysis for each scale as well as for the combined scale. If FALSE (default), run analysis only for the combined scale.
#' @param ... Additional arguments to pass onto \code{\link[lavaan]{cfa}}.
#'
#' @return A list containing the CFA results. The models are as follows:
#' \item{all}{A one-factor model where all items in the \code{itemmap} slot of \code{data} are loaded onto the factor.}
#' \item{anchor}{A one-factor model where the items in the \code{anchor} slot of \code{data} are loaded onto the factor.}
#'
#' @examples
#' \dontshow{
#' f1 <- tempfile()
#' f2 <- tempfile()
#' f3 <- tempfile()
#' write.csv(response_asq, f1, row.names = FALSE)
#' write.csv(itemmap_asq, f2, row.names = FALSE)
#' write.csv(anchor_asq, f3, row.names = FALSE)
#' cfg <- createConfig(response_file = f1, itemmap_file = f2, anchor_file = f3)
#' }
#' \donttest{
#' write.csv(response_asq, "response.csv", row.names = FALSE)
#' write.csv(itemmap_asq, "itemmap.csv", row.names = FALSE)
#' write.csv(anchor_asq, "anchor.csv", row.names = FALSE)
#' cfg <- createConfig(
#'   response_file = "response.csv",
#'   itemmap_file = "itemmap.csv",
#'   anchor_file = "anchor.csv")
#' }
#' solution <- runCFA(cfg)
#' summary(solution$all, fit.measures = TRUE, standardized = TRUE)
#' summary(solution$anchor, fit.measures = TRUE, standardized = TRUE)
#' \dontshow{
#'   file.remove(f1)
#'   file.remove(f2)
#'   file.remove(f3)
#' }
#' @export

runCFA <- function(config, estimator = "WLSMV", std.lv = TRUE, data = NULL, scalewise = FALSE, ...) {
  if (class(config) != "PROsetta_config") {
    stop("config must be a 'PROsetta_config' class object")
  }
  if (is.null(data)) {
    data <- loadData(config)
  } else if (class(data) != "PROsetta_data") {
    stop("data must be a 'PROsetta_data' class object")
  }

  out <- list()

  if (scalewise) {
    for (scale_id in unique(data@itemmap[[config@scale_id]])) {
      itemmap <- subset(data@itemmap, data@itemmap[[config@scale_id]] == scale_id)
      items <- itemmap[[config@item_id]]
      model <- paste("Factor =~ ", paste0(items, collapse = " + "))
      model_fit <- lavaan::cfa(model, data@response, estimator = estimator, ordered = items, std.lv = std.lv, ...)
      out[[as.character(scale_id)]] <- model_fit
    }
  }

  itemmap <- data@itemmap
  items <- itemmap[[config@item_id]]
  model <- paste("Factor =~ ", paste0(items, collapse = " + "))
  model_fit <- lavaan::cfa(model, data@response, estimator = estimator, ordered = items, std.lv = std.lv, ...)
  out[['combined']] <- model_fit

  return(out)
}

#' Run Calibration
#'
#' Performs item calibration for the response data based on the supplied anchor information.
#'
#' @param config A PROsetta_config object. See \code{\linkS4class{PROsetta_config}}.
#' @param data A PROsetta_data object. See \code{\link{loadData}} for loading a dataset.
#' @param ... Additional arguments to pass onto \code{\link[mirt]{mirt}} in \href{https://CRAN.R-project.org/package=mirt}{\code{mirt}} package.
#'
#' @return An object containing item calibration results. This object can be used in \code{\link[mirt:coef-method]{coef}}, \code{\link[mirt]{itemfit}}, \code{\link[mirt]{itemplot}} in \href{https://CRAN.R-project.org/package=mirt}{\code{mirt}} package to extract wanted information.
#'
#' @examples
#' \dontshow{
#' f1 <- tempfile()
#' f2 <- tempfile()
#' f3 <- tempfile()
#' write.csv(response_asq, f1, row.names = FALSE)
#' write.csv(itemmap_asq, f2, row.names = FALSE)
#' write.csv(anchor_asq, f3, row.names = FALSE)
#' cfg <- createConfig(response_file = f1, itemmap_file = f2, anchor_file = f3)
#' }
#' \donttest{
#' write.csv(response_asq, "response.csv", row.names = FALSE)
#' write.csv(itemmap_asq, "itemmap.csv", row.names = FALSE)
#' write.csv(anchor_asq, "anchor.csv", row.names = FALSE)
#' cfg <- createConfig(
#'   response_file = "response.csv",
#'   itemmap_file = "itemmap.csv",
#'   anchor_file = "anchor.csv")
#' }
#' solution <- runCalibration(cfg)
#' mirt::coef(solution, IRTpars = TRUE, simplify = TRUE)
#' mirt::itemfit(solution, empirical.plot = 1)
#' mirt::itemplot(solution, item = 1, type = "info")
#' mirt::itemfit(solution, "S_X2", na.rm = TRUE)
#'
#' cfg2 <- cfg
#' cfg2@linking_method <- "SL"
#' solution2 <- runCalibration(cfg2)
#' solution2 <- runCalibration(cfg2, technical = list(NCYCLES = 1000))
#' mirt::coef(solution2, IRTpars = TRUE, simplify = TRUE)
#' \dontshow{
#'   file.remove(f1)
#'   file.remove(f2)
#'   file.remove(f3)
#' }
#' @export

runCalibration <- function(config, data = NULL, ...) {
  if (class(config) != "PROsetta_config") {
    stop("config must be a 'PROsetta_config' class object")
  }
  if (is.null(data)) {
    data <- loadData(config)
  } else if (class(data) != "PROsetta_data") {
    stop("data must be a 'PROsetta_data' class object")
  }

  if (config@linking_method == "FIXEDPAR") {
    par_layout <- mirt::mirt(data@response[data@itemmap[[config@item_id]]], 1, itemtype = "graded", pars = "values")
    fixed <- which(par_layout$item %in% data@anchor[[config@item_id]])
    par_layout[fixed, "est"] <- FALSE
    par_layout[which(par_layout$class == "GroupPars"), "est"] <- TRUE

    for (i in fixed) {
      item <- which(data@anchor[[config@item_id]] == par_layout$item[i])

      if (substr(par_layout$name[i], 1, 1) == "a") {
        par_layout[i, "value"] <- data@anchor[item, "a"]
      } else {
        k <- as.numeric(gsub("[[:alpha:]]", "", par_layout$name[i]))
        par_layout[i, "value"] <- -data@anchor[item, "a"] * data@anchor[item, paste0("cb", k)]
      }
    }

    calibration <- mirt::mirt(data@response[data@itemmap[[config@item_id]]], 1, itemtype = "graded", pars = par_layout, ...)
  } else {
    calibration <- mirt::mirt(data@response[data@itemmap[[config@item_id]]], 1, itemtype = "graded", ...)
  }
  # ipar = mirt::coef(calibration, IRTpars = TRUE, simplify = TRUE)$items
  return(calibration)
}

#' Run Scale Linking
#'
#' Performs scale linking and obtains a set of transformation coefficients.
#'
#' @param config A PROsetta_config object. See \code{\linkS4class{PROsetta_config}}.
#' @param data (Optional) A PROsetta_data object. See \code{\link{loadData}} for loading a dataset.
#' @param ... Additional arguments to pass onto \code{\link[mirt]{mirt}} in \href{https://CRAN.R-project.org/package=mirt}{\code{mirt}} package.
#'
#' @return A list containing the scale linking results. These are obtained with \code{\link[plink]{plink-methods}} in \href{https://CRAN.R-project.org/package=plink}{\code{plink}} package.
#'
#' @examples
#' \dontshow{
#' f1 <- tempfile()
#' f2 <- tempfile()
#' f3 <- tempfile()
#' write.csv(response_asq, f1, row.names = FALSE)
#' write.csv(itemmap_asq, f2, row.names = FALSE)
#' write.csv(anchor_asq, f3, row.names = FALSE)
#' cfg <- createConfig(response_file = f1, itemmap_file = f2, anchor_file = f3,
#'   linking_method = "SL")
#' }
#' \donttest{
#' write.csv(response_asq, "response.csv", row.names = FALSE)
#' write.csv(itemmap_asq, "itemmap.csv", row.names = FALSE)
#' write.csv(anchor_asq, "anchor.csv", row.names = FALSE)
#' cfg <- createConfig(
#'   response_file = "response.csv",
#'   itemmap_file = "itemmap.csv",
#'   anchor_file = "anchor.csv",
#'   linking_method = "SL")
#' }
#' solution <- runLinking(cfg, technical = list(NCYCLES = 1000))
#' solution$link@constants$SL
#' \dontshow{
#'   file.remove(f1)
#'   file.remove(f2)
#'   file.remove(f3)
#' }
#' @export

runLinking <- function(config, data = NULL, ...) {
  if (class(config) != "PROsetta_config") {
    stop("config must be a 'PROsetta_config' class object")
  }
  if (is.null(data)) {
    data <- loadData(config)
  } else if (class(data) != "PROsetta_data") {
    stop("data must be a 'PROsetta_data' class object")
  }
  if (is.null(data@anchor)) {
    stop("anchor cannot be NULL")
  }
  if (!config@linking_method %in% c("MM", "MS", "HB", "SL", "LS")) {
    stop("config@linking_method must be one of the following: 'MM', 'MS', 'HB', 'SL', 'LS'.")
  }

  calibration <- runCalibration(config, data, ...)
  ipar <- mirt::coef(calibration, IRTpars = TRUE, simplify = TRUE)$items
  ni_all <- nrow(ipar)
  ni_anchor <- nrow(data@anchor)
  maxCat <- max(data@anchor$NCAT)
  id_new <- data.frame(New = 1:ni_all, ID = data@itemmap[[config@item_id]])
  id_old <- data.frame(Old = 1:ni_anchor, ID = data@anchor[[config@item_id]])
  common <- merge(id_new, id_old, by = "ID", sort = FALSE)[c("New", "Old")]
  pars <- vector("list", 2)

  pars[[1]] = ipar
  pars[[2]] = data@anchor[c("a", paste0("cb", 1:(maxCat - 1)))]

  pm_all <- as.poly.mod(ni_all, "grm", 1:ni_all)
  pm_anchor <- as.poly.mod(ni_anchor, "grm", 1:ni_anchor)
  ncat <- list(data@itemmap$NCAT, data@anchor$NCAT)
  out <- plink::plink(as.irt.pars(pars, common, cat = ncat, list(pm_all, pm_anchor), grp.names = c("From", "To")), rescale = config@linking_method, base.grp = 2)

  rownames(out$pars@pars$From) <- id_new$ID
  rownames(out$pars@pars$To)   <- id_old$ID
  colnames(out$pars@pars$From) <- colnames(ipar)
  colnames(out$pars@pars$To)   <- colnames(ipar)

  return(out)
}

#' Run Test Equating
#'
#' Performs equipercentile test equating between two scales.
#'
#' @param config A PROsetta_config object. See \code{\linkS4class{PROsetta_config}}.
#' @param data A PROsetta_data object. See \code{\link{loadData}} for loading a dataset.
#' @param scale_to Numeric. The index of the target scale to equate to. This and \code{scale_from} below both reference to the information stored in the \code{itemmap} slot of \code{Data} argument. The \code{scale_id} slot of \code{config} argument needs to be specified as the name of the varible containing the scale IDs in the \code{itemmap} slot.
#' @param scale_from Numeric. The index of the scale in need of test equating.
#' @param type The type of equating to be passed onto \code{\link[equate]{equate}} in \href{https://CRAN.R-project.org/package=equate}{\code{equate}} package.
#' @param smooth The type of smoothing method to be passed onto \code{\link[equate]{presmoothing}} in \href{https://CRAN.R-project.org/package=equate}{\code{equate}} package.
#' @param degrees The degrees of smoothing to be passed onto \code{\link[equate]{presmoothing}}.
#' @param boot Logical. Performs bootstrapping if \code{TRUE}.
#' @param reps Numeric. The number of replications in bootsrapping.
#' @param ... Other arguments to pass onto \code{\link[equate]{equate}}..
#'
#' @return An \code{equate} object containing the test equating result. The printed summary statistics indicate the distributional properties of the two supplied scales and the equated scale. The rows titled \code{x} and \code{y} correspond to the scales specified in \code{scale_from} and \code{scale_to} respectively. The row titled \code{yx} corresponds to the \code{scale_from} scale transformed to \code{scale_to}. See \code{\link[equate]{equate}} for details.
#'
#' @examples
#' \dontshow{
#' f1 <- tempfile()
#' f2 <- tempfile()
#' f3 <- tempfile()
#' write.csv(response_asq, f1, row.names = FALSE)
#' write.csv(itemmap_asq, f2, row.names = FALSE)
#' write.csv(anchor_asq, f3, row.names = FALSE)
#' cfg <- createConfig(response_file = f1, itemmap_file = f2, anchor_file = f3)
#' }
#' \donttest{
#' write.csv(response_asq, "response.csv", row.names = FALSE)
#' write.csv(itemmap_asq, "itemmap.csv", row.names = FALSE)
#' write.csv(anchor_asq, "anchor.csv", row.names = FALSE)
#' cfg <- createConfig(
#'   response_file = "response.csv",
#'   itemmap_file = "itemmap.csv",
#'   anchor_file = "anchor.csv")
#' }
#' solution <- runEquateObserved(cfg,
#'   scale_to = 1, scale_from = 2,
#'   type = "equipercentile", smooth = "loglinear"
#' )
#' \dontshow{
#'   file.remove(f1)
#'   file.remove(f2)
#'   file.remove(f3)
#' }
#' @export

runEquateObserved <- function(config, scale_to = 1, scale_from = 2, type = "equipercentile", smooth = "loglinear", degrees = list(3, 1), boot = TRUE, reps = 100, data = NULL, ...) {
  if (class(config) != "PROsetta_config") {
    stop("config must be a 'PROsetta_config' class object")
  }
  if (is.null(data)) {
    data <- loadData(config)
  } else if (class(data) != "PROsetta_data") {
    stop("data must be a 'PROsetta_data' class object")
  }

  scale_id       <- data@itemmap[[config@scale_id]]
  scale_code     <- unique(scale_id)
  items_to       <- which(scale_id %in% scale_to)   # Reference items
  items_from     <- which(scale_id %in% scale_from) # Items that need to be equated
  itemnames      <- data@itemmap[[config@item_id]]
  itemnames_to   <- itemnames[items_to]
  itemnames_from <- itemnames[items_from]
  scores_to      <- rowSums(data@response[itemnames_to])
  scores_from    <- rowSums(data@response[itemnames_from])
  freq_to        <- equate::freqtab(data@response[itemnames], items = items_to  )
  freq_from      <- equate::freqtab(data@response[itemnames], items = items_from)
  score_stat     <- rbind(From = summary(freq_from), To = summary(freq_to))

  # plot(x = freq_to, lwd = 2, xlab = "Score", ylab = "Count")
  # plot(x = sfreq_to, lwd = 2, xlab = "Score", ylab = "Count")

  if (smooth != "none") {
    freq_to   <- equate::presmoothing(freq_to, smoothmethod = smooth, degrees = degrees)
    freq_from <- equate::presmoothing(freq_from, smoothmethod = smooth, degrees = degrees)
  }

  out <- equate::equate(freq_from, freq_to, type = type, boot = boot, reps = reps, ...)
  return(out)
}

#' Run Scoring Table Generation
#'
#' Generates raw-score to scale-score crosswalk tables.
#'
#' @param config A PROsetta_config object. See \code{\linkS4class{PROsetta_config}}.
#' @param data (Optional) A PROsetta_data object. See \code{\link{loadData}} for loading a dataset.
#' @param calibration An object returned from \code{\link{runCalibration}} or \code{\link{runLinking}}
#' @param prior_mean Prior mean.
#' @param prior_sd Prior standard deviation.
#' @param min_theta LL of theta grid.
#' @param max_theta UL of theta grid.
#' @param inc Increment of theta grid.
#' @param min_score Minimum item score (0 or 1).
#' @param t_score TRUE to convert theta to T-score.
#'
#' @return A list containing crosswalk tables.
#'
#' @examples
#' \dontshow{
#' f1 <- tempfile()
#' f2 <- tempfile()
#' f3 <- tempfile()
#' write.csv(response_asq, f1, row.names = FALSE)
#' write.csv(itemmap_asq, f2, row.names = FALSE)
#' write.csv(anchor_asq, f3, row.names = FALSE)
#' cfg <- createConfig(response_file = f1, itemmap_file = f2, anchor_file = f3)
#' }
#' \donttest{
#' write.csv(response_asq, "response.csv", row.names = FALSE)
#' write.csv(itemmap_asq, "itemmap.csv", row.names = FALSE)
#' write.csv(anchor_asq, "anchor.csv", row.names = FALSE)
#' cfg <- createConfig(
#'   response_file = "response.csv",
#'   itemmap_file = "itemmap.csv",
#'   anchor_file = "anchor.csv")
#' }
#' solution    <- runCalibration(cfg)
#' score_table <- runRSSS(cfg, calibration = solution)
#'
#' cfg2                    <- cfg
#' cfg2@linking_method     <- "SL"
#' solution                <- runLinking(cfg2, technical = list(NCYCLES = 1000))
#' score_table_linking     <- runRSSS(cfg2, calibration = solution)
#' \dontshow{
#'   file.remove(f1)
#'   file.remove(f2)
#'   file.remove(f3)
#' }
#' @export

runRSSS <- function(config, calibration, prior_mean = 0.0, prior_sd = 1.0, min_theta = -4.0, max_theta = 4.0, inc = 0.01, min_score = 1, t_score = TRUE, data = NULL) {
  if (class(config) != "PROsetta_config") {
    stop("config must be a 'PROsetta_config' class object")
  }
  if (is.null(data)) {
    data <- loadData(config)
  } else if (class(data) != "PROsetta_data") {
    stop("data must be a 'PROsetta_data' class object")
  }
  if (is.null(attr(class(calibration), "package"))) {
    item_par <- calibration$pars@pars$From
  } else if (isS4(calibration) && attr(class(calibration), "package") == "mirt") {
    item_par <- mirt::coef(calibration, IRTpars = TRUE, simplify = TRUE)$items
  }

  item_par_by_scale <- split(data.frame(item_par), data@itemmap[[config@scale_id]])
  n_scale <- length(item_par_by_scale)

  if (!all(min_score %in% c(0, 1))) {
    stop("min_score must contain only 0 or 1")
  }
  if (length(min_score) == 1) {
    if (n_scale > 1) {
      min_score <- rep(min_score, n_scale + 1)
    }
  } else if (length(min_score) != n_scale + 1) {
    stop(sprintf("length of min_score must be either 1 or %i", n_scale + 1))
  }

  rsss <- function(ipar, base0) {
    theta <- seq(min_theta, max_theta, by = inc)
    nq <- length(theta)
    NCAT <- rowSums(!is.na(ipar))
    maxCat <- max(NCAT)
    DISC <- ipar[, 1]
    CB <- ipar[, 2:maxCat]
    ni <- dim(ipar)[1]
    pp <- array(0, c(nq, ni, maxCat))

    for (i in 1:ni) {
      ps <- matrix(0, nq, NCAT[i] + 1)
      ps[, 1] <- 1
      ps[, NCAT[i] + 1] <- 0

      for (k in 1:(NCAT[i] - 1)) {
        ps[, k + 1] <- 1 / (1 + exp(-DISC[i] * (theta - CB[i, k])))
      }
      for (k in 1:NCAT[i]) {
        pp[, i, k] <- ps[, k] - ps[, k + 1]
      }
    }

    min_raw_score <- 0                                 # minimum obtainable raw score
    max_raw_score <- sum(NCAT) - ni                    # maximum obtainable raw score
    n_score       <- max_raw_score - min_raw_score + 1 # number of score points
    inv_TCC       <- numeric(n_score)                  # initialize TCC scoring table
    raw_score     <- min_raw_score:max_raw_score       # raw scores
    LH            <- matrix(0, nq, n_score)            # initialize distribution of summed scores

    ncat <- NCAT[1]
    max_score <- 0
    LH[, 1:ncat] <- pp[, 1, 1:ncat]

    idx <- ncat

    for (i in 2:ni) {
      ncat      <- NCAT[i]                # number of categories for item i
      max_score <- ncat - 1               # maximum score for item i
      score     <- 0:max_score            # score values for item i
      prob      <- pp[, i, 1:ncat]        # category probabilities for item i
      pLH       <- matrix(0, nq, n_score) # place holder for LH

      for (k in 1:ncat) {
        for (h in 1:idx) {
          sco <- raw_score[h] + score[k]
          position <- which(raw_score == sco)
          pLH[, position] <- pLH[, position] + LH[, h] * prob[, k]
        }
      }

      idx <- idx + max_score
      LH <- pLH
    }

    scale_score <- numeric(n_score) # score table for EAP
    SE          <- numeric(n_score) # SE for EAP
    prior       <- dnorm((theta - prior_mean) / prior_sd)
    posterior   <- LH * prior
    den         <- colSums(posterior)
    den         <- matrix(rep(den, rep(nq, n_score)), nq, n_score)
    posterior   <- posterior / den

    for (j in 1:n_score) {
      scale_score[j] <- sum(posterior[, j] * theta) / sum(posterior[, j])                   # EAP
      SE[j] <- sqrt(sum(posterior[, j] * (theta - scale_score[j])^2) / sum(posterior[, j])) # EAP
    }

    if (!base0) {
      raw_score <- raw_score + ni
    }
    if (t_score) {
      scale_score <- round(scale_score * 10 + 50, 1)
      SE          <- round(SE * 10, 1)
    }

    rsss_table <- data.frame(Raw = raw_score, Scale = scale_score, SE)

    return(rsss_table)
  }

  if (n_scale == 1) {
    score_table <- rsss(item_par, min_score == 0)
    return(score_table)
  } else if (n_scale > 1) {
    score_table <- vector(mode = "list", length = n_scale + 1)

    for (s in 1:n_scale) {
      score_table[[s]] <- rsss(item_par_by_scale[[s]], min_score[s] == 0)
    }

    score_table[[n_scale + 1]] <- rsss(item_par, min_score[n_scale + 1] == 0)
    names(score_table) <- c(names(item_par_by_scale), "combined")

    return(score_table)
  }
}
