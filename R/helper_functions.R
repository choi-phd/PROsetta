#' @include core_functions.R
NULL

#' Extract scale-wise response
#'
#' \code{\link{getResponse}} is a helper function for extracting scale-wise response from a \code{\linkS4class{PROsetta_data}} object.
#'
#' @param d a \code{\linkS4class{PROsetta_data}} object.
#' @param scale_id scale IDs to extract response. If \code{all}, use all scale IDs. (default = \code{all})
#' @param person_id if \code{TRUE}, also return person IDs. (default = \code{FALSE})
#'
#' @return \code{\link{getResponse}} returns a \code{\link{data.frame}} containing scale-wise response.
#'
#' @examples
#' getResponse(data_asq)
#' getResponse(data_asq, 1)
#' getResponse(data_asq, 2)
#' getResponse(data_asq, c(1, 2))
#' getResponse(data_asq, c(2, 1))
#' getResponse(data_asq, c(1, 2), person_id = TRUE)
#'
#' @export
getResponse <- function(d, scale_id = "all", person_id = FALSE) {

  subset_idx <- c()
  item_idx   <- c()
  if (length(scale_id) == 1) {
    if (scale_id == "all") {
      item_idx <- d@itemmap[, d@item_id]
    } else {
      item_idx <- getItemNames(d, scale_id)
    }
  } else {
    item_idx <- getItemNames(d, scale_id)
  }

  if (!person_id) {
    resp_data <- d@response[,
      item_idx
    ]
    return(resp_data)
  }

  if (person_id) {
    resp_data <- d@response[,
      c(d@person_id, item_idx)
    ]
    return(resp_data)
  }

}

#' Get item names
#'
#' \code{\link{getItemNames}} is a helper function for extracting item names for a specified scale from a \code{\linkS4class{PROsetta_data}} object.
#'
#' @param d a \code{\linkS4class{PROsetta_data}} object.
#' @param scale_id scale IDs to extract item names.
#'
#' @return \code{\link{getItemNames}} returns a vector containing item names.
#'
#' @examples
#' getItemNames(data_asq, 1)
#' getItemNames(data_asq, 2)
#' getItemNames(data_asq, c(1, 2))
#' getItemNames(data_asq, c(2, 1))
#'
#' @export
getItemNames <- function(d, scale_id) {

  item_idx   <- c()
  for (s in scale_id) {
    subset_idx <- d@itemmap[, d@scale_id] == s
    item_idx   <- c(
      item_idx,
      d@itemmap[subset_idx, d@item_id]
    )
  }

  return(item_idx)

}

#' Get complete data
#'
#' \code{\link{getCompleteData}} is a helper function for performing casewise deletion of missing values.
#'
#' @param data a \code{\linkS4class{PROsetta_data}} object.
#' @param scale the index of the scale to perform casewise deletion. Leave empty or set to "combined" to perform on all scales.
#' @param verbose if \code{TRUE}, print status messages. (default = \code{FALSE})
#'
#' @returns \code{\link{getCompleteData}} returns an updated \code{\linkS4class{PROsetta_data}} object.
#' @examples
#' d <- getCompleteData(data_asq, verbose = TRUE)
#'
#' @export
getCompleteData <- function(data, scale = NULL, verbose = FALSE) {

  validateData(data)

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
    printLog(
      "sanitize",
      sprintf("getCompleteData() removed %s cases with missing responses in %s", n_resp, scale_text),
      verbose
    )
  } else {
    printLog(
      "sanitize",
      sprintf("getCompleteData() did not remove any cases because all %i responses are complete in %s", dim(data@response)[1], scale_text),
      verbose
    )
  }
  return(data)
}
