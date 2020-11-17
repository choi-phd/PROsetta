#' @include core_functions.R
NULL

#' Extract scale-wise response
#'
#' \code{\link{getResponse}} is a helper function to extract scale-wise response from a \code{\linkS4class{PROsetta_data}} object.
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
#' \code{\link{getItemNames}} is a helper function to extract item names for a specified scale from a \code{\linkS4class{PROsetta_data}} object.
#'
#' @param d a \code{\linkS4class{PROsetta_data}} object.
#' @param scale_id scale IDs to extract item names.
#'
#' @return \code{\link{getItemNames}} returns a vector containing item names.
#'
#' @examples
#' idx <- getItemNames(data_asq, 1)
#' data_asq@response[, idx]
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
