#' Prices of 50,000 round cut diamonds
#'
#' A dataset containing the responses of prices and other attributes of almost 54,000
#'  diamonds. The variables are as follows:
#'
#' \itemize{
#'   \item prosettaid. sample ID
#'   \item EDANX01 -- EDANX55. response to items
#'   \item MASQ1 -- MASQ11. response to items
#' }
#'
#' @docType data
#' @keywords datasets
#' @name response_asq
#' @usage data(response_asq)
#' @format A data frame with 751 rows and 41 variables
NULL

#' Prices of 50,000 round cut diamonds
#'
#' A dataset containing the responses of prices and other attributes of almost 54,000
#'  diamonds. The variables are as follows:
#'
#' \itemize{
#'   \item item_order. index of the item.
#'   \item instrument. index of the instrument.
#'   \item item_id. item ID, same to those in \code{response_asq} data.
#'   \item item_name. new item names to use in mapping.
#'   \item NCAT. number of categories.
#'   \item minScore. minimum score in the item.
#'   \item reverse. \code{1} indicates a reverse item. accepts (0, 1)
#'   \item scores. a vector representing all possible values in an item.
#'   \item NCATc. (placeholder)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name itemmap_asq
#' @usage data(itemmap_asq)
#' @format A data frame with 40 rows and 9 variables
NULL

#' Prices of 50,000 round cut diamonds
#'
#' A dataset containing the responses of prices and other attributes of almost 54,000
#'  diamonds. The variables are as follows:
#'
#' \itemize{
#'   \item item_order. index of the item, same to those used in \code{itemmap_asq} data.
#'   \item item_id. item ID, same to those in \code{response_asq} data.
#'   \item a. the discrimination (slope) parameter.
#'   \item cb1 - cb4. the boundaries between each category-pair.
#'   \item NCAT. the number of categories.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name anchor_asq
#' @usage data(anchor_asq)
#' @format A data frame with 29 rows and 8 variables
NULL
