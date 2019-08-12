#' ASQ dataset
#'
#' This dataset is associated with the following objects:
#'
#' \itemize{
#'   \item \code{response_asq} Raw response matrix. A data frame with 751 rows and 41 variables.
#'   \itemize{
#'     \item \code{prosettaid}. Sample IDs.
#'     \item \code{EDANX01 -- EDANX55}. Response to anchor items.
#'     \item \code{MASQ1 -- MASQ11}. Response to development items.
#'   }
#'
#'   \item \code{anchor_asq} Item attributes. A data frame with 29 rows and 8 variables.
#'   \itemize{
#'     \item \code{item_order} Item index.
#'     \item \code{item_id} Item ID, same to those in \code{response_asq} data.
#'     \item \code{a} The discrimination (slope) parameter.
#'     \item \code{cb1 - cb4} The boundaries between each category-pair.
#'     \item \code{NCAT} The number of categories.
#'   }
#'
#'   \item \code{itemmap_asq} Item map describing the items in each instrument. A data frame with 40 rows and 9 variables.
#'   \itemize{
#'     \item \code{item_order} Item index. Refers to column \code{item_order} in item attributes.
#'     \item \code{instrument} Instrument index.
#'     \item \code{item_id} Item ID. Refers to column \code{prosettaid} in \code{response_asq} data.
#'     \item \code{item_name} New item names to be used in mapping.
#'     \item \code{NCAT} Number of categories.
#'     \item \code{minScore} Minimum score of the item.
#'     \item \code{reverse} 1 indicates a reverse-scored item. Accepts 0 or 1.
#'     \item \code{scores} A vector representing all possible score values from an item.
#'     \item \code{NCATc} (placeholder)
#'   }
#'
#'   \item \code{cfg_asq}
#' }
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_asq
#' @name dataset_asq
#' @aliases response_asq anchor_asq itemmap_asq cfg_asq
#' @examples
#' \dontrun{
#' data(response_asq)
#' data(anchor_asq)
#' data(itemmap_asq)
#' }
NULL
