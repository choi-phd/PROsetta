#' @include configPROsetta.R
NULL

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
#'   \item \code{itemmap_asq} Item map describing the items in each instrument. A data frame with 40 rows and 9 variables.
#'   \itemize{
#'     \item \code{item_order} Item index. Refers to column \code{item_order} in anchor item attributes.
#'     \item \code{instrument} Instrument index.
#'     \item \code{item_id} Item ID. Refers to column \code{prosettaid} in \code{response_asq} data.
#'     \item \code{item_name} New item names to be used in mapping.
#'     \item \code{ncat} The number of response categories.
#'     \item \code{min_score} The minimum score of the item.
#'     \item \code{reverse} 1 indicates a reverse-scored item.
#'     \item \code{scores} A vector representing all possible score values from the item.
#'   }
#'   \item \code{anchor_asq} Anchor item attributes. A data frame with 29 rows and 8 variables.
#'   \itemize{
#'     \item \code{item_order} Item index.
#'     \item \code{item_id} Item ID, same to those in the response data above.
#'     \item \code{a} The discrimination (slope) parameter.
#'     \item \code{cb1 - cb4} The boundaries between each category-pair.
#'     \item \code{ncat} The number of response categories.
#'   }
#'
#' }
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_asq
#' @name dataset_asq
#' @aliases response_asq itemmap_asq anchor_asq data_asq
#' @examples
#' \dontrun{
#' response_asq
#' itemmap_asq
#' anchor_asq
#' data_asq
#' }
#'
#' data_asq <- loadData(
#'   response = response_asq,
#'   itemmap  = itemmap_asq,
#'   anchor   = anchor_asq
#' )
NULL



#' DEP dataset
#'
#' This dataset is associated with the following objects:
#'
#' \itemize{
#'   \item \code{response_dep} Raw response matrix. A data frame with 747 rows and 49 variables.
#'   \itemize{
#'     \item \code{prosettaid}. Sample IDs.
#'     \item \code{EDDEP04 -- EDDEP54}. Response to anchor items.
#'     \item \code{CESD1 -- CESD20}. Response to development items.
#'   }
#'   \item \code{itemmap_dep} Item map describing the items in each instrument. A data frame with 48 rows and 9 variables.
#'   \itemize{
#'     \item \code{item_order} Item index. Refers to column \code{item_order} in anchor item attributes.
#'     \item \code{instrument} Instrument index.
#'     \item \code{item_id} Item ID. Refers to column \code{prosettaid} in \code{response_asq} data.
#'     \item \code{item_name} New item names to be used in mapping.
#'     \item \code{ncat} The number of response categories.
#'     \item \code{min_score} The minimum score of the item.
#'     \item \code{reverse} 1 indicates a reverse-scored item.
#'     \item \code{scores} A vector representing all possible score values from the item.
#'   }
#'   \item \code{anchor_dep} Anchor item attributes. A data frame with 28 rows and 8 variables.
#'   \itemize{
#'     \item \code{item_order} Item index.
#'     \item \code{item_id} Item ID, same to those in the response data above.
#'     \item \code{a} The discrimination (slope) parameter.
#'     \item \code{cb1 - cb4} The boundaries between each category-pair.
#'     \item \code{ncat} The number of response categories.
#'   }
#' }
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_dep
#' @name dataset_dep
#' @aliases response_dep anchor_dep itemmap_dep data_dep
#' @examples
#' \dontrun{
#' response_dep
#' itemmap_dep
#' anchor_dep
#' data_dep
#' }
#'
#' data_dep <- loadData(
#'   response = response_dep,
#'   itemmap  = itemmap_dep,
#'   anchor   = anchor_dep
#' )
NULL
