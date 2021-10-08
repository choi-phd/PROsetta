#' @include configPROsetta.R
NULL

#' ASQ dataset
#'
#' This dataset is associated with the following objects:
#'
#' \itemize{
#'   \item \code{\link{response_asq}} a \code{\link{data.frame}} containing raw response data of 751 participants and 41 variables.
#'   \itemize{
#'     \item \code{prosettaid}. participant IDs.
#'     \item \code{EDANX01 -- MASQ11}. response to items.
#'   }
#'   \item \code{\link{itemmap_asq}} a \code{\link{data.frame}} containing the item map, describing the items in each instrument.
#'   \itemize{
#'     \item \code{item_order} item numeric IDs. This column refers to the column \code{item_order} in anchor item attributes.
#'     \item \code{instrument} the instrument ID that each item belongs to.
#'     \item \code{item_id} item ID strings. This column refers to column names in raw response data, excluding the participant ID column.
#'     \item \code{item_name} new item ID strings to be used in the combined scale.
#'     \item \code{ncat} the number of response categories.
#'   }
#'   \item \code{\link{anchor_asq}} a \code{\link{data.frame}} containing anchor item parameters for 29 items.
#'   \itemize{
#'     \item \code{item_order} item numeric IDs.
#'     \item \code{item_id} item ID strings. This column refers to column names in raw response data, excluding the participant ID column.
#'     \item \code{a} the discrimination (slope) parameter for the graded response model.
#'     \item \code{cb1 - cb4} the boundaries between each category-pair for the graded response model.
#'     \item \code{ncat} the number of response categories.
#'   }
#'   \item \code{\link{data_asq}} a \code{\linkS4class{PROsetta_data}} object containing the datasets above. See \code{\link{loadData}} for creating \code{\linkS4class{PROsetta_data}} objects.
#' }
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_asq
#' @name dataset_asq
#' @aliases response_asq itemmap_asq anchor_asq data_asq
#' @examples
#'
#' ## load datasets into a PROsetta_data object
#' data_asq <- loadData(
#'   response = response_asq,
#'   itemmap  = itemmap_asq,
#'   anchor   = anchor_asq
#' )
#'
#' ## run descriptive statistics
#' runDescriptive(data_asq)
#'
#' ## run item parameter calibration on the response data, linking to the anchor item parameters
#' runLinking(data_asq, method = "FIXEDPAR")
NULL

#' DEP dataset
#'
#' This dataset is associated with the following objects:
#'
#' \itemize{
#'   \item \code{\link{response_dep}} a \code{\link{data.frame}} containing raw response data of 747 participants and 49 variables.
#'   \itemize{
#'     \item \code{prosettaid}. participant IDs.
#'     \item \code{EDDEP04 -- CESD20}. response to items.
#'   }
#'   \item \code{\link{itemmap_dep}} a \code{\link{data.frame}} containing the item map, describing the items in each instrument.
#'   \itemize{
#'     \item \code{item_order} item numeric IDs. This column refers to the column \code{item_order} in anchor item parameters.
#'     \item \code{instrument} the instrument ID that each item belongs to.
#'     \item \code{item_id} item ID strings. This column refers to column names in raw response data, excluding the participant ID column.
#'     \item \code{item_name} new item ID strings to be used in the combined scale.
#'     \item \code{ncat} the number of response categories.
#'   }
#'   \item \code{\link{anchor_dep}} a \code{\link{data.frame}} containing anchor item parameters for 28 items.
#'   \itemize{
#'     \item \code{item_order} item numeric IDs.
#'     \item \code{item_id} item ID strings. This column refers to column names in raw response data, excluding the participant ID column.
#'     \item \code{a} the discrimination (slope) parameter for the graded response model.
#'     \item \code{cb1 - cb4} the boundaries between each category-pair for the graded response model.
#'     \item \code{ncat} the number of response categories.
#'   }
#'   \item \code{\link{data_dep}} a \code{\linkS4class{PROsetta_data}} object containing the datasets above. See \code{\link{loadData}} for creating \code{\linkS4class{PROsetta_data}} objects.
#' }
#'
#' @docType data
#' @keywords datasets
#' @rdname dataset_dep
#' @name dataset_dep
#' @aliases response_dep anchor_dep itemmap_dep data_dep
#' @examples
#'
#' ## load datasets into a PROsetta_data object
#' data_dep <- loadData(
#'   response = response_dep,
#'   itemmap  = itemmap_dep,
#'   anchor   = anchor_dep
#' )
#'
#' ## run descriptive statistics
#' runDescriptive(data_dep)
#'
#' ## run item parameter calibration on the response data, linking to the anchor item parameters
#' runLinking(data_dep, method = "FIXEDPAR")
NULL
