#' @include import.R
NULL

#' Load data from supplied config
#'
#' \code{\link{loadData}} is a data loading function for creating a \code{\linkS4class{PROsetta_data}} object, for performing scale linking/equating in the 'PROsetta' package.
#' \code{\link{loadData}} assumes the response data has been reverse-coded for applicable items.
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
loadData <- function(
  response, itemmap, anchor,
  item_id = NULL, person_id = NULL, scale_id = NULL,
  input_dir = getwd()
) {

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

  response <- sanitizeData(response)
  itemmap  <- sanitizeData(itemmap)
  anchor   <- sanitizeData(anchor)

  colnames(itemmap) <- tolower(colnames(itemmap))

  names_response <- colnames(response)
  names_itemmap  <- colnames(itemmap)

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

  for (s in unique(data@itemmap[[scale_id]])) {
    cor_matrix <- cor(getResponse(data, 1), use = "pairwise.complete.obs")
    reverse_code_check <- apply(cor_matrix, 1, sum) < 0
    if (any(reverse_code_check)) {
      potentially_not_reverse_coded_items <- names(which(reverse_code_check))
      warning(sprintf("Some variables may need reverse-coding: %s", potentially_not_reverse_coded_items))
    }
  }

  if (validObject(data)) {
    return(data)
  }
}
