#' @include example.R
NULL

#' PROsetta
#'
#' \code{\link{PROsetta}} is a caller function to launch a Shiny app locally.
#'
#' @examples
#' if (interactive()) {
#'   PROsetta()
#' }
#'
#' @aliases guiPROsetta
#' @export
PROsetta <- function() {
  app_dir <- system.file("shiny", package = "PROsetta")
  if (app_dir == "") {
    stop("Could not find application directory. Try re-installing `PROsetta`.", call. = FALSE)
  }

  pkgs <- c("shiny", "shinythemes", "shinyWidgets", "shinyjs", "DT")
  tmp <- NULL

  for (pkg in pkgs) {
    if (length(find.package(pkg, quiet = TRUE)) == 0) {
      tmp <- c(tmp, sprintf("'%s'", pkg))
    }
  }

  if (!is.null(tmp)) {
    message("Shiny application requires additional packages.")
    message("Run the following code to install:")
    message("")
    tmp <- paste(tmp, collapse = ", ")
    tmp <- paste0("install.packages(c(", tmp, "))")
    message(tmp)
  } else {
    if (!isNamespaceLoaded("shiny")) {
      attachNamespace("shiny")
    }
    shiny::runApp(app_dir, display.mode = "normal", launch.browser = TRUE)
  }
}

#' @rdname PROsetta
guiPROsetta <- function() {
  .Deprecated("PROsetta", msg = "guiPROsetta() is deprecated. Use PROsetta() instead.")
  PROsetta()
}
