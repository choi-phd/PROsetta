library(shiny, quietly = TRUE)
library(shinythemes, quietly = TRUE)
library(shinyWidgets, quietly = TRUE)
suppressPackageStartupMessages(library(shinyjs, quietly = TRUE, warn.conflicts = FALSE))
library(DT, quietly = TRUE, warn.conflicts = FALSE)
library(PROsetta)

css_y <- "overflow-y:scroll; max-height: 65vh"
pre_icon    <- list(yes = icon("brush"), no = icon("brush"))
solver_icon <- list(yes = icon("drafting-compass"), no = icon("drafting-compass"))

verifyText <- function(arg_text) {
  txt = gsub("[^0-9\\., \\-]", "", arg_text) # Limits eval to only accept legit inputs
  return(txt == arg_text)
}

toggleSolverButtons <- function(enable, session) {
  button_ids <- c("rundescriptive", "runcalibration", "runlinking", "runequating")
  for (id in button_ids) {
    shinyjs::toggleState(id, enable)
    if (enable) {
      updateCheckboxGroupButtons(
        session = session,
        inputId = id,
        selected = character(0)
      )
    }
  }
}

updateTabSet <- function(tabset, new_tabset = NULL, add_tabset = NULL, session) {

  i1 <- 11:13
  i2 <- 21:24
  i3 <- 31:35
  i4 <- 41:43
  i5 <- 51:52
  is <- list(i1, i2, i3, i4, i5)

  if (!is.null(add_tabset)) {
    new_tabset <- unique(c(tabset, add_tabset))
  }
  if (!is.null(new_tabset)) {
    if (identical(tabset, new_tabset)) {
      tabset <- 1:5
    }
    old_tabs <- do.call(c, is[tabset])
    new_tabs <- do.call(c, is[new_tabset])
  }

  for (i in old_tabs[!(old_tabs %in% new_tabs)]) {
    hideTab("tabs", target = as.character(i))
  }

  if (!is.null(new_tabs)) {
    for (i in new_tabs[!(new_tabs %in% old_tabs)]) {
      showTab("tabs", target = as.character(i))
    }
  }

  updateCheckboxGroupButtons(
    session = session,
    inputId = "tabvisibility",
    selected = as.character(new_tabset)
  )

  return(new_tabset)
}

getDataStatusMsg <- function(ok) {
  if (ok) {
    tmp <- "Files OK."
  } else {
    tmp <- "Error: the files are not in the correct format, or the specified IDs are not in the files."
  }
  return(tmp)
}

parseObject <- function(arg_object, digits = NULL){
  if (is.null(arg_object)) return(NULL)
  if (inherits(arg_object, "matrix")) {
    arg_object <- as.data.frame(arg_object)
  }
  if (!is.null(digits)) {
    for (i in 1:dim(arg_object)[2]) {
      if (inherits(arg_object[, i], "numeric")) {
        if (any(arg_object[, i] %% 1 != 0)) {
          arg_object[, i] <- sprintf("%.*f", digits, arg_object[, i])
        }
      }
    }
  }

  return(arg_object)
}

is_first_assignment <- TRUE
assignObject <- function(objname, obj, desc){
  if (is_first_assignment){
    is_first_assignment <<- FALSE
    message("\nRefresh the environment tab to see the objects in the list.")
  }
  assign(objname, obj, envir = .GlobalEnv)
  pad <- paste0(rep(" ", 48 - nchar(desc)), collapse = "")
  tmp <- paste0(desc, pad, "assigned to : ", objname)
  message(tmp)
}

loadDataFromShiny <- function(input, guess = FALSE) {
  if (!guess) {
    d <- loadData(
      response  = input$response_file$datapath,
      itemmap   = input$itemmap_file$datapath,
      anchor    = input$anchor_file$datapath,
      item_id   = input$item_id,
      person_id = input$person_id,
      scale_id  = input$scale_id)
  } else {
    d <- loadData(
      response  = input$response_file$datapath,
      itemmap   = input$itemmap_file$datapath,
      anchor    = input$anchor_file$datapath)
  }
  return(d)
}

getPath <- function(fn) {
  tmpdir <- tempdir()
  tmp <- file.path(tmpdir, fn)
  tmp <- normalizePath(tmp, mustWork = FALSE)
  return(tmp)
}

updateLogs <- function(v, newlog) {
  v$logs     <- c(v$logs, newlog)
  v$logstext <- paste0(v$logs, collapse = "\n")
  return(v)
}

validateData <- function(v, input, session) {

  if (!is.null(input$anchor_file) & !is.null(input$response_file) & !is.null(input$itemmap_file)) {

    v$inputdata <- try(loadDataFromShiny(input, guess = FALSE))

    assignObject("shiny_data", v$inputdata, "PROsetta_data object")

    v$data_exists <- inherits(v$inputdata, "PROsetta_data")
    v <- updateLogs(v, getDataStatusMsg(v$data_exists))

    if (!v$data_exists) {
      v$inputdata <- try(loadDataFromShiny(input, guess = TRUE))
      v$data_exists <- inherits(v$inputdata, "PROsetta_data")
      v <- updateLogs(v, "Attempting to determine IDs..")

      if (v$data_exists) {
        updateTextInput(session, "item_id", value = cfg@item_id)
        updateTextInput(session, "person_id", value = cfg@person_id)
        updateTextInput(session, "scale_id", value = cfg@scale_id)
        v <- updateLogs(v, getDataStatusMsg(v$data_exists))
      } else {
        v <- updateLogs(v, "Error: IDs could not be determined.")
      }

    } else {
      v$active_tabset <- updateTabSet(v$active_tabset, add_tabset = 1, session = session)
    }

    if (v$data_exists) {
      updateSliderInput(
        session, "item_id_to_plot",
        min = 1,
        max = ncol(getResponse(v$inputdata))
      )
    }

    toggleSolverButtons(v$data_exists, session)

  }

}
