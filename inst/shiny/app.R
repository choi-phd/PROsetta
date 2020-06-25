library(shiny, quietly = TRUE)
library(shinythemes, quietly = TRUE)
library(shinyWidgets, quietly = TRUE)
suppressPackageStartupMessages(library(shinyjs, quietly = TRUE, warn.conflicts = FALSE))
library(DT, quietly = TRUE, warn.conflicts = FALSE)
library(PROsetta)

css_y <- "overflow-y:scroll; max-height: 65vh"
solver_icon <- list(yes = icon("drafting-compass"), no = icon("drafting-compass"))

ui <- fluidPage(
  theme = shinytheme("lumen"),
  shinyjs::useShinyjs(),
  includeCSS("styles.css"),
  titlePanel("PROsetta - scale linking/equating"),

  sidebarLayout(
    sidebarPanel(

      dropdownButton(

        h3(""),

        fileInput("response_file", buttonLabel = "Response data", label = NULL),
        fileInput("itemmap_file" , buttonLabel = "Item map", label = NULL),
        fileInput("anchor_file"  , buttonLabel = "Anchor data", label = NULL),

        textInput(label = "Item ID", inputId = "item_id", value = "item_id"),
        textInput(label = "Person ID", inputId = "person_id", value = "prosettaid"),
        textInput(label = "Scale ID", inputId = "scale_id", value = "instrument"),

        circle = FALSE, status = "primary",
        icon = icon("file-import"), width = "100%",

        label = "Load files"
      ),

      h3("Linking method"),

      radioGroupButtons(
        inputId = "linking_type",
        choices = c("MM", "MS", "HB", "SL", "FIXEDPAR", "NONE"),
        justified = TRUE
      ),

      h3(""),

      dropdownButton(
        inputId = "simulation_dropdown",

        textInput(label = "# of iterations", inputId = "n_iter", value = "1000"),

        circle = FALSE,
        icon = icon("gear"), width = "100%",
        label = "Options"
      ),

      checkboxGroupButtons(choices = c("Run descriptive")      , inputId = "rundescriptive", status = "primary", justified = TRUE, checkIcon = solver_icon),
      checkboxGroupButtons(choices = c("Run calibration")      , inputId = "runcalibration", status = "primary", justified = TRUE, checkIcon = solver_icon),
      checkboxGroupButtons(choices = c("Run parameter linking"), inputId = "runlinking",     status = "primary", justified = TRUE, checkIcon = solver_icon),
      checkboxGroupButtons(choices = c("Run score equating")   , inputId = "runequating",    status = "primary", justified = TRUE, checkIcon = solver_icon),

      h3(""),

      dropdownButton(
        label = "Tab visibility", inputId = "tabvisibility_dropdown",
        circle = FALSE, width = "100%", icon = icon("thumbtack"),
        checkboxGroupButtons(
          inputId = "tabvisibility",
          choiceNames = c("Raw data", "Basic stats", "Calibration", "Linking", "Equating"),
          choiceValues = 1:5,
          justified = TRUE,
          selected = c(1,2)
        ),
        textInput(label = "Item ID to plot", inputId = "item_id_to_plot", value = "1"),
        textInput(label = "Scale ID to display crosswalk tables", inputId = "id_cross", value = "1")
      ),

      downloadButton("export_data", "Export visible tabs"),

      dropdownButton(
        label = "Close app", inputId = "closeapp_dropdown",
        circle = FALSE, width = "100%", icon = icon("thumbtack"),
        h3("Are you sure?"),
        checkboxGroupButtons(
          inputId = "closeapp",
          choices = c("Yes", "No"),
          justified = TRUE
        )
      ),

      width = 3
    ),

    mainPanel(
      verbatimTextOutput("textoutput", placeholder = TRUE),
      hr(),
      tabsetPanel(id = "tabs",
        tabPanel("Response data",          value = 11, DTOutput("response_data"),                style = css_y),
        tabPanel("Item map data",          value = 12, DTOutput("itemmap_data"),                 style = css_y),
        tabPanel("Anchor data",            value = 13, DTOutput("anchor_data"),                  style = css_y),
        tabPanel("Frequency table",        value = 21, DTOutput("freqtable"),                    style = css_y),
        tabPanel("Descriptives",           value = 22, DTOutput("desctable"),                    style = css_y),
        tabPanel("Classical",              value = 23, verbatimTextOutput("classical"),          style = css_y),
        tabPanel("Classical (omega)",      value = 24, verbatimTextOutput("classical2"),         style = css_y),
        tabPanel("Calibration result",     value = 31, DTOutput("calib_params"),                 style = css_y),
        tabPanel("Item fit plot",          value = 32, plotOutput("plot_itemfit", width = "100%", height = "65vh"),  style = css_y),
        tabPanel("Item info",              value = 33, plotOutput("plot_iteminfo", width = "100%", height = "65vh"), style = css_y),
        tabPanel("Item fit table",         value = 34, DTOutput("table_itemfit"),                style = css_y),
        tabPanel("Crosswalk table (from calibration)", value = 35,
                 DTOutput("crosswalk_calibration"), style = css_y),
        tabPanel("Linking constants",      value = 41, verbatimTextOutput("linking_constants"),  style = css_y),
        tabPanel("Linked parameters", value = 42, DTOutput("table_linked_params"),     style = css_y),
        tabPanel("Crosswalk table (from linking)", value = 43,
                 DTOutput("crosswalk_linking"), style = css_y),
        tabPanel("Equating",               value = 51, verbatimTextOutput("equating_constants"), style = css_y),
        tabPanel("Concordance table",      value = 52, DTOutput("table_concordance"),            style = css_y)
      ),

      width = 9
    )
  )
)

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

    message("t1")

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
    toggleSolverButtons(v$data_exists, session)
  }

}

server <- function(input, output, session) {
  v <- reactiveValues(
    data_exists   = FALSE,
    active_tabset = c(1,2))

  toggleSolverButtons(FALSE, session)

  observeEvent(input$tabvisibility, {
    v$active_tabset <- updateTabSet(v$active_tabset, new_tabset = as.numeric(input$tabvisibility), session = session)
  }, ignoreNULL = FALSE)

  observeEvent(input$anchor_file, {
    if (!is.null(input$anchor_file)) {
      v$anchor_data <- read.csv(input$anchor_file$datapath)
      v <- updateLogs(v, sprintf("Anchor data imported: %i items", dim(v$anchor_data)[1]))
    }
    v <- validateData(v, input, session)
  })

  observeEvent(input$response_file, {
    if (!is.null(input$response_file)) {
      v$response_data <- read.csv(input$response_file$datapath)
      v <- updateLogs(v, sprintf("Response data imported: %i cases * %i items", dim(v$response_data)[1], dim(v$response_data)[2] - 1))
    }
    v <- validateData(v, input, session)
  })

  observeEvent(input$itemmap_file, {
    if (!is.null(input$itemmap_file)) {
      v$itemmap_data <- read.csv(input$itemmap_file$datapath)
      v$n.items <- dim(v$itemmap_data)[1]
      v <- updateLogs(v, sprintf("Item map imported: %i items", v$n.items))
    }
    v <- validateData(v, input, session)
  })

  observeEvent(input$rundescriptive, {

    toggleSolverButtons(FALSE, session)

    v$text <- "Running.."
    v$time <- Sys.time()

    v$inputdata <- loadDataFromShiny(input)
    assignObject("shiny_data", v$inputdata, "PROsetta_data object")
    v$freqtable <- runFrequency(v$inputdata)
    assignObject("shiny_freq", v$freqtable, "Frequency table tab")
    v$desctable <- runDescriptive(v$inputdata)
    assignObject("shiny_desc", v$desctable, "Descriptives tab")

    tmp <- try(runClassical(v$inputdata, omega = TRUE, fm = "ml"))
    v$classical  <- tmp$alpha$combined
    assignObject("shiny_alpha", v$classical, "Classical tab")
    v$classical2 <- tmp$omega$combined
    assignObject("shiny_omega", v$classical2, "Classical (omega) tab")

    v$time <- Sys.time() - v$time
    v <- updateLogs(v, sprintf("Done in %7.3fs : run descriptives", v$time))

    v$active_tabset <- updateTabSet(v$active_tabset, add_tabset = 2, session = session)
    toggleSolverButtons(TRUE, session)

  })



  observeEvent(input$runcalibration, {

    toggleSolverButtons(FALSE, session)

    progress <- Progress$new(session)
    on.exit(progress$close())
    progress$set(message = 'Computing..',
                 detail = 'This may take a while.')

    v$time <- Sys.time()

    v$inputdata <- loadDataFromShiny(input)
    assignObject("shiny_data", v$inputdata, "PROsetta_data object")

    updateSliderTextInput(
      session = session,
      inputId = "item_id_to_plot",
      choices = seq(1, v$n.items),
      selected = min(v$item_id_to_plot, v$n.items)
    )

    if (input$linking_type == "FIXEDPAR") {
      do_fixedpar <- TRUE
      calib_type  <- "fixed parameter using anchor data"
    } else {
      do_fixedpar <- FALSE
      calib_type  <- "free estimation without linking"
    }

    v$calib <- try(runCalibration(v$inputdata, fixedpar = do_fixedpar, technical = list(NCYCLES = as.numeric(input$n_iter))))

    assignObject("shiny_calib", v$calib, "Calibration result (full object)")
    v$calib_params <- mirt::coef(v$calib, IRTpars = TRUE, simplify = TRUE)$items
    assignObject("shiny_params", v$calib_params, "Calibration result tab")
    v$plot_itemfit <- mirt::itemfit(v$calib, empirical.plot = v$item_id_to_plot)
    assignObject("shiny_itemfit", v$plot_itemfit, "Item fit plot tab")
    v$plot_iteminfo <- mirt::itemplot(v$calib, item = v$item_id_to_plot, type = "info")
    assignObject("shiny_iteminfo", v$plot_iteminfo, "Item info tab")

    tmp <- try(mirt::itemfit(v$calib, "S_X2", na.rm = TRUE), silent = T)
    if (class(tmp)[1] == "try-error"){
      tmp <- try(mirt::itemfit(v$calib, "S_X2"))
    }

    v$table_itemfit <- tmp
    assignObject("shiny_itemfittable", v$table_itemfit, "Item fit table tab")

    v$crosswalk_calibration <- runRSSS(v$inputdata, v$calib)
    assignObject("shiny_crosswalk_calibration", v$crosswalk_calibration, "Crosswalk (calibration) tab")

    v$time <- Sys.time() - v$time
    v <- updateLogs(v, sprintf("Done in %7.3fs : run calibration (%s)", v$time, calib_type))

    v$active_tabset <- updateTabSet(v$active_tabset, add_tabset = 3, session = session)
    toggleSolverButtons(TRUE, session)

  })



  observeEvent(input$runlinking, {

    toggleSolverButtons(FALSE, session)

    if (!(input$linking_type %in% c("MM", "MS", "HB", "SL", "FIXEDPAR"))) {
      v <- updateLogs(v, "Linking method must be one of the following: 'MM', 'MS', 'HB', 'SL', 'FIXEDPAR'.")
      break
    }

    progress <- Progress$new(session)
    on.exit(progress$close())
    progress$set(message = 'Computing..',
                  detail = 'This may take a while.')

    v$text <- "Running.."
    v$time <- Sys.time()

    v$inputdata <- loadDataFromShiny(input)
    assignObject("shiny_data", v$inputdata, "PROsetta_data object")

    v$linking <- runLinking(v$inputdata, method = input$linking_type, technical = list(NCYCLES = 1000))
    assignObject("shiny_link", v$linking, "Linking result (full object)")

    v$linking_constants <- v$linking$constants
    assignObject("shiny_link_constants", v$linking_constants, "Linking constants tab")

    v$linked_params <- v$linking$ipar_linked
    assignObject("shiny_linked_params", v$linked_params, "Linked parameters tab")

    v$crosswalk_linking <- runRSSS(v$inputdata, v$linking)
    assignObject("shiny_crosswalk_linking", v$crosswalk_linking, "Crosswalk (linking) tab")

    v$time <- Sys.time() - v$time
    v <- updateLogs(v, sprintf("Done in %7.3fs : run parameter linking (%s)", v$time, input$linking_type))

    v$active_tabset <- updateTabSet(v$active_tabset, add_tabset = 4, session = session)
    toggleSolverButtons(TRUE, session)
  })




  observeEvent(input$runequating, {

    toggleSolverButtons(FALSE, session)

    v$text <- "Running.."
    v$time <- Sys.time()

    v$inputdata <- loadDataFromShiny(input)
    assignObject("shiny_data", v$inputdata, "PROsetta_data object")

    v$out_equ <- runEquateObserved(v$inputdata, scale_from = 2, scale_to = 1, eq_type = "equipercentile", smooth = "loglinear")
    assignObject("shiny_eq", v$out_equ, "Equating tab")
    v$concordance <- v$out_equ$concordance
    assignObject("shiny_concordance", v$concordance, "Concordance table")

    v$time <- Sys.time() - v$time
    v <- updateLogs(v, sprintf("Done in %7.3fs : run equating", v$time))

    v$active_tabset <- updateTabSet(v$active_tabset, add_tabset = 5, session = session)
    toggleSolverButtons(TRUE, session)

  })



  observeEvent(input$item_id_to_plot, {
    if (verifyText(input$item_id_to_plot)) {
      eval(parse(text = sprintf("item_id_to_plot <- c(%s)[1]", input$item_id_to_plot)))
      item_id_to_plot <- min(item_id_to_plot, v$n.items)
      item_id_to_plot <- max(1, item_id_to_plot)
      v$item_id_to_plot <- item_id_to_plot
      if (is.null(v$calib)) {
        return()
      }
      v$plot_itemfit <- mirt::itemfit(v$calib, empirical.plot = v$item_id_to_plot)
      assignObject("shiny_itemfit", v$plot_itemfit, "Item fit plot tab")
      v$plot_iteminfo <- mirt::itemplot(v$calib, item = v$item_id_to_plot, type = "info")
      assignObject("shiny_iteminfo", v$plot_iteminfo, "Item info tab")
    }
  })

  observeEvent(input$closeapp, {
    if ("Yes" %in% input$closeapp) {
      stopApp()
    }
    if ("No" %in% input$closeapp) {
      updateCheckboxGroupButtons(
        session = session,
        inputId = "closeapp",
        selected = character(0)
      )
      toggleDropdownButton(inputId = "closeapp_dropdown")
    }
  })

  output$textoutput            <- renderText(parseObject(v$logstext))

  opts = list(
    pageLength = 100,
    columnDefs = list(list(className = 'dt-right', targets = "_all"))
  )

  output$response_data         <- renderDT(parseObject(v$response_data),      options = opts)
  output$itemmap_data          <- renderDT(parseObject(v$itemmap_data),       options = opts)
  output$anchor_data           <- renderDT(parseObject(v$anchor_data, 3),     options = opts)

  output$freqtable             <- renderDT(parseObject(v$freqtable),          options = opts)
  output$desctable             <- renderDT(parseObject(v$desctable, 3),       options = opts)
  output$classical             <- renderPrint(parseObject(v$classical))
  output$classical2            <- renderPrint(parseObject(v$classical2))

  output$calib_params          <- renderDT(parseObject(v$calib_params, 3),    options = opts)
  output$plot_itemfit          <- renderPlot(parseObject(v$plot_itemfit))
  output$plot_iteminfo         <- renderPlot(parseObject(v$plot_iteminfo))
  output$table_itemfit         <- renderDT(parseObject(v$table_itemfit, 3),   options = opts)
  output$crosswalk_calibration <- renderDT(parseObject(v$crosswalk_calibration[[input$id_cross]], 3), options = opts)

  output$linking_constants     <- renderPrint(parseObject(v$linking_constants))
  output$table_linked_params   <- renderDT(parseObject(v$linked_params, 3),   options = opts)
  output$crosswalk_linking     <- renderDT(parseObject(v$crosswalk_linking[[input$id_cross]], 3), options = opts)

  output$equating_constants    <- renderPrint(parseObject(v$out_equ))
  output$table_concordance     <- renderDT(parseObject(v$concordance, 3),     options = opts)

  output$export_data <- downloadHandler(
    filename = function() {
      paste0("data-", Sys.Date(), ".zip")
    },
    content = function(fname) {

      fs     <- c()

      for (i in v$active_tabset) {
        if (i == 1) {

          if (!is.null(v$anchor_data)) {
            path <- getPath("raw_data_anchor.csv")
            fs <- c(fs, path)
            write.csv(v$anchor_data, path, row.names = F)
          }
          if (!is.null(v$response_data)) {
            path <- getPath("raw_data_response.csv")
            fs <- c(fs, path)
            write.csv(v$response_data, path, row.names = F)
          }
          if (!is.null(v$itemmap_data)) {
            path <- getPath("raw_data_itemmap.csv")
            fs <- c(fs, path)
            write.csv(v$itemmap_data, path, row.names = F)
          }

        }
        if (i == 2) {

          if (!is.null(v$freqtable)) {
            path <- getPath("basic_frequency.csv")
            fs <- c(fs, path)
            write.csv(v$freqtable, path)
          }
          if (!is.null(v$desctable)) {
            path <- getPath("basic_descriptive.csv")
            fs <- c(fs, path)
            write.csv(v$desctable, path)
          }
          if (!is.null(v$classical)) {
            path <- getPath("basic_reliability_alpha.txt")
            fs <- c(fs, path)
            tmp <- paste0(capture.output(v$classical), collapse = "\n")
            write(tmp, path)
          }
          if (!is.null(v$classical2)) {
            path <- getPath("basic_reliability_omega.txt")
            fs <- c(fs, path)
            tmp <- paste0(capture.output(v$classical2), collapse = "\n")
            write(tmp, path)
          }

        }
        if (i == 3) {

          if (!is.null(v$calib_params)) {
            path <- getPath("calib_params.csv")
            fs <- c(fs, path)
            write.csv(v$calib_params, path)
          }

          n.items <- dim(v$response_data)[2] - 1

          if (!is.null(v$calib)) {

            path <- getPath("calib_itemfit.pdf")
            fs <- c(fs, path)
            pdf(path)
            for (id in 1:n.items) {
              p <- mirt::itemfit(v$calib, empirical.plot = id)
              print(p)
            }
            dev.off()

            path <- getPath("calib_iteminfo.pdf")
            fs <- c(fs, path)
            pdf(path)
            for (id in 1:n.items) {
              p <- mirt::itemplot(v$calib, item = id, type = "info")
              print(p)
            }
            dev.off()

          }

          if (!is.null(v$table_itemfit)) {
            path <- getPath("calib_fit.csv")
            fs <- c(fs, path)
            write.csv(v$table_itemfit, path, row.names = F)
          }
          if (!is.null(v$crosswalk_calibration)) {
            for (i in 1:length(v$crosswalk_calibration)) {
              path <- getPath(sprintf("crosswalk_calibration_%s.csv", names(v$crosswalk_calibration)[i]))
              fs <- c(fs, path)
              write.csv(v$crosswalk_calibration[[i]], path, row.names = F)
            }
          }

        }

        if (i == 4) {

          if (!is.null(v$linking_constants)) {
            path <- getPath("linking_constants.csv")
            fs <- c(fs, path)
            write.csv(v$linking_constants, path)
          }
          if (!is.null(v$transformed_params)) {
            path <- getPath("transformed_params.csv")
            fs <- c(fs, path)
            write.csv(v$transformed_params, path)
          }
          if (!is.null(v$crosswalk_linking)) {
            for (i in 1:length(v$crosswalk_linking)) {
              path <- getPath(sprintf("crosswalk_linking_%s.csv", names(v$crosswalk_linking)[i]))
              fs <- c(fs, path)
              write.csv(v$crosswalk_linking[[i]], path, row.names = F)
            }
          }

        }

        if (i == 5) {

          if (!is.null(v$outequateequipercentile)) {
            path <- getPath("equating_constants.txt")
            fs <- c(fs, path)
            tmp <- paste0(capture.output(v$outequateequipercentile), collapse = "\n")
            write(tmp, path)
          }

          if (!is.null(v$concordance)) {
            path <- getPath("equating_concordance.csv")
            fs <- c(fs, path)
            write.csv(v$concordance, path, row.names = F)
          }

        }
      }

      if (length(fs) > 0) {
        zip(zipfile = fname, files = fs, flags = "-j")
        file.remove(fs)
      }

    },
    contentType = "application/zip"
  )

}

shinyApp(ui = ui, server = server)
