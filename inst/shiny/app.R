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
  tags$head(tags$style(HTML("
h3 {
  font-size: 125%;
}
i {
  display: inline-block;
  margin-right: 0.2em;
}
label, .form-group, .progress {
  margin-bottom: 0px;
}
.btn {
  width: 100%;
}

"))),
  titlePanel("PROsetta"),

  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style(type = "text/css", "select { min-width: 100%; max-width: 100%; }"),
        tags$style(type = "text/css", ".span4 { min-width: 100%; max-width: 100%; }"),
        tags$style(type = "text/css", ".well { min-width: 100%; max-width: 100%; }"),
        tags$style(type = "text/css", "#textoutput { background-color: rgba(64,64,64,1); color: cyan; overflow-y:auto; height: 64px; display: flex; flex-direction: column-reverse; }"),
        tags$style(type = "text/css", ".shiny-notification { font-size: 20px; background-color: #404040; color: #fff; }"),
        tags$style(type = "text/css", "#shiny-notification-panel { width: 500px; }")
      ),
      helpText("This is a demo of PROsetta Linking Analysis."),

      dropdownButton(

        h3(""),

        fileInput("anchor_file", buttonLabel = "Anchor data", label = NULL),
        fileInput("response_file", buttonLabel = "Response data", label = NULL),
        fileInput("itemmap_file", buttonLabel = "Item map", label = NULL),

        circle = FALSE, status = "primary",
        icon = icon("file-import"), width = "100%",

        label = "Load files"
      ),

      h3(""),

      radioGroupButtons(
        inputId = "linking_type",
        choices = c("MM", "MS", "HB", "SL", "FIXEDPAR", "NONE"),
        justified = TRUE
      ),


      dropdownButton(
        inputId = "simulation_dropdown",

        textInput(label = "Item ID", inputId = "item_id", value = "item_id"),
        textInput(label = "Person ID", inputId = "person_id", value = "prosettaid"),
        textInput(label = "Scale ID", inputId = "scale_id", value = "instrument"),

        circle = FALSE,
        icon = icon("database"), width = "100%",
        label = "Column names"
      ),

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
        sliderTextInput(label = "Item ID to plot", inputId = "item_id_to_plot", choices = c(1))
      ),

      checkboxGroupButtons(choices = c("Run descriptive"), inputId = "rundescriptive", status = "primary", justified = T, checkIcon = solver_icon),
      checkboxGroupButtons(choices = c("Run calibration"), inputId = "runcalibration", status = "primary", justified = T, checkIcon = solver_icon),
      checkboxGroupButtons(choices = c("Run linking"),     inputId = "runlinking",     status = "primary", justified = T, checkIcon = solver_icon),
      checkboxGroupButtons(choices = c("Run equating"),    inputId = "runequating",    status = "primary", justified = T, checkIcon = solver_icon),
      downloadButton("export_data", "Export visible tabs")
    ),

    mainPanel(
      verbatimTextOutput("textoutput", placeholder = TRUE),
      progressBar(id = "pb", value = 0, total = 1, display_pct = TRUE),
      hr(),
      tabsetPanel(id = "tabs",
        tabPanel("Anchor data",            value = 11, DTOutput("anchor_data"),                  style = css_y),
        tabPanel("Response data",          value = 12, DTOutput("response_data"),                style = css_y),
        tabPanel("Item map data",          value = 13, DTOutput("itemmap_data"),                 style = css_y),
        tabPanel("Frequency table",        value = 21, DTOutput("freqtable"),                    style = css_y),
        tabPanel("Descriptives",           value = 22, DTOutput("desctable"),                    style = css_y),
        tabPanel("Classical",              value = 23, verbatimTextOutput("classical"),          style = css_y),
        tabPanel("Classical (omega)",      value = 24, verbatimTextOutput("classical2"),         style = css_y),
        tabPanel("Calibration result",     value = 31, DTOutput("calib_params"),                 style = css_y),
        tabPanel("Item fit plot",          value = 32, plotOutput("plot_itemfit", width = "100%", height = "65vh"),  style = css_y),
        tabPanel("Item info",              value = 33, plotOutput("plot_iteminfo", width = "100%", height = "65vh"), style = css_y),
        tabPanel("Item fit table",         value = 34, DTOutput("table_itemfit"),                style = css_y),
        tabPanel("Crosswalk table (from calibration)", value = 35,
                 verbatimTextOutput("crosswalk_calibration"), style = css_y),
        tabPanel("Linking constants",      value = 41, verbatimTextOutput("linking_constants"),  style = css_y),
        tabPanel("Transformed parameters", value = 42, DTOutput("table_transformed_params"),     style = css_y),
        tabPanel("Crosswalk table (from linking)", value = 43,
                 verbatimTextOutput("crosswalk_linking"), style = css_y),
        tabPanel("Equating",               value = 51, verbatimTextOutput("equating_constants"), style = css_y),
        tabPanel("Concordance table",      value = 52, DTOutput("table_concordance"),            style = css_y)
      )
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
    tmp <- "Files OK. Press the button to run analysis."
  } else {
    tmp <- "Error: the files are not in the correct format, or the specified ids are not in the files."
  }
  return(tmp)
}

parseObject <- function(arg_object, digits = NULL){
  if (is.null(arg_object)) return(NULL)
  if (!is.null(digits)) return(round(arg_object, digits))
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

createConfigFromShiny <- function(input, guess = FALSE) {
  if (!guess) {
    cfg <- createConfig(
      anchor_file    = input$anchor_file$datapath,
      response_file  = input$response_file$datapath,
      itemmap_file   = input$itemmap_file$datapath,
      linking_method = input$linking_type,
      item_id        = input$item_id,
      person_id      = input$person_id,
      scale_id       = input$scale_id)
  } else {
    cfg <- createConfig(
      anchor_file    = input$anchor_file$datapath,
      response_file  = input$response_file$datapath,
      itemmap_file   = input$itemmap_file$datapath,
      linking_method = input$linking_type)
  }
  return(cfg)
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
    cfg <- createConfigFromShiny(input)
    v$inputdata <- tryCatch({loadData(cfg)}, warning = function (msg) { warning(msg); return(FALSE) })
    v$data_exists <- class(v$inputdata) == "PROsetta_data"
    v <- updateLogs(v, getDataStatusMsg(v$data_exists))

    if (!v$data_exists) {
      cfg <- createConfigFromShiny(input, guess = TRUE)
      v$inputdata <- tryCatch({loadData(cfg)}, warning = function (msg) { warning(msg); return(FALSE) })
      v$data_exists <- class(v$inputdata) == "PROsetta_data"
      v <- updateLogs(v, "Attempting to guess the IDs..")

      if (v$data_exists) {
        updateTextInput(session, "item_id", value = cfg@item_id)
        updateTextInput(session, "person_id", value = cfg@person_id)
        updateTextInput(session, "scale_id", value = cfg@scale_id)
        v <- updateLogs(v, getDataStatusMsg(v$data_exists))
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

    cfg <- createConfigFromShiny(input)
    assignObject("shiny_config", cfg, "PROsetta_config object")
    v$inputdata <- loadData(cfg)
    assignObject("shiny_data", v$inputdata, "PROsetta_data object")
    v$freqtable <- runFrequency(cfg, data = v$inputdata)
    assignObject("shiny_freq", v$freqtable, "Frequency table tab")
    v$desctable <- runDescriptive(cfg, v$inputdata)
    assignObject("shiny_desc", v$desctable, "Descriptives tab")
    v$classical <- runClassical(cfg, data = v$inputdata)
    assignObject("shiny_alpha", v$classical, "Classical tab")
    v$classical2 <- try(runClassical(cfg, data = v$inputdata, omega = TRUE, fm = "ml")[["Omega"]])
    assignObject("shiny_omega", v$classical2, "Classical (omega) tab")

    v$time <- Sys.time() - v$time
    v <- updateLogs(v, sprintf("Done in %7.3fs : run descriptive", v$time))

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

    cfg <- createConfigFromShiny(input)
    assignObject("shiny_config", cfg, "PROsetta_config object")

    v$inputdata <- loadData(cfg)
    assignObject("shiny_data", v$inputdata, "PROsetta_data object")

    updateSliderTextInput(
      session = session,
      inputId = "item_id_to_plot",
      choices = seq(1, v$n.items),
      selected = min(v$item_id_to_plot, v$n.items)
    )

    v$calib <- runCalibration(cfg)
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

    v$crosswalk_calibration <- runRSSS(cfg, calibration = v$calib)
    assignObject("shiny_crosswalk_calibration", v$crosswalk_calibration, "Crosswalk (calibration) tab")

    v$time <- Sys.time() - v$time
    v <- updateLogs(v, sprintf("Done in %7.3fs : run calibration", v$time))

    v$active_tabset <- updateTabSet(v$active_tabset, add_tabset = 3, session = session)
    toggleSolverButtons(TRUE, session)

  })



  observeEvent(input$runlinking, {

    toggleSolverButtons(FALSE, session)

    if (!(input$linking_type %in% c("MM", "MS", "HB", "SL", "LS"))) {
      v <- updateLogs(v, "Linking method must be one of the following: 'MM', 'MS', 'HB', 'SL', 'LS'.")
      break
    }

    progress <- Progress$new(session)
    on.exit(progress$close())
    progress$set(message = 'Computing..',
                  detail = 'This may take a while.')

    v$text <- "Running.."
    v$time <- Sys.time()

    cfg <- createConfigFromShiny(input)
    assignObject("shiny_config", cfg, "PROsetta_config object")

    v$inputdata <- loadData(cfg)
    assignObject("shiny_data", v$inputdata, "PROsetta_data object")

    v$linking <- runLinking(cfg, technical = list(NCYCLES = 1000))
    assignObject("shiny_link", v$linking, "Linking result (full object)")
    v$linking_constants <- v$linking$link@constants$SL
    assignObject("shiny_link_constants", v$linking_constants, "Linking constants tab")
    v$transformed_params <- v$linking$pars@pars$From
    assignObject("shiny_transformed_params", v$transformed_params, "Transformed parameters tab")

    v$crosswalk_linking <- runRSSS(cfg, calibration = v$linking)
    assignObject("shiny_crosswalk_linking", v$crosswalk_linking, "Crosswalk (linking) tab")

    v$time <- Sys.time() - v$time
    v <- updateLogs(v, sprintf("Done in %7.3fs : run linking", v$time))

    v$active_tabset <- updateTabSet(v$active_tabset, add_tabset = 4, session = session)
    toggleSolverButtons(TRUE, session)
  })




  observeEvent(input$runequating, {

    toggleSolverButtons(FALSE, session)

    v$text <- "Running.."
    v$time <- Sys.time()

    cfg <- createConfigFromShiny(input)
    assignObject("shiny_config", cfg, "PROsetta_config object")

    v$inputdata <- loadData(cfg)
    assignObject("shiny_data", v$inputdata, "PROsetta_data object")

    v$outequateequipercentile <- runEquateObserved(cfg, scale_to = 1, scale_from = 2, type = "equipercentile", smooth = "loglinear")
    assignObject("shiny_eq", v$outequateequipercentile, "Equating tab")
    v$concordance <- v$outequateequipercentile$concordance
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

  output$textoutput               <- renderText(parseObject(v$logstext))

  output$anchor_data              <- renderDT(parseObject(v$anchor_data),           options = list(pageLength = 100))
  output$response_data            <- renderDT(parseObject(v$response_data),         options = list(pageLength = 100))
  output$itemmap_data             <- renderDT(parseObject(v$itemmap_data),          options = list(pageLength = 100))

  output$freqtable                <- renderDT(parseObject(v$freqtable),             options = list(pageLength = 100))
  output$desctable                <- renderDT(parseObject(v$desctable, 3),          options = list(pageLength = 100))
  output$classical                <- renderPrint(parseObject(v$classical))
  output$classical2               <- renderPrint(parseObject(v$classical2))

  output$calib_params             <- renderDT(parseObject(v$calib_params, 3),       options = list(pageLength = 100))
  output$plot_itemfit             <- renderPlot(parseObject(v$plot_itemfit))
  output$plot_iteminfo            <- renderPlot(parseObject(v$plot_iteminfo))
  output$table_itemfit            <- renderDT(parseObject(v$table_itemfit),         options = list(pageLength = 100))
  output$crosswalk_calibration    <- renderPrint(parseObject(v$crosswalk_calibration))

  output$linking_constants        <- renderPrint(parseObject(v$linking_constants))
  output$table_transformed_params <- renderDT(parseObject(v$transformed_params, 3), options = list(pageLength = 100))
  output$crosswalk_linking        <- renderPrint(parseObject(v$crosswalk_linking))

  output$equating_constants       <- renderPrint(parseObject(v$outequateequipercentile))
  output$table_concordance        <- renderDT(parseObject(v$concordance, 3),        options = list(pageLength = 100))

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
            path <- getPath("crosswalk_calibration.txt")
            fs <- c(fs, path)
            tmp <- paste0(capture.output(v$crosswalk_calibration), collapse = "\n")
            write(tmp, path)
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
            path <- getPath("crosswalk_linking.txt")
            fs <- c(fs, path)
            tmp <- paste0(capture.output(v$crosswalk_linking), collapse = "\n")
            write(tmp, path)
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
