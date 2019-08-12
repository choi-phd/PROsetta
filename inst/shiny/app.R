library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(PROsetta)
library(DT)

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
        tags$style(type="text/css", "select { min-width: 100%; max-width: 100%; }"),
        tags$style(type="text/css", ".span4 { min-width: 100%; max-width: 100%; }"),
        tags$style(type="text/css", ".well { min-width: 100%; max-width: 100%; }"),
        tags$style(type="text/css", "#textoutput { background-color: rgba(64,64,64,1); color: cyan; overflow-y:auto; height: 64px; display: flex; flex-direction: column-reverse; }"),
        tags$style(type="text/css", ".shiny-notification { font-size: 20px; background-color: #404040; color: #fff; }"),
        tags$style(type="text/css", "#shiny-notification-panel { width: 500px; }")
      ),
      helpText("This is a demo of PROsetta Linking Analysis. (UI work in progress)"),

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
        label = "Tab visibility", inputId = "tabvisibility.dropdown",
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
        tabPanel("Equating",               value = 51, verbatimTextOutput("equating_constants"), style = css_y)
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

updateTabSet <- function(tabset, id, session) {

  tabset <- unique(c(tabset, id))

  i1 <- 11:13
  i2 <- 21:24
  i3 <- 31:35
  i4 <- 41:43
  i5 <- 51
  is <- list(i1, i2, i3, i4, i5)

  for (i in do.call(c, is)) {
    hideTab("tabs", target = as.character(i))
  }

  if (!is.null(tabset)) {
    for (i in do.call(c, is[tabset])) {
      showTab("tabs", target = as.character(i))
    }
  }

  updateCheckboxGroupButtons(
    session = session,
    inputId = "tabvisibility",
    selected = as.character(tabset)
  )

  return(tabset)
}

getDataStatus <- function(ok) {
  if (ok) {
    tmp <- "Files OK. Press the button to run analysis."
  } else {
    tmp <- "Error: files are not in the correct format."
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

createConfigFromShiny <- function(input){
  cfg <- createConfig(
    anchor_file    = input$anchor_file$datapath,
    response_file  = input$response_file$datapath,
    itemmap_file   = input$itemmap_file$datapath,
    linking_method = input$linking_type,
    item_id        = input$item_id,
    person_id      = input$person_id,
    scale_id       = input$scale_id)
  return(cfg)
}

getPath <- function(tmpdir, fn) {
  tmp <- file.path(tmpdir, fn)
  tmp <- normalizePath(tmp, mustWork = FALSE)
  return(tmp)
}

updateLogs <- function(v, newlog) {
  v$logs <- c(v$logs, newlog)
  v$logstext <- paste0(v$logs, collapse = "\n")
  return(v)
}

server <- function(input, output, session) {
  v <- reactiveValues(
    data_exists = FALSE,
    active_tabset = c(1,2))

  toggleSolverButtons(FALSE, session)

  observeEvent(input$tabvisibility, {
    v$active_tabset = as.numeric(input$tabvisibility)
    updateTabSet(v$active_tabset, v$active_tabset, session)
  }, ignoreNULL = FALSE)

  observeEvent(input$anchor_file, {
    if (!is.null(input$anchor_file)){
      v$anchor_data <- read.csv(input$anchor_file$datapath)
      v <- updateLogs(v, sprintf("Anchor data imported: %i items", dim(v$anchor_data)[1]))
    }
    if (!is.null(input$anchor_file) & !is.null(input$response_file) & !is.null(input$itemmap_file)) {
      cfg <- createConfigFromShiny(input)
      v$inputdata <- try(loadData(cfg))
      v$data_exists <- class(v$inputdata) == "PROsetta_data"
      v <- updateLogs(v, getDataStatus(v$data_exists))

      if (v$data_exists) {
        v$active_tabset <- updateTabSet(v$active_tabset, 1, session)
      }

      toggleSolverButtons(v$data_exists, session)
    }
  })

  observeEvent(input$response_file, {
    if (!is.null(input$response_file)){
      v$response_data <- read.csv(input$response_file$datapath)
      v <- updateLogs(v, sprintf("Response data imported: %i cases * %i items", dim(v$response_data)[1], dim(v$response_data)[2] - 1))
    }
    if (!is.null(input$anchor_file) & !is.null(input$response_file) & !is.null(input$itemmap_file)){
      cfg <- createConfigFromShiny(input)
      v$inputdata <- try(loadData(cfg))
      v$data_exists <- class(v$inputdata) == "PROsetta_data"
      v <- updateLogs(v, getDataStatus(v$data_exists))

      if (v$data_exists) {
        v$active_tabset <- updateTabSet(v$active_tabset, 1, session)
      }
      toggleSolverButtons(v$data_exists, session)
    }
  })

  observeEvent(input$itemmap_file, {
    if (!is.null(input$itemmap_file)){
      v$itemmap_data <- read.csv(input$itemmap_file$datapath)
      v$n.items <- dim(v$itemmap_data)[1]
      v <- updateLogs(v, sprintf("Item map imported: %i items", v$n.items))
    }
    if (!is.null(input$anchor_file) & !is.null(input$response_file) & !is.null(input$itemmap_file)){
      cfg <- createConfigFromShiny(input)
      v$inputdata <- try(loadData(cfg))
      v$data_exists <- class(v$inputdata) == "PROsetta_data"
      v <- updateLogs(v, getDataStatus(v$data_exists))

      if (v$data_exists) {
        v$active_tabset = updateTabSet(v$active_tabset, 1, session)
      }
      toggleSolverButtons(v$data_exists, session)
    }
  })

  observeEvent(input$rundescriptive, {

    toggleSolverButtons(FALSE, session)

    v$text <- "Running.."
    v$time <- Sys.time()

    cfg <- createConfigFromShiny(input)
    assignObject("shiny_config", cfg, "PROsetta_config object")
    v$inputdata <- loadData(cfg)
    assignObject("shiny_data", v$inputdata, "PROsetta_data object")
    v$freqtable <- runFrequency(cfg, v$inputdata)
    assignObject("shiny_freq", v$freqtable, "Frequency table tab")
    v$desctable <- runDescriptive(cfg, v$inputdata)
    assignObject("shiny_desc", v$desctable, "Descriptives tab")
    v$classical <- runClassical(cfg, v$inputdata)
    assignObject("shiny_alpha", v$classical, "Classical tab")
    v$classical2 <- try(runClassical(cfg, v$inputdata, omega = T, fm = "ml")[["Omega"]])
    assignObject("shiny_omega", v$classical2, "Classical (omega) tab")

    v$time <- Sys.time() - v$time
    v <- updateLogs(v, sprintf("Done in %7.3fs : run descriptive", v$time))

    updateTabSet(v$active_tabset, 2, session)
    toggleSolverButtons(TRUE, session)

  })


  observeEvent(input$runcalibration, {

    toggleSolverButtons(FALSE, session)

    progress = Progress$new(session)
    on.exit(progress$close())
    progress$set(message = 'Computing..',
                 detail = 'This may take a while.')

    v$time <- Sys.time()

    cfg <- createConfigFromShiny(input)
    assignObject("shiny_config", cfg, "PROsetta_config object")

    v$inputdata = loadData(cfg)
    assignObject("shiny_data", v$inputdata, "PROsetta_data object")

    updateSliderTextInput(
      session = session,
      inputId = "item_id_to_plot",
      choices = seq(1, v$n.items),
      selected = min(v$item_id_to_plot, v$n.items)
    )

    v$calib <- runCalibration(cfg, v$inputdata)
    assignObject("shiny_calib", v$calib, "Calibration result (full object)")
    v$calib_params = mirt::coef(v$calib, IRTpars = TRUE, simplify = TRUE)$items
    assignObject("shiny_params", v$calib_params, "Calibration result tab")
    v$plot_itemfit  = mirt::itemfit(v$calib, empirical.plot = v$item_id_to_plot)
    assignObject("shiny_itemfit", v$plot_itemfit, "Item fit plot tab")
    v$plot_iteminfo = mirt::itemplot(v$calib, item = v$item_id_to_plot, type = "info")
    assignObject("shiny_iteminfo", v$plot_iteminfo, "Item info tab")

    tmp <- try(mirt::itemfit(v$calib, "S_X2", na.rm = TRUE), silent = T)
    if (class(tmp)[1] == "try-error"){
      tmp <- try(mirt::itemfit(v$calib, "S_X2"))
    }

    v$table_itemfit = tmp
    assignObject("shiny_itemfittable", v$table_itemfit, "Item fit table tab")

    v$crosswalk_calibration <- runRSSS(cfg, v$inputdata, v$calib)
    assignObject("shiny_crosswalk_calibration", v$crosswalk_calibration, "Crosswalk (calibration) tab")

    v$time <- Sys.time() - v$time
    v <- updateLogs(v, sprintf("Done in %7.3fs : run calibration", v$time))

    v$active_tabset <- updateTabSet(v$active_tabset, 3, session)
    toggleSolverButtons(TRUE, session)

  })



  observeEvent(input$runlinking, {

    toggleSolverButtons(FALSE, session)

    if (!(input$linking_type %in% c("MM", "MS", "HB", "SL", "LS"))) {
      v$text = "Linking method must be one of the following: 'MM', 'MS', 'HB', 'SL', 'LS'."
      break
    }

    progress = Progress$new(session)
    on.exit(progress$close())
    progress$set(message = 'Computing..',
                  detail = 'This may take a while.')

    v$text <- "Running.."
    v$time <- Sys.time()

    cfg <- createConfigFromShiny(input)
    assignObject("shiny_config", cfg, "PROsetta_config object")

    v$inputdata <- loadData(cfg)
    assignObject("shiny_data", v$inputdata, "PROsetta_data object")

    v$linking <- runLinking(cfg, v$inputdata, technical = list(NCYCLES = 1000))
    assignObject("shiny_link", v$linking, "Linking result (full object)")
    v$linking_constants <- v$linking$link@constants$SL
    assignObject("shiny_link_constants", v$linking_constants, "Linking constants tab")
    v$transformed_params <- v$linking$pars@pars$From
    assignObject("shiny_transformed_params", v$transformed_params, "Transformed parameters tab")

    v$crosswalk_linking <- runRSSS(cfg, v$inputdata, v$linking)
    assignObject("shiny_crosswalk_linking", v$crosswalk_linking, "Crosswalk (linking) tab")

    v$time <- Sys.time() - v$time
    v <- updateLogs(v, sprintf("Done in %7.3fs : run linking", v$time))

    v$active_tabset <- updateTabSet(v$active_tabset, 4, session)
    toggleSolverButtons(TRUE, session)
  })




  observeEvent(input$runequating, {

    toggleSolverButtons(FALSE, session)

    v$text <- "Running.."
    v$time <- Sys.time()

    cfg <- createConfigFromShiny(input)
    assignObject("shiny_config", cfg, "PROsetta_config object")

    v$inputdata = loadData(cfg)
    assignObject("shiny_data", v$inputdata, "PROsetta_data object")

    v$outequateequipercentile = runEquateObserved(cfg, v$inputdata, scale_to = 1, scale_from = 2, type = "equipercentile", smooth = "loglinear")
    assignObject("shiny_eq", v$outequateequipercentile, "Equating tab")

    v$time <- Sys.time() - v$time
    v <- updateLogs(v, sprintf("Done in %7.3fs : run equating", v$time))

    v$active_tabset <- updateTabSet(v$active_tabset, 5, session)
    toggleSolverButtons(TRUE, session)

  })

  observeEvent(input$item_id_to_plot, {
    if (verifyText(input$item_id_to_plot)){
      eval(parse(text = sprintf("item_id_to_plot <- c(%s)[1]", input$item_id_to_plot)))
      item_id_to_plot <- min(item_id_to_plot, v$n.items)
      item_id_to_plot = max(1, item_id_to_plot)
      v$item_id_to_plot = item_id_to_plot
      if (is.null(v$calib)) return()
      v$plot_itemfit  = mirt::itemfit(v$calib, empirical.plot = v$item_id_to_plot)
      assignObject("shiny_itemfit", v$plot_itemfit, "Item fit plot tab")
      v$plot_iteminfo = mirt::itemplot(v$calib, item = v$item_id_to_plot, type = "info")
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

  output$export_data <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".zip", sep="")
    },
    content = function(fname) {

      fs     <- c()
      tmpdir <- tempdir()

      for (i in v$active_tabset) {
        if (i == 1){
          if (!is.null(v$anchor_data)){
            path = getPath(tmpdir, "raw.data.anchor.csv")
            fs = c(fs, path)
            write.csv(v$anchor_data, path, row.names = F)
          }
          if (!is.null(v$response_data)){
            path = getPath(tmpdir, "raw.data.response.csv")
            fs = c(fs, path)
            write.csv(v$response_data, path, row.names = F)
          }
          if (!is.null(v$itemmap_data)){
            path = getPath(tmpdir, "raw.data.itemmap.csv")
            fs = c(fs, path)
            write.csv(v$itemmap_data, path, row.names = F)
          }
        }
        if (i == 2) {
          if (!is.null(v$freqtable)) {
            path <- getPath(tmpdir, "basic_frequency.csv")
            fs <- c(fs, path)
            write.csv(v$freqtable, path)
          }
          if (!is.null(v$desctable)) {
            path <- getPath(tmpdir, "basic_descriptive.csv")
            fs <- c(fs, path)
            write.csv(v$desctable, path)
          }
          if (!is.null(v$classical)) {
            path <- getPath(tmpdir, "basic_reliability_alpha.txt")
            fs <- c(fs, path)
            tmp <- paste0(capture.output(v$classical), collapse = "\n")
            write(tmp, path)
          }
          if (!is.null(v$classical2)) {
            path <- getPath(tmpdir, "basic_reliability_omega.txt")
            fs <- c(fs, path)
            tmp <- paste0(capture.output(v$classical2), collapse = "\n")
            write(tmp, path)
          }
        }
        if (i == 3) {

          if (!is.null(v$calib_params)) {
            path = getPath(tmpdir, "calib_params.csv")
            fs = c(fs, path)
            write.csv(v$calib_params, path)
          }

          n.items = dim(v$calib@data$data)[2]

          if (!is.null(v$calib)){
            path = getPath(tmpdir, "calib.itemfit.pdf")
            fs = c(fs, path)
            pdf(path)
            for (id in 1:n.items){
              p = mirt::itemfit(v$calib, empirical.plot = id)
              print(p)
            }
            dev.off()

            path = getPath(tmpdir, "calib.iteminfo.pdf")
            fs = c(fs, path)
            pdf(path)
            for (id in 1:n.items){
              p = mirt::itemplot(v$calib, item = id, type = "info")
              print(p)
            }
            dev.off()
          }

          if (!is.null(v$table_itemfit)){
            path = getPath(tmpdir, "calib.fit.csv")
            fs = c(fs, path)
            write.csv(v$table_itemfit, path, row.names = F)
          }
          if (!is.null(v$crosswalk_calibration)){
            path = getPath(tmpdir, "crosswalk_calibration.txt")
            fs = c(fs, path)
            tmp = paste0(capture.output(v$crosswalk_calibration), collapse = "\n")
            write(tmp, path)
          }
        }

        if (i == 4){
          if (!is.null(v$linking_constants)){
            path = getPath(tmpdir, "linking_constants.csv")
            fs = c(fs, path)
            write.csv(v$linking_constants, path)
          }
          if (!is.null(v$transformed_params)){
            path = getPath(tmpdir, "transformed_params.csv")
            fs = c(fs, path)
            write.csv(v$transformed_params, path)
          }
          if (!is.null(v$crosswalk_linking)){
            path = getPath(tmpdir, "crosswalk_linking.txt")
            fs = c(fs, path)
            tmp = paste0(capture.output(v$crosswalk_linking), collapse = "\n")
            write(tmp, path)
          }
        }

        if (i == 5){
          if (!is.null(v$outequateequipercentile)){
            path = getPath(tmpdir, "equating_constants.txt")
            fs = c(fs, path)
            tmp = paste0(capture.output(v$outequateequipercentile), collapse = "\n")
            write(tmp, path)
          }
        }
      }
      zip(zipfile = fname, files = fs, flags = "-j")
    },
    contentType = "application/zip"
  )
}

shinyApp(ui = ui, server = server)
