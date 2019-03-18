library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(PROsetta)
library(DT)

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
        tags$style(type="text/css", ".well { min-width: 100%; max-width: 100%; }")
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

        textInput("item_id", label = "Item ID", value = "item_id"),
        textInput("person_id", label = "Person ID", value = "prosettaid"),
        textInput("scale_id", label = "Scale ID", value = "instrument"),

        circle = FALSE,
        icon = icon("database"), width = "100%",
        label = "Column names"
      ),

      dropdownButton(
        inputId = "tabvisibility_dropdown",

        checkboxGroupButtons(
          inputId = "tabvisibility",
          choiceNames = c("Raw data", "Basic stats", "Calibration", "Linking", "Equating"),
          choiceValues = 1:5,
          justified = TRUE,
          selected = c(1,2)
        ),

        circle = FALSE,
        icon = icon("thumbtack"), width = "100%",
        label = "Tab visibility"
      ),

      checkboxGroupButtons(
        inputId = "rundescriptive",
        choices = c("Run descriptive"),
        checkIcon = list(yes = icon("drafting-compass"), no = icon("drafting-compass")),
        status = "primary",
        justified = TRUE
      ),
      checkboxGroupButtons(
        inputId = "runcalibration",
        choices = c("Run calibration"),
        checkIcon = list(yes = icon("drafting-compass"), no = icon("drafting-compass")),
        status = "primary",
        justified = TRUE
      ),
      checkboxGroupButtons(
        inputId = "runlinking",
        choices = c("Run linking"),
        checkIcon = list(yes = icon("drafting-compass"), no = icon("drafting-compass")),
        status = "primary",
        justified = TRUE
      ),
      checkboxGroupButtons(
        inputId = "runequating",
        choices = c("Run equating"),
        checkIcon = list(yes = icon("drafting-compass"), no = icon("drafting-compass")),
        status = "primary",
        justified = TRUE
      ),
      downloadButton("exportData", "Export visible tabs")
    ),


    mainPanel(
      tags$head(
        tags$style(type='text/css', '#textoutput {background-color: rgba(64,64,64,1); color: cyan;}')
      ),
      verbatimTextOutput("textoutput", placeholder = T),
      progressBar(id = "pb", value = 0, total = 1, display_pct = TRUE),
      hr(),
      tabsetPanel(id = "tabs",

        tabPanel("Anchor data",
                 style = "overflow-y:scroll; max-height: 700px",
                 DTOutput("anchor_data"),
                 value = 11),
        tabPanel("Response data",
                 style = "overflow-y:scroll; max-height: 700px",
                 DTOutput("response_data"),
                 value = 12),
        tabPanel("Item map data",
                 style = "overflow-y:scroll; max-height: 700px",
                 DTOutput("itemmap_data"),
                 value = 13),

        tabPanel("Frequency table",
                 style = "overflow-y:scroll; max-height: 700px",
                 DTOutput("freqtable"),
                 value = 21),
        tabPanel("Descriptives",
                 style = "overflow-y:scroll; max-height: 700px",
                 DTOutput("desctable"),
                 value = 22),
        tabPanel("Classical",
                 style = "overflow-y:scroll; max-height: 700px",
                 verbatimTextOutput("classical"),
                 value = 23),
        tabPanel("Classical (omega)",
                 style = "overflow-y:scroll; max-height: 700px",
                 verbatimTextOutput("classical2"),
                 value = 24),

        tabPanel("Calibration result",
                 style = "overflow-y:scroll; max-height: 700px",
                 DTOutput("calib_params"),
                 value = 31),
        tabPanel("Item fit plot",
                 style = "overflow-y:scroll; max-height: 700px",
                 plotOutput("plot_itemfit", width = "100%", height = "700px"),
                 value = 32),
        tabPanel("Item info",
                 style = "overflow-y:scroll; max-height: 700px",
                 plotOutput("plot_iteminfo", width = "100%", height = "700px"),
                 value = 33),
        tabPanel("Item fit table",
                 style = "overflow-y:scroll; max-height: 700px",
                 DTOutput("table_itemfit"),
                 value = 34),

        tabPanel("Linking constants",
                 style = "overflow-y:scroll; max-height: 700px",
                 verbatimTextOutput("linking_constants"),
                 value = 41),
        tabPanel("Equating",
                 style = "overflow-y:scroll; max-height: 700px",
                 verbatimTextOutput("equating_constants"),
                 value = 51)
      )
    )
  )
)

switch_main_buttons = function(enable){
  if (enable){
    shinyjs::enable("rundescriptive")
    shinyjs::enable("runcalibration")
    shinyjs::enable("runlinking")
    shinyjs::enable("runequating")
  } else {
    shinyjs::disable("rundescriptive")
    shinyjs::disable("runcalibration")
    shinyjs::disable("runlinking")
    shinyjs::disable("runequating")
  }
}

switch_tabs = function(id){

  i1 = 11:13
  i2 = 21:24
  i3 = 31:34
  i4 = 41
  i5 = 51
  is = list(i1, i2, i3, i4, i5)

  for (i in do.call(c, is)){
    hideTab("tabs", target = as.character(i))
  }

  if (!is.null(id)){
    for (i in do.call(c, is[id])){
      showTab("tabs", target = as.character(i))
    }
  }

}

get_data_status = function(ok){
  if (ok){
    tmp = "Files OK. Press the button to run analysis."
  } else {
    tmp = "Error: files are not in the correct format."
  }
  return(tmp)
}


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  v = reactiveValues(data.exists = F, active_tabset = c(1,2))

  switch_main_buttons(F)
  switch_tabs(c(1,2))

  observeEvent(input$tabvisibility, {
    v$active_tabset = as.numeric(input$tabvisibility)
    switch_tabs(v$active_tabset)
  },
  ignoreNULL = F
  )

  observeEvent(input$anchor_file, {
    if (!is.null(input$anchor_file)){
      v$anchor_data = read.csv(input$anchor_file$datapath)
    }
    if (!is.null(input$anchor_file) &
        !is.null(input$response_file) &
        !is.null(input$itemmap_file)){

      new.Config = new.config(anchorFile = input$anchor_file$datapath,
                              responseFile = input$response_file$datapath,
                              itemmapFile = input$itemmap_file$datapath,
                              linkingMethod = input$linking_type,
                              itemID = input$item_id,
                              personID = input$person_id,
                              scaleID = input$scale_id)
      v$inputdata = try(LoadData(new.Config))
      v$data.exists = class(v$inputdata) == "PROsetta.Data"
      v$text = get_data_status(v$data.exists)

      if (v$data.exists){
        v$active_tabset = unique(c(v$active_tabset, 1))
        switch_tabs(v$active_tabset)

        updateCheckboxGroupButtons(
          session = session,
          inputId = "tabvisibility",
          selected = as.character(v$active_tabset)
        )
      }

      switch_main_buttons(v$data.exists)
    }
  })

  observeEvent(input$response_file, {
    if (!is.null(input$response_file)){
      v$response_data = read.csv(input$response_file$datapath)
    }
    if (!is.null(input$anchor_file) &
        !is.null(input$response_file) &
        !is.null(input$itemmap_file)){
      new.Config = new.config(anchorFile = input$anchor_file$datapath,
                              responseFile = input$response_file$datapath,
                              itemmapFile = input$itemmap_file$datapath,
                              linkingMethod = input$linking_type,
                              itemID = input$item_id,
                              personID = input$person_id,
                              scaleID = input$scale_id)
      v$inputdata = try(LoadData(new.Config))
      v$data.exists = class(v$inputdata) == "PROsetta.Data"
      v$text = get_data_status(v$data.exists)

      if (v$data.exists){
        v$active_tabset = unique(c(v$active_tabset, 1))
        switch_tabs(v$active_tabset)

        updateCheckboxGroupButtons(
          session = session,
          inputId = "tabvisibility",
          selected = as.character(v$active_tabset)
        )
      }

      switch_main_buttons(v$data.exists)
    }
  })

  observeEvent(input$itemmap_file, {
    if (!is.null(input$itemmap_file)){
      v$itemmap_data = read.csv(input$itemmap_file$datapath)
    }
    if (!is.null(input$anchor_file) &
        !is.null(input$response_file) &
        !is.null(input$itemmap_file)){
      new.Config = new.config(anchorFile = input$anchor_file$datapath,
                              responseFile = input$response_file$datapath,
                              itemmapFile = input$itemmap_file$datapath,
                              linkingMethod = input$linking_type,
                              itemID = input$item_id,
                              personID = input$person_id,
                              scaleID = input$scale_id)
      v$inputdata = try(LoadData(new.Config))
      v$data.exists = class(v$inputdata) == "PROsetta.Data"
      v$text = get_data_status(v$data.exists)

      if (v$data.exists){
        v$active_tabset = unique(c(v$active_tabset, 1))
        switch_tabs(v$active_tabset)

        updateCheckboxGroupButtons(
          session = session,
          inputId = "tabvisibility",
          selected = as.character(v$active_tabset)
        )
      }

      switch_main_buttons(v$data.exists)
    }
  })

  observeEvent(input$rundescriptive, {

    switch_main_buttons(F)

    v$text = "Running.."
    v$time = Sys.time()

    new.Config = new.config(anchorFile = input$anchor_file$datapath,
                            responseFile = input$response_file$datapath,
                            itemmapFile = input$itemmap_file$datapath,
                            linkingMethod = input$linking_type,
                            itemID = input$item_id,
                            personID = input$person_id,
                            scaleID = input$scale_id)
    v$inputdata = LoadData(new.Config)
    v$freqtable = RunFrequency(new.Config, v$inputdata)
    v$desctable = RunDescriptive(new.Config, v$inputdata)
    v$classical = RunClassical(new.Config, v$inputdata)
    v$classical2 = RunClassical(new.Config, v$inputdata, omega = T)

    v$time = Sys.time() - v$time
    v$text = paste0("Done in ", sprintf("%3.3f", v$time), "s")

    v$active_tabset = unique(c(v$active_tabset, 2))
    switch_tabs(v$active_tabset)

    updateCheckboxGroupButtons(
      session = session,
      inputId = "tabvisibility",
      selected = as.character(v$active_tabset)
    )

    updateCheckboxGroupButtons(
      session = session,
      inputId = "rundescriptive",
      selected = character(0)
    )

    switch_main_buttons(T)

  })


  observeEvent(input$runcalibration, {

    switch_main_buttons(F)

    progress <- Progress$new(session)
    on.exit(progress$close())
    progress$set(message = 'Computing..',
                 detail = 'This may take a while.')

    v$text = "Running.."
    v$time = Sys.time()

    new.Config = new.config(anchorFile = input$anchor_file$datapath,
                            responseFile = input$response_file$datapath,
                            itemmapFile = input$itemmap_file$datapath,
                            linkingMethod = input$linking_type,
                            itemID = input$item_id,
                            personID = input$person_id,
                            scaleID = input$scale_id)
    v$inputdata = LoadData(new.Config)
    v$outCalib = RunCalibration(new.Config, v$inputdata)
    v$calib_params = mirt::coef(v$outCalib, IRTpars = TRUE, simplify = TRUE)$items
    v$table_itemfit = mirt::itemfit(v$outCalib, "S_X2", na.rm = TRUE)

    v$time = Sys.time() - v$time
    v$text = paste0("Done in ", sprintf("%3.3f", v$time), "s")

    v$active_tabset = unique(c(v$active_tabset, 3))
    switch_tabs(v$active_tabset)

    updateCheckboxGroupButtons(
      session = session,
      inputId = "tabvisibility",
      selected = as.character(v$active_tabset)
    )

    updateCheckboxGroupButtons(
      session = session,
      inputId = "runcalibration",
      selected = character(0)
    )

    switch_main_buttons(T)

  })



  observeEvent(input$runlinking, {

    switch_main_buttons(F)

    progress <- Progress$new(session)
    on.exit(progress$close())
    progress$set(message = 'Computing..',
                 detail = 'This may take a while.')

    v$text = "Running.."
    v$time = Sys.time()

    new.Config = new.config(anchorFile = input$anchor_file$datapath,
                            responseFile = input$response_file$datapath,
                            itemmapFile = input$itemmap_file$datapath,
                            linkingMethod = input$linking_type,
                            itemID = input$item_id,
                            personID = input$person_id,
                            scaleID = input$scale_id)
    v$inputdata = LoadData(new.Config)
    v$outequate = RunLinking(new.Config, v$inputdata, technical = list(NCYCLES = 1000))

    v$time = Sys.time() - v$time
    v$text = paste0("Done in ", sprintf("%3.3f", v$time), "s")

    v$active_tabset = unique(c(v$active_tabset, 4))
    switch_tabs(v$active_tabset)

    updateCheckboxGroupButtons(
      session = session,
      inputId = "tabvisibility",
      selected = as.character(v$active_tabset)
    )

    updateCheckboxGroupButtons(
      session = session,
      inputId = "runlinking",
      selected = character(0)
    )

    switch_main_buttons(T)

  })




  observeEvent(input$runequating, {

    switch_main_buttons(F)

    v$text = "Running.."
    v$time = Sys.time()

    new.Config = new.config(anchorFile = input$anchor_file$datapath,
                            responseFile = input$response_file$datapath,
                            itemmapFile = input$itemmap_file$datapath,
                            linkingMethod = input$linking_type,
                            itemID = input$item_id,
                            personID = input$person_id,
                            scaleID = input$scale_id)
    v$inputdata = LoadData(new.Config)
    v$outequateequipercentile = RunEquateObserved(new.Config, v$inputdata, scaleTo = 1, scaleFrom = 2, type = "equipercentile", smooth = "loglinear")

    v$time = Sys.time() - v$time
    v$text = paste0("Done in ", sprintf("%3.3f", v$time), "s")

    v$active_tabset = unique(c(v$active_tabset, 5))
    switch_tabs(v$active_tabset)

    updateCheckboxGroupButtons(
      session = session,
      inputId = "tabvisibility",
      selected = as.character(v$active_tabset)
    )

    updateCheckboxGroupButtons(
      session = session,
      inputId = "runequating",
      selected = character(0)
    )

    switch_main_buttons(T)

  })


  output$textoutput <- renderText({
    if (is.null(v$text)) return()
    v$text
  })

  output$anchor_data <- renderDT({
    if (is.null(v$anchor_data)) return()
    v$anchor_data},
    options = list(pageLength = 100)
  )
  output$response_data <- renderDT({
    if (is.null(v$response_data)) return()
    v$response_data},
    options = list(pageLength = 100)
  )
  output$itemmap_data <- renderDT({
    if (is.null(v$itemmap_data)) return()
    v$itemmap_data},
    options = list(pageLength = 100)
  )


  output$freqtable <- renderDT({
    if (is.null(v$freqtable)) return()
    v$freqtable},
    options = list(pageLength = 100)
  )
  output$desctable <- renderDT({
    if (is.null(v$desctable)) return()
    v$desctable},
    options = list(pageLength = 100)
  )
  output$classical <- renderPrint({
    if (is.null(v$classical)) return()
    v$classical
  })
  output$classical2 <- renderPrint({
    if (is.null(v$classical2)) return()
    v$classical2
  })

  output$calib_params <- renderDT({
    if (is.null(v$calib_params)) return()
    v$calib_params
    },
    options = list(pageLength = 100)
  )

  output$plot_itemfit <- renderPlot({
    if (is.null(v$outCalib)) return()
    mirt::itemfit(v$outCalib, empirical.plot = 1)
  })

  output$plot_iteminfo <- renderPlot({
    if (is.null(v$outCalib)) return()
    mirt::itemplot(v$outCalib, item = 1, type = "info")
  })
  output$table_itemfit <- renderDT({
    if (is.null(v$table_itemfit)) return()
    v$table_itemfit
    },
    options = list(pageLength = 100)
  )

  output$linking_constants <- renderPrint({
    if (is.null(v$outequate)) return()
    v$outequate$link@constants$SL
  })

  output$equating_constants <- renderPrint({
    if (is.null(v$outequateequipercentile)) return()
    v$outequateequipercentile
  })

  output$exportData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".zip", sep="")
    },
    content = function(fname) {
      fs <- c()
      tmpdir <- tempdir()
      setwd(tempdir())
      for (i in v$active_tabset) {
        if (i == 1){
          if (!is.null(v$anchor_data)){
            path = "raw_data_anchor.csv"
            fs <- c(fs, path)
            write.csv(v$anchor_data, path)
          }
          if (!is.null(v$response_data)){
            path = "raw_data_response.csv"
            fs <- c(fs, path)
            write.csv(v$response_data, path)
          }
          if (!is.null(v$itemmap_data)){
            path = "raw_data_itemmap.csv"
            fs <- c(fs, path)
            write.csv(v$itemmap_data, path)
          }
        }
        if (i == 2){
          if (!is.null(v$freqtable)){
            path = "basic_frequency.csv"
            fs <- c(fs, path)
            write.csv(v$freqtable, path)
          }
          if (!is.null(v$desctable)){
            path = "basic_descriptive.csv"
            fs <- c(fs, path)
            write.csv(v$desctable, path)
          }
          if (!is.null(v$classical)){
            path = "basic_reliability_alpha.txt"
            fs <- c(fs, path)
            tmp = paste0(capture.output(v$classical), collapse = "\n")
            write(tmp, path)
          }
          if (!is.null(v$classical2)){
            path = "basic_reliability_omega.txt"
            fs <- c(fs, path)
            tmp = paste0(capture.output(v$classical2), collapse = "\n")
            write(tmp, path)
          }
        }
        if (i == 3){

          if (!is.null(v$calib_params)){
            path = "calib_params.csv"
            fs <- c(fs, path)
            write.csv(v$calib_params, path)
          }

          n.items = dim(outCalib@Data$data)[2]

          if (!is.null(v$outCalib)){
            path = "calib_itemfit.pdf"
            fs <- c(fs, path)
            pdf(path)
            for (id in 1:n.items){
              p = mirt::itemfit(v$outCalib, empirical.plot = id)
              print(p)
            }
            dev.off()

            path = "calib_iteminfo.pdf"
            fs <- c(fs, path)
            pdf(path)
            for (id in 1:n.items){
              p = mirt::itemplot(v$outCalib, item = id, type = "info")
              print(p)
            }
            dev.off()
          }

          if (!is.null(v$table_itemfit)){
            path = "calib_fit.csv"
            fs <- c(fs, path)
            write.csv(v$table_itemfit, path)
          }
        }

        if (i == 4){
          if (!is.null(v$outequate)){
            path = "linking_constants.csv"
            fs <- c(fs, path)
            write.csv(v$outequate$link@constants$SL, path)
          }
        }

        if (i == 5){
          if (!is.null(v$outequateequipercentile)){
            path = "equating_constants.txt"
            fs <- c(fs, path)
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

# Run the application
shinyApp(ui = ui, server = server)

