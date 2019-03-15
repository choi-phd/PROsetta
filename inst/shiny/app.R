library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(PROsetta)
library(DT)
library(readxl)

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

      checkboxGroupButtons(
        inputId = "runsolver",
        choices = c("Run analysis"),
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
      )
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
    shinyjs::enable("runsolver")
    shinyjs::enable("runcalibration")
    shinyjs::enable("runlinking")
  } else {
    shinyjs::disable("runsolver")
    shinyjs::disable("runcalibration")
    shinyjs::disable("runlinking")
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

  for (i in do.call(c, is[id])){
    showTab("tabs", target = as.character(i))
  }

}

get_data_status = function(ok){
  if (ok){
    tmp = "Files OK. Press the button to run solver."
  } else {
    tmp = "Error: files are not in the correct format."
  }
  return(tmp)
}


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  v = reactiveValues(data.exists = F, active_tabset = 1)

  switch_main_buttons(F)
  switch_tabs(1)

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
      switch_main_buttons(v$data.exists)
    }
  })

  observeEvent(input$runsolver, {

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
    v$classical = capture.output(RunClassical(new.Config, v$inputdata))
    v$classical2 = capture.output(RunClassical(new.Config, v$inputdata, omega = T))

    v$time = Sys.time() - v$time
    v$text = paste0("Done in ", sprintf("%3.3f", v$time), "s")

    v$active_tabset = unique(c(v$active_tabset, 2))
    switch_tabs(v$active_tabset)

    updateCheckboxGroupButtons(
      session = session,
      inputId = "runsolver",
      selected = character(0)
    )

    switch_main_buttons(T)

  })


  observeEvent(input$runcalibration, {

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
    v$outCalib = RunCalibration(new.Config, v$inputdata)
    v$table_itemfit = mirt::itemfit(v$outCalib, "S_X2", na.rm = TRUE)

    v$time = Sys.time() - v$time
    v$text = paste0("Done in ", sprintf("%3.3f", v$time), "s")

    v$active_tabset = unique(c(v$active_tabset, 3))
    switch_tabs(v$active_tabset)

    updateCheckboxGroupButtons(
      session = session,
      inputId = "runcalibration",
      selected = character(0)
    )

    switch_main_buttons(T)

  })



  observeEvent(input$runlinking, {

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
    v$outequate = RunLinking(new.Config, v$inputdata, technical = list(NCYCLES = 1000))

    v$time = Sys.time() - v$time
    v$text = paste0("Done in ", sprintf("%3.3f", v$time), "s")

    v$active_tabset = unique(c(v$active_tabset, 4))
    switch_tabs(v$active_tabset)

    updateCheckboxGroupButtons(
      session = session,
      inputId = "runlinking",
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
  output$classical <- renderText({
    if (is.null(v$classical)) return()
    paste0(v$classical, collapse = "\n")
  })
  output$classical2 <- renderText({
    if (is.null(v$classical2)) return()
    paste0(v$classical2, collapse = "\n")
  })

  output$calib_params <- renderDT({
    if (is.null(v$outCalib)) return()
    mirt::coef(v$outCalib, IRTpars = TRUE, simplify = TRUE)$items
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

}

# Run the application
shinyApp(ui = ui, server = server)

