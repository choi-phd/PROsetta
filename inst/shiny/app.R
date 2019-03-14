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
      )
    ),


    mainPanel(
      tags$head(
        tags$style(type='text/css', '#textoutput {background-color: rgba(64,64,64,1); color: cyan;}')
      ),
      disabled(
      checkboxGroupButtons(
        inputId = "runsolver",
        choices = c("Run analysis"),
        checkIcon = list(yes = icon("drafting-compass"), no = icon("drafting-compass")),
        status = "primary",
        justified = TRUE
      )),
      verbatimTextOutput("textoutput", placeholder = T),
      progressBar(id = "pb", value = 0, total = 1, display_pct = TRUE),
      hr(),
      tabsetPanel(id = "tabs",
        tabPanel("Frequency Table",
                 style = "overflow-y:scroll; max-height: 700px",
                 DTOutput("freqtable")),
        tabPanel("Descriptives",
                 style = "overflow-y:scroll; max-height: 700px",
                 DTOutput("desctable")),
        tabPanel("Classical",
                 style = "overflow-y:scroll; max-height: 700px",
                 verbatimTextOutput("classical")),
        tabPanel("Classical (omega)",
                 style = "overflow-y:scroll; max-height: 700px",
                 verbatimTextOutput("classical2"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  v = reactiveValues(data.exists = F)

  observeEvent(input$anchor_file, {
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
      if (class(v$inputdata) == "PROsetta.Data"){
        v$data.exists = T
        v$text = "Files OK. Press the button to run solver."
        enable("runsolver")
      } else {
        v$data.exists = F
        v$text = "Error: files are not in the correct format."
        shinyjs::disable("runsolver")
      }
    }
  })

  observeEvent(input$response_file, {
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
      if (class(v$inputdata) == "PROsetta.Data"){
        v$data.exists = T
        v$text = "Files OK. Press the button to run solver."
        enable("runsolver")
      } else {
        v$data.exists = F
        v$text = "Error: files are not in the correct format."
        shinyjs::disable("runsolver")
      }
    }
  })

  observeEvent(input$itemmap_file, {
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
      if (class(v$inputdata) == "PROsetta.Data"){
        v$data.exists = T
        v$text = "Files OK. Press the button to run solver."
        enable("runsolver")
      } else {
        v$data.exists = F
        v$text = "Error: files are not in the correct format."
        shinyjs::disable("runsolver")
      }
    }
  })

  observeEvent(input$runsolver, {

    shinyjs::disable("runsolver")

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

    updateCheckboxGroupButtons(
      session = session,
      inputId = "runsolver",
      selected = character(0)
    )

    shinyjs::enable("runsolver")

  })

  output$textoutput <- renderText({
    if (is.null(v$text)) return()
    v$text
  })

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


}

# Run the application
shinyApp(ui = ui, server = server)

