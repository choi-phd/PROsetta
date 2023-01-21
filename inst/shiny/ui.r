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

      pickerInput(
        inputId = "linking_type",
        choices = c("MM", "MS", "HB", "SL", "FIXEDPAR", "CP", "CPLA", "NONE")
      ),

      h3(""),

      dropdownButton(
        inputId = "simulation_dropdown",

        textInput(label = "# of iterations", inputId = "n_iter", value = "1000"),

        circle = FALSE,
        icon = icon("gear"), width = "100%",
        label = "Options"
      ),

      checkboxGroupButtons(
        choices = c("Pre-analysis: descriptive"), inputId = "rundescriptive",
        status = "primary", justified = TRUE, checkIcon = pre_icon),
      checkboxGroupButtons(
        choices = c("Pre-analysis: calibration (for item fit)"), inputId = "runcalibration",
        status = "primary", justified = TRUE, checkIcon = pre_icon),
      checkboxGroupButtons(
        choices = c("Run parameter linking"), inputId = "runlinking",
        status = "warning", justified = TRUE, checkIcon = solver_icon),
      checkboxGroupButtons(
        choices = c("Run score equating")   , inputId = "runequating",
        status = "warning", justified = TRUE, checkIcon = solver_icon),

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
        sliderInput(
          label = "Item ID to plot", inputId = "item_id_to_plot",
          min = 1, max = 1, step = 1,
          value = 1
        ),
        textInput(label = "Scale ID to display crosswalk tables (also accepts 'combined')", inputId = "id_cross", value = "1")
      ),

      downloadButton("export_data", "Export visible tabs"),

      dropdownButton(
        label = "Close app", inputId = "closeapp_dropdown",
        circle = FALSE, width = "100%", icon = icon("times-circle"),
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
        tabPanel("Response data"                     , value = 11, DTOutput("response_data")                                   , style = css_y),
        tabPanel("Item map data"                     , value = 12, DTOutput("itemmap_data")                                    , style = css_y),
        tabPanel("Anchor data"                       , value = 13, DTOutput("anchor_data")                                     , style = css_y),
        tabPanel("Frequency table"                   , value = 21, DTOutput("freqtable")                                       , style = css_y),
        tabPanel("Descriptives"                      , value = 22, DTOutput("desctable")                                       , style = css_y),
        tabPanel("Classical"                         , value = 23, verbatimTextOutput("classical")                             , style = css_y),
        tabPanel("Classical (omega)"                 , value = 24, verbatimTextOutput("classical2")                            , style = css_y),
        tabPanel("Calibration result (not linked)"   , value = 31, DTOutput("calib_params")                                    , style = css_y),
        tabPanel("Item fit plot"                     , value = 32, plotOutput("plot_itemfit", width = "100%", height = "65vh") , style = css_y),
        tabPanel("Item info"                         , value = 33, plotOutput("plot_iteminfo", width = "100%", height = "65vh"), style = css_y),
        tabPanel("Item fit table"                    , value = 34, DTOutput("table_itemfit")                                   , style = css_y),
        tabPanel("Crosswalk table (from calibration)", value = 35, DTOutput("crosswalk_calibration")                           , style = css_y),
        tabPanel("Linking constants"                 , value = 41, verbatimTextOutput("linking_constants")                     , style = css_y),
        tabPanel("Linked parameters"                 , value = 42, DTOutput("table_linked_params")                             , style = css_y),
        tabPanel("Crosswalk table (from linking)"    , value = 43, DTOutput("crosswalk_linking")                               , style = css_y),
        tabPanel("Equating"                          , value = 51, verbatimTextOutput("equating_constants")                    , style = css_y),
        tabPanel("Concordance table"                 , value = 52, DTOutput("table_concordance")                               , style = css_y)
      ),

      width = 9
    )
  )
)
