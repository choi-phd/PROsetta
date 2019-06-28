library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(PROsetta)
library(DT)

css.y = "overflow-y:scroll; max-height: 65vh"
solver.icon = list(yes = icon("drafting-compass"), no = icon("drafting-compass"))

ui = fluidPage(
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

        fileInput("anchor.file", buttonLabel = "Anchor data", label = NULL),
        fileInput("response.file", buttonLabel = "Response data", label = NULL),
        fileInput("itemmap.file", buttonLabel = "Item map", label = NULL),

        circle = FALSE, status = "primary",
        icon = icon("file-import"), width = "100%",

        label = "Load files"
      ),

      h3(""),

      radioGroupButtons(
        inputId = "linking.type",
        choices = c("MM", "MS", "HB", "SL", "FIXEDPAR", "NONE"),
        justified = TRUE
      ),


      dropdownButton(
        inputId = "simulation.dropdown",

        textInput(label = "Item ID", inputId = "item.id", value = "item_id"),
        textInput(label = "Person ID", inputId = "person.id", value = "prosettaid"),
        textInput(label = "Scale ID", inputId = "scale.id", value = "instrument"),

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
        sliderTextInput(label = "Item ID to plot", inputId = "item.id.to.plot", choices = c(1))
      ),

      checkboxGroupButtons(choices = c("Run descriptive"), inputId = "rundescriptive", status = "primary", justified = T, checkIcon = solver.icon),
      checkboxGroupButtons(choices = c("Run calibration"), inputId = "runcalibration", status = "primary", justified = T, checkIcon = solver.icon),
      checkboxGroupButtons(choices = c("Run linking"),     inputId = "runlinking",     status = "primary", justified = T, checkIcon = solver.icon),
      checkboxGroupButtons(choices = c("Run equating"),    inputId = "runequating",    status = "primary", justified = T, checkIcon = solver.icon),
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

        tabPanel("Anchor data",            value = 11, DTOutput("anchor.data"),                  style = css.y),
        tabPanel("Response data",          value = 12, DTOutput("response.data"),                style = css.y),
        tabPanel("Item map data",          value = 13, DTOutput("itemmap.data"),                 style = css.y),
        tabPanel("Frequency table",        value = 21, DTOutput("freqtable"),                    style = css.y),
        tabPanel("Descriptives",           value = 22, DTOutput("desctable"),                    style = css.y),
        tabPanel("Classical",              value = 23, verbatimTextOutput("classical"),          style = css.y),
        tabPanel("Classical (omega)",      value = 24, verbatimTextOutput("classical2"),         style = css.y),
        tabPanel("Calibration result",     value = 31, DTOutput("calib.params"),                 style = css.y),
        tabPanel("Item fit plot",          value = 32, plotOutput("plot.itemfit", width = "100%", height = "65vh"),  style = css.y),
        tabPanel("Item info",              value = 33, plotOutput("plot.iteminfo", width = "100%", height = "65vh"), style = css.y),
        tabPanel("Item fit table",         value = 34, DTOutput("table.itemfit"),                style = css.y),
        tabPanel("Crosswalk table (from calibration)", value = 35,
                 verbatimTextOutput("crosswalk.calibration"), style = css.y),
        tabPanel("Linking constants",      value = 41, verbatimTextOutput("linking.constants"),  style = css.y),
        tabPanel("Transformed parameters", value = 42, DTOutput("table.transformed.params"),     style = css.y),
        tabPanel("Crosswalk table (from linking)", value = 43,
                 verbatimTextOutput("crosswalk.linking"), style = css.y),
        tabPanel("Equating",               value = 51, verbatimTextOutput("equating.constants"), style = css.y)
      )
    )
  )
)

is.text.parsable = function(arg.text){
  txt = gsub("[^0-9\\., \\-]", "", arg.text) # Limits eval to only accept legit inputs
  return(txt == arg.text)
}

switch.main.buttons = function(enable){
  shinyjs::toggleState("rundescriptive", enable)
  shinyjs::toggleState("runcalibration", enable)
  shinyjs::toggleState("runlinking", enable)
  shinyjs::toggleState("runequating", enable)
}

switch.tabs = function(id){

  i1 = 11:13
  i2 = 21:24
  i3 = 31:35
  i4 = 41:43
  i5 = 51
  is = list(i1, i2, i3, i4, i5)

  for (i in do.call(c, is)){ hideTab("tabs", target = as.character(i)) }

  if (!is.null(id)){
    for (i in do.call(c, is[id])){ showTab("tabs", target = as.character(i)) }
  }
}

get.data.status = function(ok){
  if (ok){
    tmp = "Files OK. Press the button to run analysis."
  } else {
    tmp = "Error: files are not in the correct format."
  }
  return(tmp)
}

return.object.or.null = function(arg.object, digits = NULL){
  if (is.null(arg.object)) return(NULL)
  if (!is.null(digits)) return(round(arg.object, digits))
  return(arg.object)
}

assign.object.first = TRUE

assign.object = function(objname, obj, desc){
  if (assign.object.first){
    assign.object.first <<- FALSE
    message("\nRefresh the environment tab to see the objects in the list.")
  }
  assign(objname, obj, envir = .GlobalEnv)
  pad = paste0(rep(" ", 48 - nchar(desc)), collapse = "")
  tmp = paste0(desc, pad, "assigned to : ", objname)
  message(tmp)
}

shiny.new.config = function(input){
  config = new.config(anchorFile = input$anchor.file$datapath,
                      responseFile = input$response.file$datapath,
                      itemmapFile = input$itemmap.file$datapath,
                      linkingMethod = input$linking.type,
                      itemID = input$item.id,
                      personID = input$person.id,
                      scaleID = input$scale.id)
  return(config)
}

server = function(input, output, session) {
  v = reactiveValues(data.exists = F, active.tabset = c(1,2))

  switch.main.buttons(F)
  switch.tabs(c(1,2))

  observeEvent(input$tabvisibility, {
    v$active.tabset = as.numeric(input$tabvisibility)
    switch.tabs(v$active.tabset)
  }, ignoreNULL = F)

  observeEvent(input$anchor.file, {
    if (!is.null(input$anchor.file)){
      v$anchor.data = read.csv(input$anchor.file$datapath)
    }
    if (!is.null(input$anchor.file) & !is.null(input$response.file) & !is.null(input$itemmap.file)){
      new.Config = shiny.new.config(input)
      v$inputdata = try(LoadData(new.Config))
      v$data.exists = class(v$inputdata) == "PROsetta.Data"
      v$text = get.data.status(v$data.exists)

      if (v$data.exists){
        v$active.tabset = unique(c(v$active.tabset, 1))
        switch.tabs(v$active.tabset)

        updateCheckboxGroupButtons(
          session = session,
          inputId = "tabvisibility",
          selected = as.character(v$active.tabset)
        )
      }

      switch.main.buttons(v$data.exists)
    }
  })

  observeEvent(input$response.file, {
    if (!is.null(input$response.file)){
      v$response.data = read.csv(input$response.file$datapath)
    }
    if (!is.null(input$anchor.file) & !is.null(input$response.file) & !is.null(input$itemmap.file)){
      new.Config = new.config(anchorFile = input$anchor.file$datapath,
                              responseFile = input$response.file$datapath,
                              itemmapFile = input$itemmap.file$datapath,
                              linkingMethod = input$linking.type,
                              itemID = input$item.id,
                              personID = input$person.id,
                              scaleID = input$scale.id)
      v$inputdata = try(LoadData(new.Config))
      v$data.exists = class(v$inputdata) == "PROsetta.Data"
      v$text = get.data.status(v$data.exists)

      if (v$data.exists){
        v$active.tabset = unique(c(v$active.tabset, 1))
        switch.tabs(v$active.tabset)

        updateCheckboxGroupButtons(
          session = session,
          inputId = "tabvisibility",
          selected = as.character(v$active.tabset)
        )
      }

      switch.main.buttons(v$data.exists)
    }
  })

  observeEvent(input$itemmap.file, {
    if (!is.null(input$itemmap.file)){
      v$itemmap.data = read.csv(input$itemmap.file$datapath)
      v$n.items = dim(v$itemmap.data)[1]
    }
    if (!is.null(input$anchor.file) & !is.null(input$response.file) & !is.null(input$itemmap.file)){
      new.Config = new.config(anchorFile = input$anchor.file$datapath,
                              responseFile = input$response.file$datapath,
                              itemmapFile = input$itemmap.file$datapath,
                              linkingMethod = input$linking.type,
                              itemID = input$item.id,
                              personID = input$person.id,
                              scaleID = input$scale.id)
      v$inputdata = try(LoadData(new.Config))
      v$data.exists = class(v$inputdata) == "PROsetta.Data"
      v$text = get.data.status(v$data.exists)

      if (v$data.exists){
        v$active.tabset = unique(c(v$active.tabset, 1))
        switch.tabs(v$active.tabset)

        updateCheckboxGroupButtons(
          session = session,
          inputId = "tabvisibility",
          selected = as.character(v$active.tabset)
        )
      }

      switch.main.buttons(v$data.exists)
    }
  })

  observeEvent(input$rundescriptive, {

    switch.main.buttons(F)

    v$text = "Running.."
    v$time = Sys.time()

    new.Config = new.config(anchorFile = input$anchor.file$datapath,
                            responseFile = input$response.file$datapath,
                            itemmapFile = input$itemmap.file$datapath,
                            linkingMethod = input$linking.type,
                            itemID = input$item.id,
                            personID = input$person.id,
                            scaleID = input$scale.id)
    assign.object("shiny.config", new.Config, "PROsetta.Config object")
    v$inputdata = LoadData(new.Config)
    assign.object("shiny.data", v$inputdata, "PROsetta.Data object")
    v$freqtable = RunFrequency(new.Config, v$inputdata)
    assign.object("shiny.freq", v$freqtable, "Frequency table tab")
    v$desctable = RunDescriptive(new.Config, v$inputdata)
    assign.object("shiny.desc", v$desctable, "Descriptives tab")
    v$classical = RunClassical(new.Config, v$inputdata)
    assign.object("shiny.alpha", v$classical, "Classical tab")
    v$classical2 = RunClassical(new.Config, v$inputdata, omega = T, fm = "ml")[["Omega"]]
    assign.object("shiny.omega", v$classical2, "Classical (omega) tab")

    v$time = Sys.time() - v$time
    v$text = paste0("Done in ", sprintf("%3.3f", v$time), "s")

    v$active.tabset = unique(c(v$active.tabset, 2))
    switch.tabs(v$active.tabset)

    updateCheckboxGroupButtons(
      session = session,
      inputId = "tabvisibility",
      selected = as.character(v$active.tabset)
    )

    updateCheckboxGroupButtons(
      session = session,
      inputId = "rundescriptive",
      selected = character(0)
    )

    switch.main.buttons(T)

  })


  observeEvent(input$runcalibration, {

    switch.main.buttons(F)

    progress = Progress$new(session)
    on.exit(progress$close())
    progress$set(message = 'Computing..',
                 detail = 'This may take a while.')

    v$text = "Running.."
    v$time = Sys.time()

    new.Config = new.config(anchorFile = input$anchor.file$datapath,
                            responseFile = input$response.file$datapath,
                            itemmapFile = input$itemmap.file$datapath,
                            linkingMethod = input$linking.type,
                            itemID = input$item.id,
                            personID = input$person.id,
                            scaleID = input$scale.id)
    assign.object("shiny.config", new.Config, "PROsetta.Config object")

    v$inputdata = LoadData(new.Config)
    assign.object("shiny.data", v$inputdata, "PROsetta.Data object")

    updateSliderTextInput(
      session = session,
      inputId = "item.id.to.plot",
      choices = seq(1, v$n.items),
      selected = min(v$item.id.to.plot, v$n.items)
    )

    v$outCalib = RunCalibration(new.Config, v$inputdata)
    assign.object("shiny.calib", v$outCalib, "Calibration result (full object)")
    v$calib.params = mirt::coef(v$outCalib, IRTpars = TRUE, simplify = TRUE)$items
    assign.object("shiny.params", v$calib.params, "Calibration result tab")
    v$plot.itemfit  = mirt::itemfit(v$outCalib, empirical.plot = v$item.id.to.plot)
    assign.object("shiny.itemfit", v$plot.itemfit, "Item fit plot tab")
    v$plot.iteminfo = mirt::itemplot(v$outCalib, item = v$item.id.to.plot, type = "info")
    assign.object("shiny.iteminfo", v$plot.iteminfo, "Item info tab")

    tmp = try(mirt::itemfit(v$outCalib, "S_X2", na.rm = TRUE), silent = T)
    if (class(tmp)[1] == "try-error"){
      tmp = try(mirt::itemfit(v$outCalib, "S_X2"))
    }

    v$table.itemfit = tmp
    assign.object("shiny.itemfittable", v$table.itemfit, "Item fit table tab")

    v$crosswalk.calibration = RunRSSS(new.Config, v$inputdata, v$outCalib)
    assign.object("shiny.crosswalk.calibration", v$crosswalk.calibration, "Crosswalk (calibration) tab")

    v$time = Sys.time() - v$time
    v$text = paste0("Done in ", sprintf("%3.3f", v$time), "s")

    v$active.tabset = unique(c(v$active.tabset, 3))
    switch.tabs(v$active.tabset)

    updateCheckboxGroupButtons(
      session = session,
      inputId = "tabvisibility",
      selected = as.character(v$active.tabset)
    )

    updateCheckboxGroupButtons(
      session = session,
      inputId = "runcalibration",
      selected = character(0)
    )

    switch.main.buttons(T)

  })



  observeEvent(input$runlinking, {
    switch.main.buttons(F)
    if (!(input$linking.type %in% c("MM", "MS", "HB", "SL", "LS"))){
      v$text = 'Linking method must be one of the following: "MM", "MS", "HB", "SL", "LS".'
    } else {
      progress = Progress$new(session)
      on.exit(progress$close())
      progress$set(message = 'Computing..',
                   detail = 'This may take a while.')

      v$text = "Running.."
      v$time = Sys.time()

      new.Config = new.config(anchorFile = input$anchor.file$datapath,
                              responseFile = input$response.file$datapath,
                              itemmapFile = input$itemmap.file$datapath,
                              linkingMethod = input$linking.type,
                              itemID = input$item.id,
                              personID = input$person.id,
                              scaleID = input$scale.id)
      assign.object("shiny.config", new.Config, "PROsetta.Config object")

      v$inputdata = LoadData(new.Config)
      assign.object("shiny.data", v$inputdata, "PROsetta.Data object")

      v$outequate = RunLinking(new.Config, v$inputdata, technical = list(NCYCLES = 1000))
      assign.object("shiny.link", v$outequate, "Linking result (full object)")
      v$linking.constants = v$outequate$link@constants$SL
      assign.object("shiny.link.ab", v$linking.constants, "Linking constants tab")
      v$transformed.params = v$outequate$pars@pars$From
      assign.object("shiny.transformed.params", v$transformed.params, "Transformed parameters tab")

      v$crosswalk.linking = RunRSSS(new.Config, v$inputdata, v$outequate)
      assign.object("shiny.crosswalk.linking", v$crosswalk.linking, "Crosswalk (linking) tab")

      v$time = Sys.time() - v$time
      v$text = paste0("Done in ", sprintf("%3.3f", v$time), "s")

      v$active.tabset = unique(c(v$active.tabset, 4))
      switch.tabs(v$active.tabset)

      updateCheckboxGroupButtons(
        session = session,
        inputId = "tabvisibility",
        selected = as.character(v$active.tabset)
      )

      updateCheckboxGroupButtons(
        session = session,
        inputId = "runlinking",
        selected = character(0)
      )
    }
    switch.main.buttons(T)
  })




  observeEvent(input$runequating, {

    switch.main.buttons(F)

    v$text = "Running.."
    v$time = Sys.time()

    new.Config = new.config(anchorFile = input$anchor.file$datapath,
                            responseFile = input$response.file$datapath,
                            itemmapFile = input$itemmap.file$datapath,
                            linkingMethod = input$linking.type,
                            itemID = input$item.id,
                            personID = input$person.id,
                            scaleID = input$scale.id)
    assign.object("shiny.config", new.Config, "PROsetta.Config object")

    v$inputdata = LoadData(new.Config)
    assign.object("shiny.data", v$inputdata, "PROsetta.Data object")

    v$outequateequipercentile = RunEquateObserved(new.Config, v$inputdata, scaleTo = 1, scaleFrom = 2, type = "equipercentile", smooth = "loglinear")
    assign.object("shiny.eq", v$outequateequipercentile, "Equating tab")

    v$time = Sys.time() - v$time
    v$text = paste0("Done in ", sprintf("%3.3f", v$time), "s")

    v$active.tabset = unique(c(v$active.tabset, 5))
    switch.tabs(v$active.tabset)

    updateCheckboxGroupButtons(
      session = session,
      inputId = "tabvisibility",
      selected = as.character(v$active.tabset)
    )

    updateCheckboxGroupButtons(
      session = session,
      inputId = "runequating",
      selected = character(0)
    )

    switch.main.buttons(T)

  })

  observeEvent(input$item.id.to.plot, {
    if (is.text.parsable(input$item.id.to.plot)){
      eval(parse(text = paste0("item.id.to.plot = c(", input$item.id.to.plot, ")[1]")))
      item.id.to.plot = min(item.id.to.plot, v$n.items)
      item.id.to.plot = max(1, item.id.to.plot)
      v$item.id.to.plot = item.id.to.plot
      if (is.null(v$outCalib)) return()
      v$plot.itemfit  = mirt::itemfit(v$outCalib, empirical.plot = v$item.id.to.plot)
      assign.object("shiny.itemfit", v$plot.itemfit, "Item fit plot tab")
      v$plot.iteminfo = mirt::itemplot(v$outCalib, item = v$item.id.to.plot, type = "info")
      assign.object("shiny.iteminfo", v$plot.iteminfo, "Item info tab")
    }
  })

  output$textoutput               = renderText(return.object.or.null(v$text))

  output$anchor.data              = renderDT(return.object.or.null(v$anchor.data),           options = list(pageLength = 100))
  output$response.data            = renderDT(return.object.or.null(v$response.data),         options = list(pageLength = 100))
  output$itemmap.data             = renderDT(return.object.or.null(v$itemmap.data),          options = list(pageLength = 100))

  output$freqtable                = renderDT(return.object.or.null(v$freqtable),             options = list(pageLength = 100))
  output$desctable                = renderDT(return.object.or.null(v$desctable, 3),          options = list(pageLength = 100))
  output$classical                = renderPrint(return.object.or.null(v$classical))
  output$classical2               = renderPrint(return.object.or.null(v$classical2))

  output$calib.params             = renderDT(return.object.or.null(v$calib.params, 3),       options = list(pageLength = 100))
  output$plot.itemfit             = renderPlot(return.object.or.null(v$plot.itemfit))
  output$plot.iteminfo            = renderPlot(return.object.or.null(v$plot.iteminfo))
  output$table.itemfit            = renderDT(return.object.or.null(v$table.itemfit),         options = list(pageLength = 100))
  output$crosswalk.calibration    = renderPrint(return.object.or.null(v$crosswalk.calibration))

  output$linking.constants        = renderPrint(return.object.or.null(v$linking.constants))
  output$table.transformed.params = renderDT(return.object.or.null(v$transformed.params, 3), options = list(pageLength = 100))
  output$crosswalk.linking        = renderPrint(return.object.or.null(v$crosswalk.linking))

  output$equating.constants       = renderPrint(return.object.or.null(v$outequateequipercentile))

  output$exportData = downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".zip", sep="")
    },
    content = function(fname) {
      fs = c()
      tmpdir = tempdir()
      setwd(tempdir())
      for (i in v$active.tabset) {
        if (i == 1){
          if (!is.null(v$anchor.data)){
            path = "raw.data.anchor.csv"
            fs = c(fs, path)
            write.csv(v$anchor.data, path, row.names = F)
          }
          if (!is.null(v$response.data)){
            path = "raw.data.response.csv"
            fs = c(fs, path)
            write.csv(v$response.data, path, row.names = F)
          }
          if (!is.null(v$itemmap.data)){
            path = "raw.data.itemmap.csv"
            fs = c(fs, path)
            write.csv(v$itemmap.data, path, row.names = F)
          }
        }
        if (i == 2){
          if (!is.null(v$freqtable)){
            path = "basic.frequency.csv"
            fs = c(fs, path)
            write.csv(v$freqtable, path)
          }
          if (!is.null(v$desctable)){
            path = "basic.descriptive.csv"
            fs = c(fs, path)
            write.csv(v$desctable, path)
          }
          if (!is.null(v$classical)){
            path = "basic.reliability.alpha.txt"
            fs = c(fs, path)
            tmp = paste0(capture.output(v$classical), collapse = "\n")
            write(tmp, path)
          }
          if (!is.null(v$classical2)){
            path = "basic.reliability.omega.txt"
            fs = c(fs, path)
            tmp = paste0(capture.output(v$classical2), collapse = "\n")
            write(tmp, path)
          }
        }
        if (i == 3){

          if (!is.null(v$calib.params)){
            path = "calib.params.csv"
            fs = c(fs, path)
            write.csv(v$calib.params, path)
          }

          n.items = dim(v$outCalib@Data$data)[2]

          if (!is.null(v$outCalib)){
            path = "calib.itemfit.pdf"
            fs = c(fs, path)
            pdf(path)
            for (id in 1:n.items){
              p = mirt::itemfit(v$outCalib, empirical.plot = id)
              print(p)
            }
            dev.off()

            path = "calib.iteminfo.pdf"
            fs = c(fs, path)
            pdf(path)
            for (id in 1:n.items){
              p = mirt::itemplot(v$outCalib, item = id, type = "info")
              print(p)
            }
            dev.off()
          }

          if (!is.null(v$table.itemfit)){
            path = "calib.fit.csv"
            fs = c(fs, path)
            write.csv(v$table.itemfit, path, row.names = F)
          }
          if (!is.null(v$crosswalk.calibration)){
            path = "crosswalk.calibration.txt"
            fs = c(fs, path)
            tmp = paste0(capture.output(v$crosswalk.calibration), collapse = "\n")
            write(tmp, path)
          }
        }

        if (i == 4){
          if (!is.null(v$linking.constants)){
            path = "linking.constants.csv"
            fs = c(fs, path)
            write.csv(v$linking.constants, path)
          }
          if (!is.null(v$transformed.params)){
            path = "transformed.params.csv"
            fs = c(fs, path)
            write.csv(v$transformed.params, path)
          }
          if (!is.null(v$crosswalk.linking)){
            path = "crosswalk.linking.txt"
            fs = c(fs, path)
            tmp = paste0(capture.output(v$crosswalk.linking), collapse = "\n")
            write(tmp, path)
          }
        }

        if (i == 5){
          if (!is.null(v$outequateequipercentile)){
            path = "equating.constants.txt"
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
