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

    v$time <- difftime(Sys.time(), v$time, units = "secs")
    v <- updateLogs(v, sprintf("Done in %5.1fs : run descriptives", v$time))

    v$active_tabset <- updateTabSet(v$active_tabset, add_tabset = 2, session = session)
    toggleSolverButtons(TRUE, session)

  })

  observeEvent(input$runcalibration, {

    toggleSolverButtons(FALSE, session)

    progress <- Progress$new(session)
    on.exit(progress$close())
    progress$set(
      message = "Computing..",
      detail = "This may take a while."
    )

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

    v$time <- difftime(Sys.time(), v$time, units = "secs")
    v <- updateLogs(v, sprintf("Done in %5.1fs : run calibration (%s)", v$time, calib_type))

    v$active_tabset <- updateTabSet(v$active_tabset, add_tabset = 3, session = session)
    toggleSolverButtons(TRUE, session)

  })

  observeEvent(input$runlinking, {

    toggleSolverButtons(FALSE, session)

    if (!(input$linking_type %in% c("MM", "MS", "HB", "SL", "FIXEDPAR", "CP", "CPLA"))) {
      v <- updateLogs(v, "Linking method must be one of the following: 'MM', 'MS', 'HB', 'SL', 'FIXEDPAR', 'CP', 'CPLA'.")
      break
    }

    progress <- Progress$new(session)
    on.exit(progress$close())
    progress$set(
      message = "Computing..",
      detail = "This may take a while."
    )

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

    v$time <- difftime(Sys.time(), v$time, units = "secs")
    v <- updateLogs(v, sprintf("Done in %5.1fs : run parameter linking (%s)", v$time, input$linking_type))

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

    v$time <- difftime(Sys.time(), v$time, units = "secs")
    v <- updateLogs(v, sprintf("Done in %5.1fs : run equating", v$time))

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
    columnDefs = list(list(className = "dt-right", targets = "_all"))
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
