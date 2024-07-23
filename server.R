#--------------------------#
#     Server Components
#--------------------------#
server <- function(input, output, session) {
  selectedCountryISO3C <-
    reactive(countrycode(sourcevar = input$selectedCountry,
                         "country.name",
                         "iso3c"))

  observe_helpers(help_dir = "helpfiles", withMathJax = TRUE)

  iv <- InputValidator$new()
  iv_alpha <- InputValidator$new()
  iv_cropped <- InputValidator$new()
  iv_seeddataupload <- InputValidator$new()

  iv$add_rule("selectedCountry", sv_required())

  iv_alpha$add_rule("alpha", sv_required())
  iv_alpha$add_rule("alpha", sv_gte(0))

  iv$add_rule("beta", sv_required())
  iv$add_rule("beta", sv_gte(0))

  iv$add_rule("gamma", sv_required())
  iv$add_rule("gamma", sv_gte(0))

  iv$add_rule("sigma", sv_required())
  iv$add_rule("sigma", sv_gte(0))

  iv$add_rule("delta", sv_required())
  iv$add_rule("delta", sv_gte(0))

  iv$add_rule("lambda", sv_required())
  iv$add_rule("lambda", sv_integer())
  iv$add_rule("lambda", sv_gt(0))

  iv$add_rule("date", sv_required())

  iv$add_rule("timestep", sv_required())
  iv$add_rule("timestep", sv_integer())
  iv$add_rule("timestep", sv_gt(0))

  iv_cropped$add_rule("level1List", sv_required())

  iv_seeddataupload$add_rule("seedData", sv_required())
  iv_seeddataupload$add_rule("seedData", ~ if(is.null(fileInputs$smStatus) || fileInputs$smStatus == 'reset') "Required")

  iv_alpha$condition(~ isTRUE(input$modelSelect == "SVEIRD"))
  iv_cropped$condition(~ isTRUE(input$cropLev1))
  iv_seeddataupload$condition(~ isTRUE(input$appMode == "Simulator"))

  iv$add_validator(iv_alpha)
  iv$add_validator(iv_cropped)
  iv$add_validator(iv_seeddataupload)

  #------------------------------------------#
  ## Visualizer Input Validators         ----
  #------------------------------------------#
  # iv <- InputValidator$new()
  iv_dataupload <- InputValidator$new()

  iv_dataupload$add_rule("latLonData", sv_required())
  iv_dataupload$add_rule("latLonData", ~ if(is.null(fileInputs$latLonStatus) || fileInputs$latLonStatus == 'reset') "Required")
  iv_dataupload$add_rule("incidenceData", sv_required())
  iv_dataupload$add_rule("incidenceData", ~ if(is.null(fileInputs$incidenceStatus) || fileInputs$incidenceStatus == 'reset') "Required")

  iv_dataupload$condition(~ isTRUE(input$appMode == "Visualizer"))

  iv$add_validator(iv_dataupload)

  iv$enable()
  iv_alpha$enable()
  iv_cropped$enable()
  iv_seeddataupload$enable()
  iv_dataupload$enable()

  observe({ if(iv$is_valid()) enable("go") else disable("go") })

  observe({
    input$muValue
    updateNumericInput(session, "muBirth", value = 0)
  })
  observe({
    input$muValue
    updateNumericInput(session, "muDeath", value = 0)
  })

  values <- reactiveValues()
  values$allow_simulation_run <- TRUE

  fileInputs <- reactiveValues(
    smStatus = NULL,
    latLonStatus = NULL,
    incidenceStatus = NULL
  )

  susceptible <- reactive({
    shiny::validate(selectedCountryISO3C())
    SusceptibleLayer <- createSusceptibleLayer(selectedCountryISO3C())
    if (req(input$agg) > 1) {
      aggregate(SusceptibleLayer, fact = input$agg, fun = sum, na.rm = TRUE)
    } else {
      SusceptibleLayer
    }
  })


  #==========================================================================#
  # World Pop Visualizer Components                                       ----
  #==========================================================================#
  #--------------------------------------------------------------------------#
  # Display the file inputs for generating the transmission path             #
  #--------------------------------------------------------------------------#
  output$transPathFileInputs <- renderUI({
    req(!is.null(input$selectedCountry) && input$selectedCountry != "")

    tagList(
      fileInput(inputId = "latLonData",
                label = strong("Upload Lat-Lon Data:"),
                placeholder = "Upload Lat-Lon data (.csv or .xls or .xlsx)",
                accept = acceptedFileTypes),
      fileInput(inputId = "incidenceData",
                label = strong("Upload Incidence/Death Data:"),
                placeholder = "Upload Incidence/Death data (.csv or .xls or .xlsx)",
                accept = acceptedFileTypes)
    )
  })

  #--------------------------------------------------------------------------#
  # Dynamically generate a date slider that contains the dates for all the
  # observed data in the incidence/death file
  #--------------------------------------------------------------------------#
  output$transPathDateInput <- renderUI({
    req(iv_dataupload$is_valid() && input$appMode == "Visualizer")

    dateInfo <- colnames(transPathData())[4:length(colnames(transPathData()))]

    sliderTextInput(
      inputId = "transPathDate",
      label = strong("Date"),
      choices = dateInfo,
      selected = dateInfo[1],
      animate = animationOptions(interval = 250, loop = FALSE))
  })


  output$resetButton <- renderUI({ ## resetButton ----
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      actionButton(
        inputId = "visReset",
        label = "Reset Values",
        class = "act-btn")
    }
  })

  output$leafletMap <- renderLeaflet({
    req(input$selectedCountry)
    createLeafletPlot(input$selectedCountry, NULL, susceptible())
  })

  ## WTF
  level1Country <- reactiveVal({
    value = NULL
  })
  observeEvent(input$level1List, {
    level1Country(input$selectedCountry)
  })
  output$croppedLeafletMap <- renderLeaflet({
    req(!is.null(input$selectedCountry) && !is.null(input$level1List))
    req(input$selectedCountry == level1Country())
    susc <- susceptible()$Susceptible
    level1Names <- input$level1List
    createLeafletPlot(input$selectedCountry, level1Names, susc)
  })

  output$terraOutputImage <- renderImage({
    validate(need(!is.null(input$selectedCountry), "Loading App..."))

    if (input$selectedCountry == ""){
      list(src = "", width = 0, height = 0)
    } else {
      outfile <- tempfile(fileext = '.png')

      png(outfile, width = 1024, height = 768)
      createBasePlot(input$selectedCountry, susceptible()$Susceptible, TRUE)
      dev.off()

      list(src = outfile, contentType = 'image/png', width = 1024, height = 768, alt = "Base plot image not found")
    }
  }, deleteFile = TRUE)

  output$transmission <- renderLeaflet({
    req(!is.null(input$selectedCountry))
    req(iv_dataupload$is_valid())

    level1Names <- NULL

    if(input$cropLev1 == TRUE){

      if(!is.null(input$level1List) && !("" %in% input$level1List)){
        level1Names <- input$level1List
      }
    }

    createLeafletBubblePlot(input$selectedCountry, level1Names, transPathData(), 1)
  })

  observe({
    req(!is.null(input$transPathDate))

    transDate <- input$transPathDate

    plotData <- transPathData()

    # To access a column inside the leafletProxy function the column name must
    # be called directly (can't use a variable storing the column name) so we
    # must set the column we want to a known name ("Current")
    colnames(plotData)[colnames(plotData) == transDate] <- "Current"

    labelText <- paste0(
      "Location: ", plotData$Location, "<br/>",
      "Count: ", plotData$Current, "<br/>") %>%
      lapply(htmltools::HTML)

    # To update the map, clear out the old markers and draw new ones using the
    # data from the newly selected date
    leafletProxy("transmission",
                 data = plotData) %>%
      clearMarkers() %>%
      addCircleMarkers(lng = ~Longitude,
                       lat = ~Latitude,
                       radius = ~Current^0.35*2,
                       weight = 1,
                       opacity = 1,
                       color = ~ifelse(Current > 0, "black", "transparent"),
                       fillColor = ~ifelse(Current > 0, colorPalette(Current), "transparent"),
                       fillOpacity = 0.8,
                       label = labelText)
  })

  output$lollipop <- renderPlotly({
    req(iv_dataupload$is_valid())

    p <- plotLolliChart(input$selectedCountry, input$incidenceData$datapath)
    ggplotly(p)
  })

  output$timeSeries <- renderPlotly({
    shiny::validate(iv_dataupload$is_valid())

    ## TODO: this needs to be updated so that it plots the users data.
    plotTimeSeries(here("data", "observeddata", "Ebola_Incidence_Data.xlsx"),
                 sprintf("Time-Series Graph of Incidence/Death in %s",
                         paste0(if (input$selectedCountry %in% prependList)
                                  "the ", input$selectedCountry)),
                 "Time",
                 "Incidence/Death",
                 "#0f0f0f",
                 "Area") %>%
    ggplotly()
  })


  transPathData <- reactive({
    req(iv_dataupload$is_valid() && input$appMode == "Visualizer")

    incidenceData <- openDataFile(input$incidenceData)
    latLonData <- openDataFile(input$latLonData)

    incidence <- as.data.frame(t(incidenceData))
    incidenceCols <- incidence[2,]
    incidence <- incidence[3:nrow(incidence),]
    colnames(latLonData) <- c("Location", "Latitude", "Longitude")
    colnames(incidence) <- incidenceCols

    plotData <- cbind(latLonData, lapply(incidence, as.numeric))
  })

  observeEvent(input$latLonData, {
    fileInputs$latLonStatus <- 'uploaded'
  })

  observeEvent(input$incidenceData, {
    fileInputs$incidenceStatus <- 'uploaded'
  })


  observeEvent({input$selectedCountry
                input$appMode}, {
    if(!is.null(input$selectedCountry) && input$selectedCountry != "" && input$appMode == "Visualizer") {
      shinyjs::show(id = "maptabPanels")
    } else {
      shinyjs::hide(id = "maptabPanels")
    }

    fileInputs$latLonStatus <- 'reset'
    fileInputs$incidenceStatus <- 'reset'
  })

  observeEvent({input$selectedCountry
                input$appMode}, priority = 100, {
    if(input$appMode == "Visualizer") {
      updateTabsetPanel(inputId = "vizTabSet", selected = "Leaflet Plot")
    }
  })

  observeEvent({input$cropLev1
                input$selectedCountry
                input$level1List
                input$appMode}, priority = 100, {
    if(input$cropLev1  == TRUE && input$appMode == "Visualizer" && !is.null(input$level1List)) {
      showTab(inputId = 'vizTabSet', target = 'Leaflet Cropped Plot')
    } else if((input$cropLev1  == FALSE && input$appMode == "Visualizer") || is.null(input$level1List)) {
      hideTab(inputId = 'vizTabSet', target = 'Leaflet Cropped Plot')
      updateTabsetPanel(inputId = "vizTabSet", selected = "Leaflet Plot")
    }
  })

  observeEvent(input$visReset, {
    updateCheckboxInput(inputId = "cropLev1", value = FALSE)
    updatePickerInput(inputId = "selectedCountry", selected = "")

    ## FIXME: WTF
    fileInputs$latLonStatus <- 'reset'
    fileInputs$incidenceStatus <- 'reset'
  })

  observe({
    if(input$appMode == "Visualizer") {
      if(iv_dataupload$is_valid()) {
        showTab(inputId = 'vizTabSet', target = "Transmission Path")
        showTab(inputId = 'vizTabSet', target = "Lollipop Chart")
        showTab(inputId = 'vizTabSet', target = "Time-Series Graph")
      } else {
        hideTab(inputId = 'vizTabSet', target = "Transmission Path")
        hideTab(inputId = 'vizTabSet', target = "Lollipop Chart")
        hideTab(inputId = 'vizTabSet', target = "Time-Series Graph")
      }
    }
  })

  observeEvent(input$go, {
    req(iv$is_valid())
    output$outputImage <- renderImage({

      outfile <- tempfile(fileext = '.png')
      png(outfile, width = 768, height = 768)

      if(input$cropLev1) {
        req(input$level1List != "")
        isolate(createCroppedRaster(selectedCountry = input$selectedCountry,
                                    level1Region = input$level1List,
                                    susceptible()$Susceptible,
                                    directOutput = TRUE))
      } else {
        isolate(createBasePlot(selectedCountry = input$selectedCountry,
                               susceptible()$Susceptible,
                               directOutput = TRUE))
      }

      dev.off()

      list(src = outfile,
           contentType = 'image/png',
           width = 768,
           height = 768,
           alt = "Base plot image not found")
      # The above line adjusts the dimensions of the base plot rendered in UI
    }, deleteFile = TRUE)
  })

  observeEvent(input$go, {
    req(iv$is_valid())
    output$modelImg <- renderImage({
      return(list(src= "www/ModelEquations.png",
                  height = 400,
                  contentType = "image/png"))
    }, deleteFile = FALSE)
  })

  observeEvent(input$go, {
    req(iv$is_valid())
    output$flowchartImg <- renderImage({
      if (input$modelSelect == "SEIRD"){
        return(list(src= "www/SEIRD.png",
                    height = 400,
                    contentType = "image/png"))
      }
      else if (input$modelSelect == "SVEIRD"){
        return(list(src = "www/SVEIRD.png",
                    height = 400,
                    contentType = "image/png"))
      }
    }, deleteFile = FALSE)
  })

  observeEvent(input$resetAll, {
    shinyjs::reset("dashboard")
    shinyjs::disable(id = "go")

    values$allow_simulation_run <- FALSE

  })

  observeEvent(input$seedData, {
    values$allow_simulation_run <- TRUE
    fileInputs$smStatus <- 'uploaded'
  })

  ## MAYBE TODO: inline the pipeline into the following observable.
  provinces <- reactive({
    upperISO3C <- toupper(req(selectedCountryISO3C()))
    path <- paste0("gadm/",
                   "gadm36_", # FIXME NOTE: What does 36 mean?
                   upperISO3C,
                   "_1_sp.rds")
    if (file.exists(path)) {
      readRDS(path)$NAME_1
    }
    else {
      raster::getData("GADM",
                      TRUE,
                      level = 1,
                      country = upperISO3C)$NAME_1
    }
  })

  observe(updateSelectizeInput(session, "level1List", choices = provinces()))

  observe({
    ## Conditionally update the values of the numeric inputs if there is exactly
    ## one row for the selected country.
    defaults <-
      filter(epiparms,
             ISONumeric == req(selectedCountryISO3C()),
             model == isolate(req(input$selectedCountry))) |>
      slice_head()
    if(count(defaults) == 1) defaultteNumericInputs(defaults, session = session)
  })

  ## FIXME: why was the code that Ashok, Michael, and others wrote printing
  ## these information on this event? What did they want to know about the files
  ## uploaded that they didn't already know when they made the file?
  observe({
    print(paste("DEBUG:", read.csv(input$dataAssimZones$datapath)))
    print(paste("DEBUG:", as.character(input$dataAssimZones[1])))
  }) |>
    bindEvent(input$dataAssimZones)

  ## MAYBE FIXME: I anticipate that if a user uploads some compartment's data
  ## then decides they want another compartment, their upload will be removed if
  ## they change the compartments. I fear any change will likely overwrite and
  ## discard the upload. TODO HACK: the code which renders the UI, ensuring that
  ## any nearly-identical DOM elements are untouched; if a Vaccinated
  ## compartment is re-rendered, it should not be reinserted. The DOM subtree
  ## should be mutated, not overwritten.
  output$dataAssimilationCompartmentDataUploaders <-
    renderUI(
      div(
        lapply(input$selectedCompartments,
               function(compartment) {
                 fileInput(paste("dataAssimilation", compartment, sep = "_"),
                           paste("Upload",
                                 switch(compartment,
                                        V = "vaccination/vaccinated",
                                        E = "exposure/exposed",
                                        I = "infection/infected",
                                        R = "recovery/recovered",
                                        D = "death/dead"),
                                 "data to be assimilated with the model"),
                           accept = acceptedFileTypes)
               })))

  ## MAYBE TODO: inline the pipeline into the following observable.
  recommendedRasterAggregationFactor <-
    reactive({
      filter(population,
             Country == input$selectedCountry) |>
        dplyr::select(reco_rasterAgg) |>
        slice_head()
    })

  observe(updateSliderInput(session,
                            "agg",
                            value = recommendedRasterAggregationFactor()))


  lineThickness <- 1.5

  observeEvent(input$go, {
    req(iv$is_valid())

    output$infectedExposedPlot <- renderPlotly({
      p <- makePlot(
        compartments = c("E", "I"),
        selectedCountry = input$selectedCountry,
        plotTitle =
          "Time-series plot of Exposed and Infectious \n compartments in " |>
          paste0(input$selectedCountry),
        xTitle = paste0("Day (from ", input$date, ")"),
        yTitle = "Compartment Value",
        lineThickness = lineThickness)
      ggplotly(p)
    })

    spatialEpisimThemeAndOptions <-
      theme(plot.title = element_text(size = 18,
                                      face = "bold",
                                      margin = margin(0, 0, 25, 0),
                                      hjust = 0.5),
            axis.title.x = element_text(size = 14,
                                        face = "bold",
                                        margin = margin(25, 0, 0, 0)),
            axis.title.y = element_text(size = 14,
                                        face = "bold",
                                        margin = margin(0, 25, 0, 0)),
            axis.text.x.bottom = element_text(size = 14),
            axis.text.y.left = element_text(size = 14),
            axis.line = element_line(linewidth = 0.5),
            plot.margin = unit(c(1, 1, 1, 0),"cm"),
            legend.title = element_text(size = 10, face = "bold"),
            legend.box = "horizontal")

    simulationSummaryPath <-
      reactive(here("www", "MP4", paste(sep = "_",
                                        selectedCountryISO3C(),
                                        "summary.xlsx")))

    simulationSummary <- reactive({
      shiny::validate(need(file.exists(simulationSummaryPath()),
                           message = "Waiting for simulation summary XLSX."))
      read_xlsx(simulationSummaryPath())
    })

    plotSimulationSummaryVariable <-
      function(colour, variableExpression, titling) {
        variableExpression <- enquo(variableExpression)
        ggplotly(ggplot(simulationSummary()) +
                 labs(title = titling,
                      subtitle = paste("in", isolate(input$selectedCountry)),
                      x = "Date",
                      y = "Number of persons") +
                 geom_line(linewidth = 2,
                           color = colour,
                           mapping = aes(x = ymd(Date),
                                         y = !!variableExpression)) +
                 spatialEpisimThemeAndOptions +
                 coord_cartesian(clip="off"))
      }

    output$cumulativeDeaths <-
      renderPlotly(
        plotSimulationSummaryVariable("red", D,
                                      "Estimated Cumulative Deaths"))

    output$dailyIncidence <-
      renderPlotly(
        plotSimulationSummaryVariable("blue", newI,
                                      "Estimated Daily Incidence"))

    output$cumulativeIncidence <-
      renderPlotly(
        plotSimulationSummaryVariable("cyan", cumI,
                                      "Estimated Cumulative Daily Incidence"))

    ## TODO: inline the plotting function in global.R, rather than in a separate
    ## file in R, especially if it is unused elsewhere in the project. If it is
    ## used elsewhere, just make it into a very simple package.
    output$fullPlot <- renderPlotly({
      ggplotly(makePlot(compartments = input$enabledCompartments,
                        selectedCountry = input$selectedCountry,
                        plotTitle = paste0("Time-series plot of epidemic compartments \n in ",
                                           input$selectedCountry),
                        xTitle = paste0("Day (from ", input$date, ")"),
                        yTitle = "Compartment Value",
                        lineThickness = lineThickness))
    })
  })

  observeEvent(input$selectedCountry, {
    sheetName <- sprintf("%s_initialSeedData", selectedCountryISO3C())

    output$downloadData <- downloadHandler(
      filename = \() paste(sheetName, Sys.Date(), ".csv",  sep = ""),
      content = function(sheetName) {
        write.csv(tibble(
          coordinates = coordinates(readRDS(here("gadm",
                                                 paste0("gadm36_",
                                                        selectedCountryISO3C(),
                                                        "_1_sp.rds")))),
          Location = coordinates$NAME_1,
          lat = coordinates[, 2], # TODO: these indices need to be verified.
          lat = coordinates[, 1], # TODO: these indices need to be verified.
          InitialVaccinated = 0,
          InitialExposed = 0,
          InitialInfections = 0,
          InitialRecovered = 0,
          InitialDead = 0), sheetName, row.names = FALSE)
      })

    ## FIXME WTF
    fileInputs$smStatus <- 'reset'
  })

  observe({
    shiny::validate(need(iv$is_valid(), message = "An input is invalid."))

    rs <- createRasterStack(selectedCountry = input$selectedCountry,
                            rasterAgg = input$agg,
                            isCropped = input$cropLev1,
                            level1Names = input$level1List,
                            susceptible = susceptible())

    output$tableSeed <- renderDT({
      path <- req(input$seedData$datapath)
      datatable(if(file_ext(path) %in% 'xlsx')
                  read_excel(path)
                else
                  read.csv(path),
                rownames = FALSE,
                options = list(dom = 't',
                               pageLength = -1,
                               ordering = FALSE,
                               searching = FALSE,
                               paging = FALSE,
                               autoWidth = TRUE,
                               scrollX = TRUE))
    })

    ## FIXME (№20): the simulationSummary reactive cannot be found inside a call
    ## to renderDT; the reactive chain is reforged (inlined) herein (that is,
    ## all the programmer-defined reactives used [simulationSummary,
    ## simmulationSummaryPath, etc.] are inlined).
    output$outputSummary <- renderDT({
      file <- here("www",
                   "MP4",
                   paste(sep = "_",
                         countrycode(sourcevar = input$selectedCountry,
                                     "country.name",
                                     "iso3c"),
                         "summary.xlsx"))

      file.exists(file) |>
      need(message = "Waiting for simulation summary XLSX.") |>
      shiny::validate()

      read_xlsx(file) |>
        datatable(options = list(autoWidth = FALSE, scrollX = TRUE)) |>
        formatRound(columns = 2:15, digits = 0)
    })

    output$dataPlot <- renderPlot(buildPlot())

    radius <- sum(1, if(input$lambda > input$agg)
                       round(((input$lambda - input$agg)/input$agg) + 1e-16))

    with(isolate(reactiveValuesToList(input)), {
      modelArguments <-
        list(modelSelect,
             rs,
             date,
             selectedCountry,
             FALSE,
             agg,
             if(input$modelSelect == "SVEIRD") alpha else 0,
             beta,
             gamma,
             sigma,
             delta,
             radius,
             lambda,
             timestep,
             seedData$datapath,
             as.numeric(seedRadius),
             stochasticSelect == "Deterministic",
             cropLev1,
             level1List,

             ## FIXME: the logic related to the new assimilation uploaders is
             ## unimplemented.
             dataAssim,
             dataAssimZones$datapath,
             assimIData$datapath,
             assimDData$datapath,

             covarianceSelect,
             QVar,
             QCorrLength,
             nbhd,
             psidiag)
      names(modelArguments) <- formals(SpatialCompartmentalModelWithDA)
      do.call(SpatialCompartmentalModelWithDA, modelArguments)
    })

    output$summaryTable <- renderDT({
      datatable(
        tribble(~Variable, ~Value,
                "Country", input$selectedCountry,

                ## TODO: reformat this text.
                "WorldPop Raster Dimension",
                paste0(rs$nRows, " rows x ",
                       rs$nCols, " columns = ",
                       rs$nCells, " grid cells"),

                "Aggregation Factor", input$agg,

                ## TODO: reformat this text.
                "Aggregated Raster Dimension",
                paste0(nrow(rs$rasterStack), " rows x ",
                       ncol(rs$rasterStack), " columns = ",
                       ncell(rs$rasterStack), " grid cells"),

                "Compartmental Model", input$modelSelect,
                "Model Parameters", paste("Rate of vaccination (α): ", alpha,
                                          "Rate of exposure (β): ", beta,
                                          "Rate of infection (γ): ", gamma,
                                          "Rate of recovery (σ): ", sigma,
                                          "Rate of fatality (δ): ", delta),
                "Average Distance Travelled/Day (in km)", input$lambda,
                "Radius (1 = Moore neighbourhood)", radius,
                "Uploaded Seed Data", input$seedData$name,
                "Number of iterations (days)", input$timestep),
        rownames = FALSE,
        options = list(dom = 't',
                       pageLength = -1,
                       ordering = FALSE,
                       searching = FALSE,
                       paging = FALSE,
                       autoWidth = FALSE))
    })

    output$seedPlot <- renderLeaflet({
      req(iv$is_valid())
      seedData <- read.csv(input$seedData$datapath, header = T)
      printCroppedBubbleSeedPlot(input$selectedCountry,
                                 input$seedData$datapath,
                                 level1Names = input$level1List,
                                 6)
    })

    ## TODO: display multiple animations or a selection among available options.
    output$outputVideo <- renderUI({
      tags$video(id = "video",
                 type = "video/mp4",
                 src = "MP4/Infected_MP4.mp4",
                 controls = "controls")
    })
  }) %>% bindEvent(input$go)

  observe({
    shinyjs::hide(id = "tabsetContainer")
    fileInputs$smStatus <- 'reset'
  }) %>% bindEvent(input$resetAll)

  observe({ if(!iv$is_valid()) shinyjs::hide(id = "tabsetContainer") })

  observe({
    shinyjs::show(id = "tabsetContainer")
    updateTabsetPanel(inputId = 'tabSet', selected = 'Input Summary')
    runjs("window.scrollTo(0, 0)")
  }) %>% bindEvent(input$go)

  observe({
    if(iv_seeddataupload$is_valid()) {
      shinyjs::hide(id = "downloadData")
      shinyjs::show(id = "seedRadius")
    } else {
      shinyjs::hide(id = "seedRadius")
      shinyjs::show(id = "downloadData")
    }
  }) %>% bindEvent(input$seedData)
}
