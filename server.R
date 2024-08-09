server <- function(input, output, session) {
  selectedCountryISO3C <-
    reactive(countrycode(sourcevar = input$selectedCountry,
                         "country.name",
                         "iso3c"))

  observe_helpers(help_dir = "helpfiles", withMathJax = TRUE)

  ## NOTE: this is not aggregated. The aggregated form is only available from getSVEIRD.SpatRaster().
  susceptible <- reactive({
    shiny::validate(selectedCountryISO3C())
    susceptibleSpatRaster <- getCountryPopulation.SpatRaster(selectedCountryISO3C())
  })

  #--------------------------------------------------------------------------#
  # Display the file inputs for generating the transmission path             #
  #--------------------------------------------------------------------------#
  output$transmissionPathFileInputs <- renderUI({
    req(!is.null(input$selectedCountry) && input$selectedCountry != "")

    tagList(
      fileInput(inputId = "latLonData",
                label = strong("Health Zone centroid coordinates"),
                placeholder = "Upload Lat-Lon data",
                accept = acceptedFileTypes),
      fileInput(inputId = "incidenceData",
                label = strong("Incidence & Deaths"),
                placeholder = "Upload Incidence/Death data",
                accept = acceptedFileTypes)
    )
  })

  # NOTE: the date input slider needs to be limited to the range of dates the
  # observed data. THEM: Dynamically generate a date slider that contains the dates
  # for all the observed data in the incidence/death file.
  output$transmissionPathDateInput <- renderUI({
    req(input$appMode == "Visualizer")

    dateInfo <- colnames(transmissionPathData())[4:length(colnames(transmissionPathData()))]

    sliderTextInput(
      inputId = "transPathDate",
      label = strong("Date"),
      choices = dateInfo,
      selected = dateInfo[1],
      animate = animationOptions(interval = 250, loop = FALSE))
  })

  output$leafletMap <- renderLeaflet({
    req(input$selectedCountry)
    createLeafletPlot(input$selectedCountry, NULL, susceptible())
  })

  output$croppedLeafletMap <- renderLeaflet({
    createLeafletPlot(req(input$selectedCountry), req(input$level1List), susceptible())
  })

  ## FIXME: rewrite this so that it isn't spoopy.
  output$terraOutputImage <- renderImage({
    ## TODO: that's not what is happening when the user hasn't selected a country!
    shiny::validate(need(input$selectedCountry, message = "Loading app..."))

    outfile <- tempfile(fileext = '.png')

    png(outfile, width = 1024, height = 768)
    createBasePlot(input$selectedCountry,
                   susceptible(),
                   TRUE)
    dev.off()

    list(src = outfile,
         contentType = 'image/png',
         width = 1024,
         height = 768,
         alt = "Base plot image not found")
  }, deleteFile = TRUE)

  output$transmission <- renderLeaflet({
    shiny::validate(need(input$selectedCountry))
    shiny::validate(need(input$cropLev1))
    shiny::validate(need(input$level1List))

    createLeafletBubblePlot(input$selectedCountry,
                            input$level1List,
                            transmissionPathData(),
                            1)
  })

  observe({
    req(!is.null(input$transPathDate))

    transDate <- input$transPathDate

    plotData <- transmissionPathData()

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
    p <- plotLolliChart(input$selectedCountry, input$incidenceData$datapath)
    ggplotly(p)
  })

  output$timeSeries <- renderPlotly({
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


  transmissionPathData <- reactive({
    req(input$appMode == "Visualizer")

    incidenceData <- openDataFile(input$incidenceData)
    latLonData <- openDataFile(input$latLonData)

    incidence <- as.data.frame(t(incidenceData))
    incidenceCols <- incidence[2,]
    incidence <- incidence[3:nrow(incidence),]
    colnames(latLonData) <- c("Location", "Latitude", "Longitude")
    colnames(incidence) <- incidenceCols

    plotData <- cbind(latLonData, lapply(incidence, as.numeric))
  })

  observeEvent({input$selectedCountry
                input$appMode}, {
    if(!is.null(input$selectedCountry) && input$selectedCountry != "" && input$appMode == "Visualizer") {
      shinyjs::show(id = "maptabPanels")
    } else {
      shinyjs::hide(id = "maptabPanels")
    }
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
  })

  ## FIXME: a better option would be to enable or disable the buttons of the tab
  ## when the data is valid or invalid.
  ## observe({
  ##   if(input$appMode == "Visualizer") {
  ##     if(iv_dataupload$is_valid()) {
  ##       showTab(inputId = 'vizTabSet', target = "Transmission Path")
  ##       showTab(inputId = 'vizTabSet', target = "Lollipop Chart")
  ##       showTab(inputId = 'vizTabSet', target = "Time-Series Graph")
  ##     } else {
  ##       hideTab(inputId = 'vizTabSet', target = "Transmission Path")
  ##       hideTab(inputId = 'vizTabSet', target = "Lollipop Chart")
  ##       hideTab(inputId = 'vizTabSet', target = "Time-Series Graph")
  ##     }
  ##   }
  ## })

  ## FIXME: plot the susceptible layer, cropped or not.
  observeEvent(input$go, {
    output$outputImage <- renderImage({

      outfile <- tempfile(fileext = '.png')
      png(outfile, width = 768, height = 768)

      if(input$cropLev1) {
        req(input$level1List != "")
        isolate(createCroppedRaster(selectedCountry = input$selectedCountry,
                                    level1Region = input$level1List,
                                    susceptible(),
                                    directOutput = TRUE))
      } else {
        isolate(createBasePlot(selectedCountry = input$selectedCountry,
                               susceptible(),
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

  observeEvent(input$resetAll, {
    shinyjs::reset("dashboard")
    ## NOTE: the application should be usable without uploading any data if a
    ## user simply selects some sane defaults like country and perhaps cropping.
    ## shinyjs::disable(id = "go")
  })

  provinces <- reactive({
    path <- here("gadm", sprintf("gadm36_%s_1_sp.rds", req(selectedCountryISO3C())))
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

  observeEvent(input$go, {
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

    ## FIXME: kill this; awful. No.
    simulationSummaryPath <-
      reactive({ here("www", "MP4", paste(sep = "_", selectedCountryISO3C(), "summary.xlsx")) })
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

    ## FIXME: these are kind of awful; DONT do it this way.
    output$cumulativeDeaths <-
      list("red", D, "Estimated Cumulative Deaths") %>%
      do.call(what = plotSimulationSummaryVariable, args = .) %>%
      renderPlotly()
    output$dailyIncidence <-
      list("blue", newI, "Estimated Daily Incidence") %>%
      do.call(what = plotSimulationSummaryVariable, args = .) %>%
      renderPlotly()
    output$cumulativeIncidence <-
      list("cyan", cumI, "Estimated Cumulative Daily Incidence") %>%
      do.call(what = plotSimulationSummaryVariable, args = .) %>%
      renderPlotly()

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
  })

  observe({
    ## TODO: validate all inputs before proceeding with the rest of the
    ## statements in the expression.

    SVEIRD.SpatRaster <- getSVEIRD.SpatRaster(subregions, susceptible(), input$agg)

    output$tableSeed <- renderDT({
      datatable(req(seedData), # TODO: must be read and created prior
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
      datatable(outputSummary,
                options = list(autoWidth = FALSE, scrollX = TRUE)) %>%
        formatRound(columns = 2:15, digits = 0)
    })

    output$dataPlot <- renderPlot(buildPlot())

    radius <- sum(1, if(input$lambda > input$agg)
                       round(((input$lambda - input$agg)/input$agg) + 1e-16))

    ## TODO: other code depends, in bad fashion, on this function having
    ## multiple, awful side effects (creating plots, MP4, XLSX, etc.); that must
    ## be addressed. The new function called here is good, though. DONT change
    ## it to accommodate other bad code elsewhere.
    with(isolate(reactiveValuesToList(input)), {
      modelArguments <-
        list(modelSelect,
             SVEIRD.SpatRaster,
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
      names(modelArguments) <- formals(SVEIRD.BayesianDataAssimilation)
      model <- do.call(SVEIRD.BayesianDataAssimilation, modelArguments)
    })

    ## NOTE: this summary doesn't need to include user-input model configuration
    ## values, given that their configuration should still be visible in the
    ## left-hand side of the application.
    output$summaryTable <- renderDT({
      datatable(
        tribble(~Variable, ~Value,
                ## TODO: reformat this text.
                "WorldPop Raster Dimension",
                paste0(SVEIRD.SpatRaster$nRows, " rows x ",
                       SVEIRD.SpatRaster$nCols, " columns = ",
                       SVEIRD.SpatRaster$nCells, " grid cells"),

                "Aggregated Raster Dimension",
                sprintf("%s rows ✕ %s columns = %s grid cells",
                        SVEIRD.SpatRaster$rows, # TODO: verify
                        SVEIRD.SpatRaster$columns, # TODO: verify
                        SVEIRD.SpatRaster$cells) # TODO: verify
                ),
        rownames = FALSE,
        options = list(dom = 't',
                       pageLength = -1,
                       ordering = FALSE,
                       searching = FALSE,
                       paging = FALSE,
                       autoWidth = FALSE))
    })

    ## FIXME: create a Leaflet plot of the seed values.
    output$seedPlot <- renderLeaflet({
      ## MAYBE TODO: reading the data in the background would be interesting,
      ## but difficult to implement given R doens't have great asynchronous
      ## programming support. Regardless, reading the data here and now is
      ## inappropriate.
      seedData <- read.csv(input$seedDataPath, header = T) ## NOP🛷
      printCroppedBubbleSeedPlot(input$selectedCountry,
                                 input$seedData$datapath,
                                 level1Names = input$level1List,
                                 ## NOTE: this is the "current" column. FIXME:
                                 ## magic number, regardless of the badly named
                                 ## identifier.
                                 activeCol = 6)
    })

    ## TODO: display multiple animations or a selection among available options.

    ## NOTE: when the user has run a simulation, scroll to the top of the page so
    ## that they can see the tab panel buttons and the output. TODO: include a
    ## scroll to top button in the UI when appropriate, and otherwise make the
    ## model configuration panel scrollable without scrolling the main output
    ## area (make them scroll separately).
    shinyjs::show(id = "tabsetContainer")
    updateTabsetPanel(inputId = 'tabSet', selected = 'Input Summary')
    runjs("window.scrollTo(0, 0)")
  }) %>% bindEvent(input$go)

  ## FIXME: update this, obviously, using the new seed data validation that Toby wrote.
  observe({
    if (iv_seeddataupload$is_valid()) shinyjs::enable(id = "seedRadius")
    else shinyjs::disable(id = "seedRadius")
  }) %>% bindEvent(input$seedData)
}
