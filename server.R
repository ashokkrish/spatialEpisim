## PROG TODO: the application should be usable without uploading any data if a
## user simply selects some sane defaults like country and perhaps cropping.
server <- function(input, output, session) {
  observe_helpers(help_dir = "markdown", withMathJax = TRUE)

  selectedCountryISO3C <-
    reactive(countrycode(sourcevar = input$selectedCountry,
                         "country.name",
                         "iso3c"))

  ## NOTE: this is not aggregated. The aggregated form is only available from getSVEIRD.SpatRaster().
  susceptible <- reactive({
    shiny::validate(selectedCountryISO3C())
    susceptibleSpatRaster <- getCountryPopulation.SpatRaster(selectedCountryISO3C())
  })

  output$transmissionPathDateInput <- renderUI({
    sliderTextInput(
      inputId = "visualizerDateSlider",
      label = strong("Date"),
      choices = as.vector(animatedProportionalSymbolMapData()$Date),
      selected = (animatedProportionalSymbolMapData()$Date)[1],
      animate = animationOptions(interval = 250, loop = FALSE))
  })

  output$leafletMap <- renderLeaflet({ createLeafletPlot(req(input$selectedCountry), NULL, susceptible()) })
  output$croppedLeafletMap <- renderLeaflet({
    createLeafletPlot(req(input$selectedCountry), req(input$level1List), susceptible())
  })

  ## FIXME: rewrite this so that it isn't spoopy.
  output$terraOutputImage <- renderImage({
    outfile <- tempfile(fileext = '.png')
    png(outfile, width = 1024, height = 768)
    createBasePlot(req(input$selectedCountry), susceptible(), TRUE)
    dev.off()
    list(src = outfile, contentType = 'image/png', width = 1024, height = 768, alt = "Base plot image not found")
  }, deleteFile = TRUE)

  output$transmission <- renderLeaflet({
    shiny::validate(need(input$selectedCountry))
    shiny::validate(need(input$cropLev1))
    shiny::validate(need(input$level1List))
    createLeafletBubblePlot(input$selectedCountry, input$level1List, animatedProportionalSymbolMapData(), 1)
  })

  observe({
    visualizerDate <- req(input$visualizerDateSlider)
    plotData <- animatedProportionalSymbolMapData()

    ## FIXME: what?
    # To access a column inside the leafletProxy function the column name must
    # be called directly (can't use a variable storing the column name) so we
    # must set the column we want to a known name ("Current")
    colnames(plotData)[colnames(plotData) == visualizerDate] <- "Current"

    labelText <- htmltools::HTML(sprintf(r"[Location: %s<br/>Count: %s<br/>]", plotData$Location, plotData$Current))

    # To update the map, clear out the old markers and draw new ones using the
    # data from the newly selected date
    leafletProxy("transmission", data = plotData) %>%
      clearMarkers() %>%
      addCircleMarkers(lng = ~ Longitude,
                       lat = ~ Latitude,
                       radius = ~ Current^0.35 * 2,
                       weight = 1,
                       opacity = 1,
                       color = ~ ifelse(Current > 0, "black", "transparent"),
                       fillColor = ~ ifelse(Current > 0, colorPalette(Current), "transparent"),
                       fillOpacity = 0.8,
                       label = labelText)
  })

  output$lollipop <- renderPlotly({ ggplotly(plotLolliChart(input$selectedCountry, input$incidenceData$datapath)) })

  output$timeSeries <- renderPlotly({
    ## TODO: this plotting function needs to take data, not a filename; incidenceData, likewise, needs to be data, not a
    ## filename.
    ggplotly(plotTimeSeries(input$incidenceData,
                            sprintf("Time-Series Graph of Incidence/Death in %s",
                                    paste(if (input$selectedCountry %in% prependList) "the",
                                          input$selectedCountry)),
                            "Time",
                            "Incidence/Death",
                            "#0f0f0f", # FIXME: NO MAGIC NUMBERS!
                            "Area"))
  })

  ## TODO: the sort of data we'd like, given my plans to use Leaflet.TimeDimension to replace this (see issue №44), is
  ## GeoJSON. Converting a dataframe to GeoJSON is easy, the only thing needed is to understand the shape of the data
  ## that that Leaflet.TimeDimension would prefer. Long? Wide? Tidy? See the following link for more information.
  ## https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html
  animatedProportionalSymbolMapData <- reactive({
    req(input$appMode == "Visualizer")

    incidence <- as.data.frame(t(openDataFile(input$incidenceData)))
    incidence <- incidence[3:nrow(incidence), ]
    colnames(incidence) <- "comment<-"(incidence[2, ], "Convert the Date column observations to column names")

    latLonData <- openDataFile(input$latLonData)
    colnames(latLonData) <- c("Location", "Latitude", "Longitude")
    plotData <- cbind(latLonData, lapply(incidence, as.numeric))
  })

  observe({
    output$inputSummaryPlotBelowDataTable <- renderImage({

      outfile <- tempfile(fileext = '.png')
      png(outfile, width = 768, height = 768)

      if (input$cropLev1 && input$level1List != "")
        createCroppedRaster(selectedCountry = input$selectedCountry,
                            level1Region = input$level1List,
                            susceptible(),
                            directOutput = TRUE)
      else
        createBasePlot(selectedCountry = input$selectedCountry, susceptible(), directOutput = TRUE)

      dev.off()

      list(src = outfile, contentType = 'image/png', alt = "Base plot image not found")
      # The above line adjusts the dimensions of the base plot rendered in UI
    }, deleteFile = TRUE)
  }) %>% bindEvent(input$go)

  observeEvent(input$resetAll, { shinyjs::reset("dashboard") })

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
             ## FIXME: model should not be a duplicate of the selected country, when ISONumeric is already the selected
             ## country.
             model == isolate(req(input$selectedCountry))) %>%
      slice_head()
    if(count(defaults) == 1) defaultteNumericInputs(defaults, session = session)
  })

  ## FIXME: why was the code that Ashok, Michael, and others wrote printing these information on this event? What did
  ## they want to know about the files uploaded that they didn't already know when they made the file?
  observe({
    print(paste("DEBUG:", read.csv(input$dataAssimZones$datapath))) # a path to a directory or file
    print(paste("DEBUG:", as.character(input$dataAssimZones[1]))) # perhaps health zones?
  }) %>% bindEvent(input$dataAssimZones)

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

  recommendedRasterAggregationFactor <- reactive({
    filter(population, Country == input$selectedCountry) %>% dplyr::select(reco_rasterAgg) %>% slice_head()
  })
  observe(updateSliderInput(session, "agg", value = recommendedRasterAggregationFactor()))

  observeEvent(input$go, {
    output$infectedExposedPlot <- renderPlotly({
      p <- makePlot(
        compartments = c("E", "I"),
        selectedCountry = input$selectedCountry,
        plotTitle =
          "Time-series plot of Exposed and Infectious \n compartments in " %>%
          paste0(input$selectedCountry),
        xTitle = paste0("Day (from ", input$date, ")"),
        yTitle = "Compartment Value",
        lineThickness = lineThickness)
      ggplotly(p)
    })

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
                 coord_cartesian(clip="off"))
      }

    output$cumulativeDeaths <- renderPlotly(plotSimulationSummaryVariable("red", D, "Estimated Cumulative Deaths"))
    output$dailyIncidence <- renderPlotly(plotSimulationSummaryVariable("blue", newI, "Estimated Daily Incidence"))
    output$cumulativeIncidence <- renderPlotly(plotSimulationSummaryVariable("cyan", cumI, "Estimated Cumulative Daily Incidence"))

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

  ## TODO: this shoudl be improved massively.
  observe({
    sheetName <- sprintf("%s_initialSeedData", selectedCountryISO3C())
    output$downloadData <- downloadHandler(
      filename = \() paste(sheetName, Sys.Date(), ".csv",  sep = ""),
      content = function(sheetName) {
        write.csv(tibble(
          coordinates = coordinates(readRDS(here("gadm", paste0("gadm36_", selectedCountryISO3C(), "_1_sp.rds")))),
          Location = coordinates$NAME_1,
          lat = coordinates[, 2], # TODO: these indices need to be verified.
          lat = coordinates[, 1], # TODO: these indices need to be verified.
          InitialVaccinated = 0,
          InitialExposed = 0,
          InitialInfections = 0,
          InitialRecovered = 0,
          InitialDead = 0), sheetName, row.names = FALSE)
      })
  }) %>% bindEvent(input$selectedCountry)

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

    ## NOTE: when the user has run a simulation, scroll to the top of the page so
    ## that they can see the tab panel buttons and the output. TODO: include a
    ## scroll to top button in the UI when appropriate, and otherwise make the
    ## model configuration panel scrollable without scrolling the main output
    ## area (make them scroll separately).
    shinyjs::show(id = "tabsetContainer")
    updateTabsetPanel(inputId = 'tabSet', selected = 'Input Summary')
    runjs(r"[window.scrollTo(0, 0);]")
  }) %>% bindEvent(input$go)
}
