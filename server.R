#--------------------------#
#     Server Components
#--------------------------#
server <- function(input, output, session) {
  packages_used <- c("av", "bslib", "cptcity", "countrycode",
                     "deSolve","dplyr","DT","ggplot2","htmltools",
                     "latex2exp","lattice","latticeExtra","leaflet",
                     "maps","markdown","plotly","purrr","rasterVis",
                     "readr","readxl","writexl","sf","shiny","shinyalert",
                     "shinybusy","shinyhelper","shinyjs","shinyBS",
                     "shinyvalidate","shinyWidgets","sp","stringr",
                     "terra","tidyverse","tinytex","here","rnaturalearth") 
  
  get_relevant_citation <- function(pkg) {
    citation_info <- tryCatch(citation(pkg), error = function(e) NULL)
    if (!is.null(citation_info)) {
      citation_text <- capture.output(print(citation_info))
      # Find the index of the "A BibTeX entry for LaTeX users is"
      bibtex_index <- grep("A BibTeX entry for LaTeX users is", citation_text)
      # Extract the relevant part
      if (length(bibtex_index) > 0) {
        relevant_part <- citation_text[2:(bibtex_index - 1)]
      } else {
        relevant_part <- citation_text[2:length(citation_text)]
      }
      relevant_part <- paste(relevant_part, collapse = "\n")
      # Ensure proper HTML rendering
      relevant_part <- gsub("<", "&lt;", relevant_part)
      relevant_part <- gsub(">", "&gt;", relevant_part)
    } else {
      relevant_part <- "Citation information not available"
    }
    return(relevant_part)
  }
  
  # Get the relevant part of citations for all packages
  package_citations <- lapply(packages_used, get_relevant_citation)
  
  output$packageList <- renderUI({
    packages <- packages_used
    citations <- package_citations
    
    panels <- lapply(seq_along(packages), function(i) {
      bsCollapsePanel(
        title = packages[i],
        content = tags$div(style = "padding: 10px;", HTML(citations[[i]])),
        style = "primary"
      )
    })
    do.call(bsCollapse, c(id = "collapseExample", open = NULL, panels))
  })
  
  selectedCountryISO3C <-
    reactive(countrycode(sourcevar = input$selectedCountry,
                         "country.name",
                         "iso3c"))

  observe_helpers(help_dir = "helpfiles", withMathJax = TRUE)

  iv <- InputValidator$new()
  iv_alpha <- InputValidator$new()
  iv_cropped <- InputValidator$new()
  iv_seeddataupload <- InputValidator$new()

  # Non-spatial Modelling

  # muValue
  # iv$add_rule("muBirth", sv_required())
  # iv$add_rule("muBirth", sv_gte(0))
  #iv$add_rule("muBirth", sv_lte(0.1))

  # iv$add_rule("muDeath", sv_required())
  # iv$add_rule("muDeath", sv_gte(0))
  #iv$add_rule("muDeath", sv_lte(0.1))

  # Spatial Modelling

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

  observe({
    if(iv$is_valid()){
      shinyjs::enable(id = "go")
    } else {
      shinyjs::disable(id = "go")
    }
  })

  # observeEvent(iv$is_valid(), {
  #   shinyjs::enable(id = "go")
  # })
  # Reset vital dynamics when not checked off
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

  # output$table <- renderTable(values$df)

  susceptible <- reactive({
    req(!is.null(input$selectedCountry) && input$selectedCountry != "")

    createSusceptibleLayer(input$selectedCountry, input$agg)
  })


  #==========================================================================#
  # World Pop Visualizer Components                                       ----
  #==========================================================================#
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
    req(!is.null(input$selectedCountry))

    susc <- susceptible()$Susceptible
    level1Names <- NULL

    createLeafletPlot(input$selectedCountry, level1Names, susc)
  })

  output$croppedLeafletMap <- renderLeaflet({
    req(!is.null(input$selectedCountry) && !is.null(input$level1List))
    req(input$selectedCountry == level1Country())

    susc <- susceptible()$Susceptible
    level1Names <- input$level1List

    createLeafletPlot(input$selectedCountry, level1Names, susc)
  })

  output$terraOutputImage <- renderImage({ ## outputImage ----
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning

    if (input$selectedCountry == ""){
      list(src = "", width = 0, height = 0)
    } else {
      outfile <- tempfile(fileext = '.png')

      # createBasePlot(input$selectedCountry, 1, FALSE) # print the susceptible plot to www/

      # png(outfile, width = 800, height = 600)
      png(outfile, width = 1024, height = 768)
      createBasePlot(input$selectedCountry, susceptible()$Susceptible, TRUE)   # print the susceptible plot direct to UI
      dev.off()

      # list(src = outfile, contentType = 'image/png', width = 800, height = 600, alt = "Base plot image not found")
      list(src = outfile, contentType = 'image/png', width = 1024, height = 768, alt = "Base plot image not found")
    }
  }, deleteFile = TRUE)


createGeoFeatures <- function(incidenceData, palette_colors, bins) {
  incidenceData %>%
    pivot_longer(cols = -c(Location, Latitude, Longitude), 
                names_to = "date", 
                values_to = "cases") %>%
    filter(cases != 0) %>%
    mutate(
      color = palette_colors[findInterval(cases, bins, rightmost.closed = TRUE)],
      time = paste0(date, "T00:00:00.000Z"),
      label = paste0(Location, " - Cases: ", cases)
    ) %>%
    pmap(function(Longitude, Latitude, time, color, label, ...) {
      list(
        type = "Feature",
        geometry = list(
          type = "Point",
          coordinates = list(Longitude, Latitude)
        ),
        properties = list(
          time = time,
          color = color,
          label = label
        )
      )
    })
}

transPathDataToJSON <- function (incidenceData, pal) {

  bins <- c(0, 5, 10, 25, 50, 100, 250, 1000, 10000)
  palette_colors <- pal(length(bins) - 1)

  features <- createGeoFeatures(incidenceData, palette_colors, bins)

  geoJSONstring <- paste0(
    '{"type": "FeatureCollection", "features": [',
    paste0(
      sapply(features, function(feature) {
        paste0(
          '{"type": "', feature$type, '", ',
          '"geometry": {"type": "', feature$geometry$type, '", ',
          '"coordinates": [', paste(feature$geometry$coordinates, collapse = ","), ']}, ',
          '"properties": {"time": "', feature$properties$time, '", ',
          '"color": "', feature$properties$color, '", ',
          '"label": "', feature$properties$label, '"}}'
        )
      }), collapse = ","
    ),
    ']}'
  )
}

output$transmission <- renderLeaflet({
  req(!is.null(input$selectedCountry))
  req(iv_dataupload$is_valid())

  level1Names <- NULL


  if(input$cropLev1 == TRUE){
    if(!is.null(input$level1List) && !("" %in% input$level1List)){
      level1Names <- input$level1List
    }
  }
  
    inputISO <- countrycode(input$selectedCountry, origin = 'country.name', destination = 'iso3c') #Converts country name to ISO Alpha
    gadmFileName <- paste0("gadm36_", toupper(inputISO), "_1_sp.rds")   # name of the .rds file 
    gadmFolder <- "gadm/"
    level1Identifier <- readRDS(paste0(gadmFolder, gadmFileName))

    if(!is.null(level1Names)){
    level1Identifier <- level1Identifier[which(level1Identifier$NAME_1 %in% level1Names), ]}
  
    valueRange <- c(0, 5, 10, 25, 50, 100, 250, 1000, 10000)

      ramp <- c(
            '#617AEC', 
            '#0027E0', 
            '#0C81F8', 
            '#43CAFF', 
            '#7BEBC8', 
            '#DFF58D', 
            '#FFA044', 
            '#EE4F4D')
  pal <- colorRampPalette(ramp)

  incidenceData <- transPathData()

  geoJson <- transPathDataToJSON(transPathData(), pal)

  leaflet(
    options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%
    addPolygons(data = level1Identifier,
                color = "#444444", 
                weight = 1.5, 
                smoothFactor = 1,
                opacity = 1.0, 
                fillColor = "#F5F5F5",
                fillOpacity = 0.75,
                popup = paste(level1Identifier$NAME_1),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = FALSE)) %>%
    addLegend(pal = colorBin(palette = pal(9)[-1],
                             bins = valueRange,
                             domain = valueRange),
              values = valueRange,
              opacity = 0.75,
              title = "Obs. persons",
              position = "topright") %>%
    htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        dataAsJson = JSON.parse(data);
        addTimeDimensionToLeaflet(map, dataAsJson);
      }
    ", data = geoJson)
})

  #--------------------------------------------------------------------------#
  # Proxy map for the leaflet plot to dynamically update the transmission
  # path data
  #--------------------------------------------------------------------------#
  observe({
    req(!is.null(input$transPathDate))

    # which date (column of data) to plot
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

  output$timeSeriesOptions <- renderUI({
    plotTitle <- paste0("Time-Series Graph of Incidence/Death in ")
    if(input$selectedCountry %in% prependList) {
      plotTitle <- paste0(plotTitle, "the ")
    }
    plotTitle <- paste0(plotTitle, input$selectedCountry)

    plotOptionsMenuUI(
      id = "timeSeriesMenu",
      plotType = "Time-Series",
      title = plotTitle,
      xlab = "Date",
      ylab = "Number of Persons",
      colour = "#22031F",
      includeFlip = FALSE,
      includeGridlines = FALSE
    )
  })

  output$timeSeries <- renderPlotly({
    req(iv_dataupload$is_valid())
    req(!is.null(input[["timeSeriesMenu-Colour"]]))

    p <- plotTimeSeries(file = input$incidenceData$datapath,
                        input[["timeSeriesMenu-Title"]],
                        input[["timeSeriesMenu-Xlab"]],
                        input[["timeSeriesMenu-Ylab"]],
                        input[["timeSeriesMenu-Colour"]],
                        input[["timeSeriesMenu-TSstyle"]])
    ggplotly(p)
  })


  #---------------------------------------------------------------------------#
  # Combine the lat/long data with the observed infection into a single table #
  #---------------------------------------------------------------------------#
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

  #--------------------------------------------------------------------------#
  # Checks to see that files have been uploaded (helper func)                #
  #--------------------------------------------------------------------------#
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

    updateCheckboxInput(
      inputId = "cropLev1",
      value = FALSE
    )
    updatePickerInput(
      inputId = "selectedCountry",
      selected = ""
    )

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

  #==========================================================================#
  # Model Simulation Components                                           ----
  #==========================================================================#

  #--------------------------------------------------------------------------#
  # Create a country plot cropped by level1Identifier and output to UI       #
  #--------------------------------------------------------------------------#
  # observeEvent(input$go, {
  #   if(input$cropLev1 == TRUE){
  #     output$croppedOutputImage <- renderImage({
  #
  #       outfile <- tempfile(fileext = '.png')
  #
  #       png(outfile, width = 800, height = 600)
  #       createCroppedRaster(selectedCountry = input$selectedCountry, level1Region = input$level1List, rasterAgg = input$agg, directOutput = T)
  #       dev.off()
  #
  #       list(src = outfile, contentType = 'image/png', width = 600, height = 400, alt = "Base plot image not found")
  #     }, deleteFile = TRUE)
  #   }
  # })

  #--------------------------------------------------------------------------#
  # Output population base plot image to the app UI                          #
  #--------------------------------------------------------------------------#
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
                               directOutput = TRUE))  # print the susceptible plot direct to UI
      }

      dev.off()

      list(src = outfile, contentType = 'image/png', width = 768, height = 768, alt = "Base plot image not found")
      # The above line adjusts the dimensions of the base plot rendered in UI
    }, deleteFile = TRUE)
  })

  #--------------------------------------------------------------------------#
  # Output IDE equations image to the app UI                                 #
  #--------------------------------------------------------------------------#
  observeEvent(input$go, {
    req(iv$is_valid())
    output$modelImg <- renderImage({
      return(list(src= "www/ModelEquations.png",
                  height = 400,
                  contentType = "image/png"))
    }, deleteFile = FALSE)
  })

  #--------------------------------------------------------------------------#
  # Output flowchart image to the app UI                                     #
  #--------------------------------------------------------------------------#
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

  #--------------------------------------------------------------------------#
  # Reset all parameter sliders, country selection, etc.                     #
  #--------------------------------------------------------------------------#
  observeEvent(input$resetAll, {
    shinyjs::reset("dashboard")
    shinyjs::disable(id = "go")

    values$allow_simulation_run <- FALSE

  })

  #--------------------------------------------------------------------------#
  # Checks to see that a new file has been uploaded (helper func)            #
  #--------------------------------------------------------------------------#

coordinatePairsInsideCountry <- function(coordinates, countryISO3C, regionNames = NULL, debug = FALSE) {

  points <-  st_as_sf(coordinates, coords = c("lon", "lat"), crs = 4326)

  country_filename <- file.path("gadm", paste0("gadm36_", countryISO3C, "_1_sp.rds"))

  if (file.exists(country_filename)) {
    print(paste("Loading country data from:", country_filename))
    admin_boundaries <- readRDS(country_filename)
  } else {
    print(paste("Country data not found locally. Downloading", countryISO3C, "using geodata::gadm()"))
    admin_boundaries <- gadm(country = countryISO3C, level = 1, resolution = 1, version = "latest", path = tempdir())

    if (is.null(admin_boundaries)) {
      stop("Country not found. Please check the ISO3C code.")
    }
    saveRDS(admin_boundaries, country_filename)
  }

  admin_boundaries <- st_as_sf(admin_boundaries)

  if (!is.null(regionNames)) {

    if (debug) {
      print("Administrative regions available:")
      print(admin_boundaries$NAME_1)
    }

    print("Checking specified regions")

    regions <- terra::subset(admin_boundaries, admin_boundaries$NAME_1 %in% regionNames, 
                     select = c("NAME_1", "geometry"))

    if (nrow(regions) == 0) {
      stop("No specified regions found in the country.")
    }

    areaPolygon <- regions %>% 
      st_union() %>%
      st_cast("MULTIPOLYGON")

  } else {

    print("No regions specified, using entire country boundaries")
    areaPolygon <- admin_boundaries %>% 
      st_union() %>%
      st_cast("MULTIPOLYGON")
    areaPolygon <- st_as_sf(areaPolygon)
  }

  areaPolygon <- st_transform(areaPolygon, st_crs(points))

  intersections <- st_intersects(points, areaPolygon)

  validPoints <- sapply(intersections, function(x) length(x) > 0)

  if (debug) {
    print(paste("Number of points:", nrow(points)))
    print("Area of interest bounding box:")
    print(st_bbox(areaPolygon))
    print("Points coordinates:")
    print(coordinates)
    print("Results:")
    print(validPoints)
  }

  return(validPoints)
}

validateAndCleanSeedData <- function(data) {
  # This is so that I can collect all the err msgs for later
  error_messages <- c()

  numeric_cols <- c("lat", "lon", "InitialVaccinated", "InitialExposed", 
                    "InitialInfections", "InitialRecovered", "InitialDead")

  missing_cols <- setdiff(c("Location", numeric_cols), colnames(data))
  if (length(missing_cols) > 0) {
    error_messages <- c(error_messages, 
                        paste0("Error: Missing columns: ", paste(missing_cols, collapse = ", ")))
  }

  if (any(is.na(data$Location) | data$Location == "")) {
    error_messages <- c(error_messages, 
                        "Error: 'Location' column cannot have empty cells or NA values.")
  }

  for (col in numeric_cols) {
    if (any(data[[col]] == "" | is.na(suppressWarnings(as.numeric(data[[col]]))))) { 
      error_messages <- c(error_messages, 
                          paste0("Error: '", col, "' column must contain only numbers and no empty cells."))
    } else {
      data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
    }
  }

  if (length(error_messages) > 0) {
    for (msg in error_messages) {
      showNotification(msg, type = "error", duration = NULL)
    }
    return(NULL)
  }

  coordinates <- data.frame(lat = data$lat, lon = data$lon)

  validCoords <- coordinatePairsInsideCountry(coordinates, selectedCountryISO3C(), input$level1List, debug = FALSE)
  
  if(all(!validCoords)) {
    showNotification("Error: ALL coordinates are not within the country or selected areas.", type = "error", duration = NULL)
    return(NULL)
  } else if (any(!validCoords)) {
    showNotification("SOME coordinates are not within the country or selected areas.", type = "warning", duration = NULL)
    return(NULL)
  }

  return (data)
}

observeEvent(input$seedData, {

  ext <- tools::file_ext(input$seedData$datapath)
  if (ext == "xlsx") {
    uploaded_data <- readxl::read_excel(input$seedData$datapath)
  } else {
    uploaded_data <- read.csv(input$seedData$datapath)
  }

  validated_data <- validateAndCleanSeedData(uploaded_data)

  if (!is.null(validated_data)) {
    # commenting this out has no effect, what is this for ? 
    # data(validated_data) 
    values$allow_simulation_run <- TRUE
    fileInputs$smStatus <- 'uploaded'
  } else {
    fileInputs$smStatus <- 'reset' 
  }
})
  

  #--------------------------------------------------------------------------#
  # Check if all mandatory fields have a value                               #
  #--------------------------------------------------------------------------#
  # observe({
  #       mandatoryFilled <-
  #       vapply(fieldsMandatory,
  #              function(x) {
  #                !is.null(input[[x]]) && input[[x]] != ""
  #              },
  #              logical(1))
  #
  #       mandatoryFilled <- all(mandatoryFilled)
  #
  #     # enable/disable the submit button
  #     if (isolate(values$allow_simulation_run) == TRUE){
  #       shinyjs::toggleState(id = "go", condition = mandatoryFilled)
  #   }
  # })

  #--------------------------------------------------------------------------#
  # highlight drop down item when hovering                                   #
  #--------------------------------------------------------------------------#
  # observe({
  #   hoverDrop <-
  #     vapply(hoverDrop,
  #            function(x) {
  #              !is.null(input[[x]]) && input[[x]] != ""
  #            },
  #            logical(1))
  #   hoverDrop <- all(hoverDrop)
  #   # enable/disable the submit button
  #   if (isolate(values$allow_simulation_run) == TRUE){
  #     shinyjs::toggleClass(class = hoverDrop)
  #   }
  # })

  #--------------------------------------------------------------------------#
  # This static ui field is in server since other dynamic ui elements need it#
  #--------------------------------------------------------------------------#
  output$countryDropdown <- renderUI({
    pickerInput(
      inputId = "selectedCountry",
      label = strong("Country"),
      choices = shortlist$Country,
      multiple = FALSE,
      selected = NA,
      options = pickerOptions(
        actionsBox = TRUE,
        title = "Please select a country")
    )
  })

  #--------------------------------------------------------------------------#
  # Checkbox for Data Assimilation                                           #
  #--------------------------------------------------------------------------#
  output$dataAssimCheckbox <- renderUI({
    validate(need(!is.null(input$selectedCountry), ""))

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      checkboxInput(inputId = "dataAssim", label = strong("Include Bayesian data assimilation?"), value = FALSE)
    }
  })

  #--------------------------------------------------------------------------#
  # Create select box for choosing input country                             #
  #--------------------------------------------------------------------------#
  output$Level1Ui <- renderUI({
    req(!is.null(input$selectedCountry) && input$selectedCountry != "")
    validate(need(input$cropLev1 == TRUE, "")) # catches UI warning

    upperISO3C <- toupper(selectedCountryISO3C())

    ## FIXME NOTE: What does 36 mean?
    if (file.exists(paste0("gadm/", "gadm36_", upperISO3C, "_1_sp.rds"))){
      level1Options <<- readRDS(paste0("gadm/", "gadm36_", upperISO3C, "_1_sp.rds"))$NAME_1
    } else {
      level1Options <<- raster::getData("GADM", download = TRUE, level = 1, country = upperISO3C)$NAME_1
    }

    selectizeInput(inputId = "level1List",
                   label = NULL,
                   choices = level1Options,
                   ## selected = c("Ituri", "Nord-Kivu"),
                   selected = NA,
                   multiple = TRUE,
                   options = list(placeholder = "Select state(s)/province(s)"))

  })

  level1Country <- reactiveVal({
    value = NULL
  })

  #--------------------------------------------------------------------------#
  # Radio button for SEIRD vs SVEIRD Model                                   #
  #--------------------------------------------------------------------------#
  output$modelRadio <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      radioButtons(inputId = "modelSelect",
                   label = strong("Epidemic Model"),
                   choiceValues = list("SEIRD","SVEIRD"),
                   choiceNames = list("SEIRD","SVEIRD"),
                   selected = "SVEIRD", #character(0), #
                   inline = TRUE,
                   width = "1000px")
    }
  })

  #--------------------------------------------------------------------------#
  # Radio button for Deterministic vs Stochastic Model                       #
  #--------------------------------------------------------------------------#
  output$stochasticRadio <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      radioButtons(inputId = "stochasticSelect",
                   label = strong("Model Stochasticity"),
                   choiceValues = list("Deterministic", "Stochastic"),
                   choiceNames = list("Deterministic", "Stochastic"),
                   selected = "Deterministic", #character(0), #
                   inline = TRUE,
                   width = "1000px")
    }
  })

  #--------------------------------------------------------------------------#
  # TODO: refactor numericInputs into single function                        #
  #--------------------------------------------------------------------------#
  output$alphaInput <- renderUI({
    alphaValue <- 0.00015 # 0.2100

    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){

      if(input$modelSelect == "SEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          alphaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"alpha"])
        } else if (input$selectedCountry == "Nigeria"){
          alphaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"alpha"])
        } else if (input$selectedCountry == "Uganda"){
          alphaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"alpha"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          alphaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"alpha"])}
      } else if (input$modelSelect == "SVEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          alphaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"alpha"])
        } else if (input$selectedCountry == "Nigeria"){
          alphaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"alpha"])
        }else if (input$selectedCountry == "Uganda"){
          alphaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"alpha"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          alphaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"alpha"])}
      }

      numericInput(inputId = "alpha",
                   label = HTML(paste("Daily Vaccination Rate (&#945)")),
                   value = alphaValue, min = 0, max = 1, step = 0.00001)
    }
  })

  #--------------------------------------------------------------------------#
  #                                                                          #
  #--------------------------------------------------------------------------#
  output$betaInput <- renderUI({
    req(!is.null(input$modelSelect))
    betaValue <- 0.055 # 0.00001

    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){

      if(input$modelSelect == "SEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          betaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"beta"])
        } else if (input$selectedCountry == "Nigeria"){
          betaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"beta"])}
        else if (input$selectedCountry == "Uganda"){
          betaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"beta"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          betaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"beta"])}
      } else if (input$modelSelect == "SVEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          betaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"beta"])
        } else if (input$selectedCountry == "Nigeria"){
          betaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"beta"])}
        else if (input$selectedCountry == "Uganda"){
          betaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"beta"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          betaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"beta"])}
      }

      numericInput(inputId = "beta",
                   label = HTML(paste("Daily Exposure Rate (&#946)")),
                   value = betaValue, min = 0, max = 1, step = 0.00001)
    }
  })

  #--------------------------------------------------------------------------#
  #                                                                          #
  #--------------------------------------------------------------------------#
  output$gammaInput <- renderUI({
    req(!is.null(input$modelSelect))
    gammaValue <- 0.009 #0.008

    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){

      if(input$modelSelect == "SEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          gammaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"gamma"])
        } else if (input$selectedCountry == "Nigeria"){
          gammaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"gamma"])}
        else if (input$selectedCountry == "Uganda"){
          gammaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"gamma"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          gammaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"gamma"])}
      } else if (input$modelSelect == "SVEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          gammaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"gamma"])
        } else if (input$selectedCountry == "Nigeria"){
          gammaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"gamma"])}
        else if (input$selectedCountry == "Uganda"){
          gammaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"gamma"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          gammaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"gamma"])}
      }

      numericInput(inputId = "gamma",
                   label = HTML(paste("Daily Infection Rate (&#947)")),
                   value = gammaValue, min = 0, max = 1, step = 0.00001)
    }
  })

  #--------------------------------------------------------------------------#
  #                                                                          #
  #--------------------------------------------------------------------------#
  output$sigmaInput <- renderUI({
    req(!is.null(input$modelSelect))
    sigmaValue <- 0.065

    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){

      if(input$modelSelect == "SEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"sigma"])
        } else if (input$selectedCountry == "Nigeria"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"sigma"])}
        else if (input$selectedCountry == "Uganda"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"sigma"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"sigma"])}
      } else if (input$modelSelect == "SVEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"sigma"])
        } else if (input$selectedCountry == "Nigeria"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"sigma"])}
        else if (input$selectedCountry == "Uganda"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"sigma"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"sigma"])}
      }

      numericInput(inputId = "sigma",
                   label = HTML(paste("Daily Recovery Rate (&#963)")),
                   value = sigmaValue, min = 0, max = 1, step = 0.00001)
    }
  })

  #--------------------------------------------------------------------------#
  #                                                                          #
  #--------------------------------------------------------------------------#
  output$deltaInput <- renderUI({
    req(!is.null(input$modelSelect))
    deltaValue <- 0.0015

    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){

      if(input$modelSelect == "SEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"delta"])
        } else if (input$selectedCountry == "Nigeria"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"delta"])}
        else if (input$selectedCountry == "Uganda"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"delta"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"delta"])}
      } else if (input$modelSelect == "SVEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"delta"])
        } else if (input$selectedCountry == "Nigeria"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"delta"])}
        else if (input$selectedCountry == "Uganda"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"delta"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          deltaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"delta"])}
      }

      numericInput(inputId = "delta",
                   label = HTML(paste("Daily Death Rate (&#948)")),
                   value = deltaValue, min = 0, max = 1, step = 0.00001)
    }
  })

  #--------------------------------------------------------------------------#
  #                                                                          #
  #--------------------------------------------------------------------------#

  #helpText('NOTE: Radius of 1 is called the Moore neighbourhood.'),
  #HTML("<p>NOTE: Radius of 1 is called the <a href='https://en.wikipedia.org/wiki/Moore_neighborhood'>Moore neighbourhood</a></p>", target = "_blank"),
  #p("NOTE:Radius of 1 is called the",a("Moore neighbourhood", href="https://en.wikipedia.org/wiki/Moore_neighborhood", target="_blank")),

  output$lambdaInput <- renderUI({
    req(!is.null(input$modelSelect))
    lambdaValue <- 15

    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){

      if(input$modelSelect == "SEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"lambda"])
        } else if (input$selectedCountry == "Nigeria"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"lambda"])}
        else if (input$selectedCountry == "Uganda"){
          lambdaValue <- 5}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SEIRD")[1,"lambda"])}
      } else if (input$modelSelect == "SVEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"lambda"])
        } else if (input$selectedCountry == "Nigeria"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"lambda"])
        }
        else if (input$selectedCountry == "Uganda"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"lambda"])}
        else if (input$selectedCountry == "Democratic Republic of Congo"){
          lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "COD" & model == "SVEIRD")[1,"lambda"])}
      }

      withMathJax(
        numericInput("lambda",
                     HTML(r"[Distance Parameter (&#955;; \(\frac{\Delta \overline{km}}{day}\))]"),
                     lambdaValue, 1, 50, 1)
      )
    }
  })

  #--------------------------------------------------------------------------#
  #                     Upload Seed Data                                     #
  #--------------------------------------------------------------------------#
  output$seedUpload <- renderUI({

    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){

      fileInput(inputId = "seedData",
                label = "Upload Seed Data",
                placeholder = "Upload seed data (.csv or .xls or .xlsx)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xls",
                  ".xlsx"),
      )
      #p("Click ", a("here", href="https://docs.google.com/spreadsheets/d/1aEfioSNVVDwwTt6ky7MrOQj5uGO7QQ1NTB2TdwOBhrM/edit?usp=sharing", target="_blank"), "for a template of initial seed data")
    }
  })

  #--------------------------------------------------------------------------#
  #                                                                          #
  #--------------------------------------------------------------------------#
  output$startDateInput <- renderUI({
    req(!is.null(input$modelSelect))
    startDateInput <- Sys.Date() # NULL

    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){

      if(input$modelSelect == "SEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          startDateInput <- "2020-09-01" #filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"startDate"]
        } else if (input$selectedCountry == "Nigeria"){
          startDateInput <- "2020-09-01" #filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"startDate"]
        }
      } else if (input$modelSelect == "SVEIRD"){
        if (input$selectedCountry == "Czech Republic"){
          startDateInput <- "2021-09-01" #filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"startDate"]
        } else if (input$selectedCountry == "Nigeria"){
          startDateInput <- "2021-09-01" #filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"startDate"]
        }
      }
      if (input$selectedCountry == "Uganda") {
        startDateInput <- "2022-10-20"
      }
      else if (input$selectedCountry == "Democratic Republic of Congo") {
        startDateInput <- "2018-08-01"}

      dateInput('date', "Choose simulation start date", value = startDateInput, max = Sys.Date(),
                format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                language = "en", width = NULL)
    }
  })

  #--------------------------------------------------------------------------#
  # numeric input for number of iterations                                   #
  #--------------------------------------------------------------------------#
  output$timestepInput <- renderUI({
    timestepValue <- 10
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (input$selectedCountry == "Czech Republic" || input$selectedCountry == "Nigeria"){timestepValue = 120}
    else if (input$selectedCountry == "Democratic Republic of Congo") {timestepValue = 440}
    else if (input$selectedCountry == "Uganda") {timestepValue = 63}

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "timestep",
                   label = "Number of Iterations (days)",
                   min = 1, max = 3650, value = timestepValue, step = 1)}
  }
  )

  #--------------------------------------------------------------------------#
  # Data Assimilation settings                                               #
  #--------------------------------------------------------------------------#

  output$dataAssimCmpts <- renderUI({
    validate(need(input$dataAssim == TRUE, "")) #catches UI Warning

    checkboxGroupInput(inputId = "selectedCompartments",
                       "Select observable compartment(s)",
                       choices = c("V", "E", "I", "R", "D"),
                       selected = c("I"),
                       inline = TRUE,
    )
  })
  showI <- reactive({
    "I" %in% input$selectedCompartments
  })

  showD <- reactive({
    "D" %in% input$selectedCompartments
  })

  output$dataAssimZones <- renderUI({
    validate(need(input$dataAssim == TRUE, "")) #catches UI Warning
    if (!is.null(input$selectedCountry) && input$selectedCountry != "") {
      fileInput(inputId = "dataAssimZones",
                label = ("Upload the lat/lon coordinates of reporting health zones (.csv or .xls or .xlsx)"),
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xls",
                  ".xlsx"),)
    }
  })

  observeEvent(input$dataAssimZones, {
    print(read.csv(input$dataAssimZones$datapath))
    print(as.character(input$dataAssimZones[1]))})

  output$dataAssimFileI <- renderUI({
    validate(need(input$dataAssim == TRUE, "")) #catches UI Warning
    if (showI()) {
      fileInput(inputId = "assimIData",
                label = ("Upload infection data to be assimilated with the model (.csv or .xls or .xlsx)"),
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xls",
                  ".xlsx"), )
    }
  })
  output$dataAssimFileD <- renderUI({
    validate(need(input$dataAssim == TRUE, "")) #catches UI Warning
    if (showD()) {
      fileInput(inputId = "assimDData",
                label = ("Upload death data to be assimilated with the model (.csv or .xls or .xlsx)"),
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xls",
                  ".xlsx"),
      )
    }
  })
  # output$dataAssimCmpts <- renderUI({
  #   validate(need(input$dataAssim == TRUE, "")) #catches UI Warning
  #
  #   selectizeInput(inputId = "level1List", "Select observable compartments",
  #                  choices = c("V", "E", "I", "R", "D"),
  #                  selected = "", multiple = TRUE,
  #                  options = list(placeholder = ""))
  #})

  #--------------------------------------------------------------------------#
  # Change the function which generates the Q matrix     #
  #--------------------------------------------------------------------------#
  output$varCovarFunc <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      selectInput(inputId = "covarianceSelect",
                  label = HTML("<span class='label-text'>Choose variance-covariance function</span>"),
                  choices = list("DBD", "Balgovind", "Exponential", "Gaussian", "Spherical"),
                  # HTML("<span class='option-text'>Distance-Based Decay</span>"),
                  # HTML("<span class='option-text'>Balgovind</span>"),
                  # HTML("<span class='option-text'>Exponential</span>"),
                  # HTML("<span class='option-text'>Gaussian</span>"),
                  # HTML("<span class='option-text'>Spherical</span>")
                  #),
                  selected = "DBD", #character(0), #
                  width = "1000px",
                  multiple = FALSE)
    }
  })

  #--------------------------------------------------------------------------#
  # Adjust parameter values for the variance=covariance function             #
  #--------------------------------------------------------------------------#

  output$selectRho <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "QCorrLength",
                   label = "Choose correlation length parameter for generating Q",
                   value = 0.675,
                   step = 0.001,
                   min = 0)
    }
  })

  output$selectSigma <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "QVar",
                   label = "Choose variance parameter for generating Q",
                   value = 0.55,
                   step = 0.01,
                   min = 0)
    }
  })

  output$selectNbhd <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "nbhd",
                   label = "Choose neighborhood parameter for generating Q",
                   value = 3,
                   step = 1,
                   min = 0)
    }
  })

  output$selectPsiDiag <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      helper(numericInput(inputId = "psidiag",
                          label = HTML("Choose a value to use for the zero elements of &#936;"),
                          value = 0.001,
                          step = 0.001,
                          min = 0),
             content = "psi")
    }
  })

  #--------------------------------------------------------------------------#
  # Change the recommended aggregation factor for slider dynamically         #
  #--------------------------------------------------------------------------#
  output$aggInput <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      sliderInput(inputId = "agg",
                  label = "Aggregation Factor",
                  min = 1, max = 100, step = 1, value = population$reco_rasterAgg[match(input$selectedCountry, population$Country)])
    }
  })

  #--------------------------------------------------------------------------#
  # Output bubble plot with initial seed data directly to the app UI         #
  #--------------------------------------------------------------------------#
  # observeEvent(input$go, {
  #   output$seededOutputImage <- renderImage({
  #     source("R/plotSeedData.R", encoding="UTF-8")
  #     outfile <- tempfile(fileext = '.png')
  #
  #     # print the seed plot direct to UI
  #     png(outfile, width = 1024, height = 768)
  #     plot(c(1,3,6,9,12), c(1.5,2,7,8,15), main = "Bubble Plot Placeholder") # TODO: example plot, below lines don't work due to "Regions defined for each Polygons" warning
  #     # createSeedPlot(countryName = "Czech Republic", seedData = "seeddata/CZE_InitialSeedData.csv", startDate = "2021-07-01", source = "testSource")
  #     dev.off()
  #
  #     list(src = outfile, contentType = 'image/png', width = 1024, height = 768, alt = "Seed image not found")
  #   }, deleteFile = TRUE)
  # })

  lineThickness <- 1.5

  observeEvent(input$go, {
    req(iv$is_valid())

    output$infectedExposedPlot <- renderPlotly({
      p <- makePlot(
        compartments = c("E", "I"),
        selectedCountry = input$selectedCountry,
        plotTitle = paste0("Time-series plot of Exposed and Infectious \n compartments in ", input$selectedCountry),
        xTitle = paste0("Day (from ", input$date, ")"),
        yTitle = "Compartment Value",
        lineThickness = lineThickness)
      ggplotly(p)
    })

    # output$cumDeathsPlot <- renderPlotly({
    #   p <- makePlot(
    #     compartments = c("D"),
    #     selectedCountry = input$selectedCountry,
    #     plotTitle = paste0("Estimated Cumulative Deaths \n in ", input$selectedCountry),
    #     xTitle = paste0("Day (from ", input$date, ")"),
    #     yTitle = "Cumulative Deaths",
    #     lineThickness = lineThickness)
    #
    #   countryISO <- countrycode(input$selectedCountry, origin = 'country.name', destination = 'iso3c') #Converts country name to ISO Alpha
    #   png(paste0("www/MP4/", countryISO, "_CumulativeDeaths.png"), width = 800, height = 600)
    #   print(p)
    #   dev.off()
    #
    #   ggplotly(p)
    # })

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

    output$fullPlot <- renderPlotly({
      if (input$modelSelect == "SVEIRD"){

        p <- makePlot(compartments = c("S", "V", "E", "I", "R", "D"),
                 selectedCountry = input$selectedCountry,
                 plotTitle = paste0("Time-series plot of epidemic compartments \n in ", input$selectedCountry),
                 xTitle = paste0("Day (from ", input$date, ")"),
                 yTitle = "Compartment Value",
                 lineThickness = lineThickness)

      } else {
        p <- makePlot(compartments = c("S", "E", "I", "R", "D"),
                 selectedCountry = input$selectedCountry,
                 plotTitle = paste0("Time-series plot of epidemic compartments \n in ", input$selectedCountry),
                 xTitle = paste0("Day (from ", input$date, ")"),
                 yTitle = "Compartment Value",
                 lineThickness = lineThickness)
      }

      ggplotly(p)
    })

    ## NOTE: fraction of Susceptible
    # output$fracSusPlot <- renderImage({
    #   outfile <- tempfile(fileext = '.png')
    #
    #   png(outfile, width = 1024, height = 768)
    #   df <- read.xlsx(paste0("www/MP4/", countrycode(input$selectedCountry, "country.name", "iso3c"), "_summary.xlsx"), sheetIndex = 1)
    #   plotData = data.frame(X = df[,"S"]/df[,"N"], Y = df[,"I"]/df[,"N"])
    #   p = ggplot(plotData, mapping = aes(X, Y, group = 1)) +
    #     geom_line(aes(X, Y), size=lineThickness, color="black") +
    #     labs(title = paste0(input$selectedCountry, " SI Phase Plane (", input$date, ", ", input$timestep, " timesteps)"),
    #          x = "Fraction susceptible", y = "Fraction Infected")
    #   plot(p)
    #   dev.off()
    #
    #   list(src = outfile, contentType = 'image/png', width = 1024, height = 768, alt = "Image not found")
    # }, deleteFile = TRUE)
  })

  #------------------------------------------------------------------------#
  # Allow the user to download the time-series plots from UI               #
  #------------------------------------------------------------------------#
  # observeEvent(input$go, {
  #   # TODO: implement downloading of files
  # })

  #--------------------------------------------------------------------------#
  # Generate seed data and have an option to download the file locally       #
  #--------------------------------------------------------------------------#

  output$seedDataButton <- renderUI({
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      downloadButton('downloadData', label = "Generate Seed Data Template",
                     style = "length:800px")
    }
  })

  output$seedRadiusInput <- renderUI({
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning


    if(!is.null(fileInputs$smStatus) && fileInputs$smStatus != 'reset') {
      radioButtons(inputId = "seedRadius",
                   label = strong("Insert infection data in"),
                   choiceNames = list("a single cell", "a Moore neighbourhood of cells"),
                   choiceValues = list(0, 1),
                   selected = 0, #character(0), #
                   inline = TRUE)
    }
  })

  observeEvent(input$selectedCountry, {
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning

    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){

      inputISO <- countrycode(input$selectedCountry, origin = 'country.name', destination = 'iso3c') # Converts country name to ISO Alpha

      gadmFileName <- paste0("gadm36_", inputISO, "_1_sp.rds")  # name of the .rds file

      gadmFolder <- "gadm/" # .rds files should be stored in local gadm/ folder

      # if (file.exists(paste0(gadmFolder, gadmFileName)))
      # {
      Level1Identifier <- readRDS(paste0(gadmFolder, gadmFileName))
      # }
      # else
      # {
      #   Level1Identifier <- getData("GADM", level = 1, country = inputISOLower)
      # }
      #print(coordinates(Level1Identifier)) # coords of the region
      #print(Level1Identifier$NAME_1) # List of all states/provinces/regions

      seedNames <- Level1Identifier$NAME_1
      seedCoords <- coordinates(Level1Identifier)
      #print(seedCoords)
      seedVaxx <- c(0)
      seedExpo <- c(0)
      seedInfect <- c(0)
      seedRec <- c(0)
      seedDead <- c(0)
      seedCombine <- cbind(seedNames, seedCoords, seedVaxx, seedExpo, seedInfect, seedRec, seedDead)
      frameCombine <- data.frame(seedCombine)

      frameCombine <- frameCombine[c("seedNames", "V3", "V2", "seedVaxx", "seedExpo", "seedInfect", "seedRec", "seedDead")]

      colnames(frameCombine) <- c("Location", "lat", "lon", "InitialVaccinated", "InitialExposed", "InitialInfections", "InitialRecovered", "InitialDead")
      #print(frameCombine)

      isoCode <- countrycode(input$selectedCountry, origin = "country.name", destination = "iso3c")
      sheetName <- sprintf("%s_initialSeedData", isoCode)

      output$downloadData <- downloadHandler(
        filename = function() {
          paste(sheetName, Sys.Date(), ".csv",  sep = "")
        },
        content = function(sheetName) {
          write.csv(frameCombine, sheetName, row.names = FALSE)
        }
      )

      # if(input$selectedCountry != "Democratic Republic of Congo") {
      #   updateCheckboxInput(
      #     inputId = "cropLev1",
      #     value = FALSE
      #   )
      # }
    }

    fileInputs$smStatus <- 'reset'
  })

  #--------------------------------------------------------------------------#
  # Multiple functionality when 'Run Simulation' is pressed                  #
  #--------------------------------------------------------------------------#
  observeEvent(input$go, {
    req(iv$is_valid())
    # show_modal_spinner(spin = "cube-grid",
    #                    color = "#18536F",
    #                    text = p("   Calculating...   ",
    #                             br(),
    #                             "This may take several minutes."))
    isCropped <- input$cropLev1

    # if(input$cropLev1 == TRUE)
    # {
    #   isCropped <- TRUE
    # }
    # else
    # {
    #   isCropped <- FALSE
    # }

    print(paste0(c("isCropped", isCropped)))

    rs <- createRasterStack(selectedCountry = input$selectedCountry,
                            rasterAgg = input$agg,
                            isCropped = isCropped,
                            level1Names = input$level1List,
                            susceptible = susceptible())

    # ============= TAB TO SHOW SEED DATA IN TABLE ===========
    data <- reactive({               # read seed data from .csv or .xlsx
      req(iv$is_valid())
      req(input$seedData)
      ext <- tools::file_ext(input$seedData$datapath)
      seedData <- input$seedData
      if(ext == 'xlsx'){
        readxl::read_excel(input$seedData$datapath)
      } else {
        read.csv(input$seedData$datapath)
      }
    })

    ## seed data table ----
    output$tableSeed <- renderDT({ # print initial seed data to UI
      req(input$seedData)
      if(is.null(data())){
        return ()
      }
      datatable(data(),
                rownames = FALSE,
                options = list(
                  dom = 't',
                  pageLength = -1,
                  ordering = FALSE,
                  searching = FALSE,
                  paging = FALSE,
                  autoWidth = TRUE,
                  scrollX = TRUE),)
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

      output$downloadOutputSummary <- downloadHandler(
        function() paste0(input$selectedCountry, "_Simulation_Summary", Sys.Date(), ".xlsx"),
        function(ouputFile) file.copy(file, ouputFile)
      )

      file.exists(file) |>
      need(message = "Waiting for simulation summary XLSX.") |>
      shiny::validate()

      read_xlsx(file) |>
        datatable(options = list(autoWidth = FALSE, scrollX = TRUE)) |>
        formatRound(columns = 2:15, digits = 0)

    })

    output$dataPlot <- renderPlot({
      buildPlot()
    })

    # # Allow user to download the raster plot
    # output$downloadPlot <- downloadHandler(
    #     filename = function() {
    #         "susceptibleDataPlot.pdf"
    #     },
    #
    #     content = function(file) {
    #         pdf(file = file, width = 12, height = 12)
    #         print(buildPlot())
    #         dev.off()
    #     }
    # )

    # #Allow user to download the simulation summary data, simply save as csv
    # output$downloadData <- downloadHandler(
    #     filename = function() {
    #         "simulationSummary.csv"
    #     },
    #
    #     content = function(file) {
    #         write.table(x = cDatTable(),
    #                     file = file,
    #                     quote = FALSE, sep = ",", row.names = FALSE)
    #     }
    #  )

    # validate(need(!is.null(data()),'No csv uploaded.'))
    #
    # if(nrow(data())>1)
    # {
    #     return('Your csv has enough rows!')
    # }
    # else
    # {
    #     return('Your csv has not enough rows!')
    # }

    #---------------------------------------#
    # Compartmental model simulation begins #
    #---------------------------------------#

    #print(data())          # Prints the seed data

    #print(names(data()))   # Prints the column names of the seed data

    alpha <- ifelse(input$modelSelect == "SVEIRD", input$alpha, 0) # DO NOT DELETE
    beta  <- input$beta  # DO NOT DELETE
    gamma <- input$gamma # DO NOT DELETE
    sigma <- input$sigma # DO NOT DELETE
    delta <- input$delta # ifelse(input$modelSelect == "SEIR", 0, input$delta) # DO NOT DELETE

    eps <- 0.0000000000000001

    radius <- ifelse(input$lambda <= input$agg, 1, round(((input$lambda - input$agg)/input$agg) + eps) + 1)

    isDeterministic <- TRUE

    if(input$stochasticSelect == "Deterministic")
    {
      isDeterministic <- TRUE
    }
    else
    {
      isDeterministic <- FALSE
    }

    ### model function call ----
    SpatialCompartmentalModelWithDA(model = input$modelSelect,
                                    stack = rs,
                                    startDate = input$date,
                                    selectedCountry = input$selectedCountry,
                                    directOutput = FALSE,
                                    rasterAgg = input$agg,
                                    alpha, beta, gamma, sigma, delta,
                                    radius = radius,
                                    lambda = input$lambda,
                                    timestep = input$timestep,
                                    seedFile = input$seedData$datapath,
                                    seedRadius = as.numeric(input$seedRadius),
                                    deterministic = isDeterministic,
                                    isCropped = input$cropLev1,
                                    level1Names = input$level1List,
                                    DA = input$dataAssim,
                                    sitRepData = input$dataAssimZones$datapath,
                                    dataI = input$assimIData$datapath,
                                    dataD = input$assimDData$datapath,
                                    varCovarFunc = input$covarianceSelect,
                                    QVar = input$QVar,
                                    QCorrLength = input$QCorrLength,
                                    nbhd = input$nbhd,
                                    psiDiag = sidiag)

    row1  <- data.frame(Variable = "Country", Value = input$selectedCountry)
    row2  <- data.frame(Variable = "WorldPop Raster Dimension", Value = paste0(rs$nRows, " rows x ", rs$nCols, " columns = ", rs$nCells, " grid cells"))
    row3  <- data.frame(Variable = "Aggregation Factor", Value = input$agg)
    row4  <- data.frame(Variable = "Aggregated Raster Dimension", Value = paste0(nrow(rs$rasterStack), " rows x ", ncol(rs$rasterStack), " columns = ", ncell(rs$rasterStack), " grid cells"))
    row5  <- data.frame(Variable = "Compartmental Model", Value = input$modelSelect)
    row6  <- data.frame(Variable = "Model Parameters", Value = paste("Alpha:", alpha,"Beta:", beta,"Gamma:", gamma, "Sigma:", sigma,"Delta:", delta))
    row7  <- data.frame(Variable = "Average Distance Travelled/Day (in km)", Value = input$lambda)
    row8  <- data.frame(Variable = "Radius (1 = Moore neighbourhood)", Value = radius)
    row9  <- data.frame(Variable = "Uploaded Seed Data", Value = input$seedData$name)
    row10 <- data.frame(Variable = "Number of iterations (days)", Value = input$timestep)

    values$df <- rbind(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10)

    ## raster summary table ----
    output$summaryTable <- renderDT({
      datatable(values$df,
                rownames = FALSE,
                options = list(
                  dom = 't',
                  pageLength = -1,
                  ordering = FALSE,
                  searching = FALSE,
                  paging = FALSE,
                  autoWidth = FALSE))
    })

    #---------------------------------------#
    # Output seed plot image to the app UI  #
    #---------------------------------------#

    ## seed data leaflet plot output ----
    output$seedPlot <- renderLeaflet({
      req(iv$is_valid())
      # outfile <- tempfile(fileext = '.png')
      #
      # png(outfile, width = 1024, height = 768)
      # createCroppedSeedPlot(selectedCountry = input$selectedCountry,
      #                       isCropped,
      #                       level1Names = input$level1List,
      #                       susceptibleLayer = susceptible()$Aggregated,
      #                       seedData = input$seedData$datapath,
      #                       seedRadius = as.numeric(input$seedRadius))  # print the seed plot direct to UI
      # dev.off()
      #
      # list(src = outfile, contentType = 'image/png', width = 1024, height = 768, alt = "Seed plot image not found")
      # The above line adjusts the dimensions of the base plot rendered in UI
      seedData <- read.csv(input$seedData$datapath, header = T)
      printCroppedBubbleSeedPlot(input$selectedCountry, input$seedData$datapath, level1Names = input$level1List, 6)
    })

    output$outputVideo <- renderUI({
      tags$video(
        id = "video",
        type = "video/mp4",
        src = "MP4/Infected_MP4.mp4",  # TODO: dynamically change which mp4 is printed
        controls = "controls"
      )
    })
  })

  observeEvent(input$level1List, {
    level1Country(input$selectedCountry)
  })
  # observeEvent(input$filterLMIC,{
  #   updateCheckboxInput(session, inputId = "cropLev1", value = FALSE) # uncheck the crop box first
  #   if(input$filterLMIC){
  #     population <- population[population$LMIC == 'TRUE',]
  #   } else {
  #     population <- population #[population$LMIC == 'TRUE' || population$LMIC == 'FALSE']
  #   }
  #   updatePickerInput(session, inputId = 'selectedCountry', choices = population$Country, selected = "Nigeria")
  # })

  #--------------#
  # Tabset Panel #
  #--------------#

  observeEvent(input$resetAll,{
    shinyjs::hide(id = "tabsetContainer")
    fileInputs$smStatus <- 'reset'
  })

  observeEvent(!iv$is_valid(),{
    shinyjs::hide(id = "tabsetContainer")
  })

  observeEvent(input$go,{
    shinyjs::show(id = "tabsetContainer")
    updateTabsetPanel(inputId = 'tabSet', selected = 'Input Summary')
    shinyjs::runjs("window.scrollTo(0, 0)")
  })

  # output$downloadOutputSummary <- downloadHandler(
  #   filename = function() {"output.csv"},
  #   content = function(file){
  #     write.csv(data(), file, row.names = FALSE)
  #   }
  #
  # )

  observeEvent(is.null(input$seedData), {
    shinyjs::hide(id = "seedRadius")
  })

  observeEvent(input$seedData, {

    if(iv_seeddataupload$is_valid()) {
      shinyjs::hide(id = "downloadData")
      shinyjs::show(id = "seedRadius")
    } else {
      shinyjs::hide(id = "seedRadius")
      shinyjs::show(id = "downloadData")
    }
  })

}
