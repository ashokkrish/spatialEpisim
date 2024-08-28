server <- function(input, output, session) {
  ## TODO: when model customization is implemented, remove V, R, and D from the
  ## disabledChoices vector. NOTE: technically, vaccination can be enabled or
  ## disabled at the moment, but for now I'm leaving it disabled until the
  ## integration of spatialEpisim with its new foundation is completed. NOTE:
  ## this needs to be done before anything else, so it's the first thing we
  ## request the server do.
  updateCheckboxGroupButtons(inputId = "enabledCompartments",
                             disabledChoices = c("S", "E", "I", # forever disabled
                                                 "V", "R", "D")) # TODO: enable later
  updateCheckboxGroupButtons(inputId = "selectedCompartments", # TODO: rename to observed/reported
                             disabledChoices = c("S", "V", "R", "D"))

  observe_helpers(help_dir = "markdown", withMathJax = TRUE)

  ISO3C <- reactive(countrycode(sourcevar = input$selectedCountry, "country.name", "iso3c"))
  ## FIXME: this is not necessary when things are done properly. I'll figure out
  ## paths later.
  temporaryDirectory <- tempdir()
  dir.create(temporaryDirectory, showWarnings = FALSE)
  provinces <- reactive(getCountrySubregions.SpatVector(req(ISO3C()), folder = tempdir()))
  observe(updateSelectizeInput(session, "provinces", choices = provinces()$NAME_1))

  ## NOTE: this is not aggregated. The aggregated form is only available from
  ## getSVEIRD.SpatRaster().
  populationSpatRaster <- reactive({
    shiny::validate(need(ISO3C(), "A country must be selected."))
    susceptibleSpatRaster <- getCountryPopulation.SpatRaster(ISO3C())
  })
  observeEvent(input$resetAll, shinyjs::reset("dashboard")) ## TODO: review the usage of this.

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
                           accept = mimetypes)
               })))

  observe({
    factor <-
      dplyr::filter(shortlist, `English Name` == req(input$selectedCountry)) %>%
      dplyr::select(`Recommended Raster Aggregation Factor`)

    if (nrow(factor) > 1) {
      warning(sprintf("There is more than one recommended raster aggregation factor for %s.", input$selectedCountry))
      updateSliderInput(inputId = "agg", value = as.numeric(dplyr::slice_head(factor)))
    } else {
      updateSliderInput(inputId = "agg", value = as.numeric(factor))
    }
  })

  observe({
    simulationWaitress <- Waitress$new("nav", theme = "overlay", min = 0, max = input$timestep)
    ## TODO: validate all inputs before proceeding with the rest of the
    ## statements in the expression.
    ## PROG the validation here.

    provincesSpatVector <- terra::subset(provinces(), provinces()$NAME_1 == input$provinces)

    SVEIRD.SpatRaster <-
      spatialEpisim.foundation::getSVEIRD.SpatRaster(terra::subset(provinces(),
                                                                   provinces()$NAME_1 %in% input$provinces),
                                                     populationSpatRaster(),
                                                     input$agg)

    uploadedSeedData <- openDataFile(input$uploadedSeedData)

    modelArguments <-
      list(
        ## Non-spatial parameters
        if ("V" %in% input$enabledCompartments) input$alpha else 0,
        input$beta,
        input$gamma,
        input$sigma,
        input$delta,

        ## Temporal parameters
        n.days = input$timestep, ## Temporal parameter; TODO: rename "timestep"

        ## Model parameters
        seedData = uploadedSeedData,
        neighbourhood.order = as.numeric(input$seedRadius),
        input$lambda,
        layers = SVEIRD.SpatRaster,
        aggregationFactor = input$agg,
        startDate = input$date,
        countryCodeISO3C = ISO3C(),

        ## TODO: move these to the common area below...
        input$dataAssimilation_I, # incidence
        input$dataAssimilation_D, # death

        ## Model options
        simulationIsDeterministic = input$stochasticSelect == "Deterministic",
        dataAssimilationEnabled = input$enablebayes,

        ## BDA data, options, parameters, etc.
        healthZoneCoordinates = NULL,
        variableCovarianceFunction = input$covarianceSelect,
        forecastError.cov.sdBackground = input$QVar,
        forecastError.cor.length = input$QCorrLength,
        neighbourhood.Bayes = input$nbhd,
        psi.diagonal = input$psidiag,

        ## NOTE: these are callback functions which are used every iteration
        ## to "call back" to the UI to update the "waitress" on the progress
        ## the backend has made on the simulation.
        callback = list(during = list(fun = simulationWaitress$inc, args = 1),
                        after = list(fun = simulationWaitress$close))
      )
    names(modelArguments) <- names(formals(spatialEpisim.foundation::SVEIRD.BayesianDataAssimilation))
    if (!input$enablebayes) {
      modelArguments <- as.environment(modelArguments)
      rm(list = c("incidenceData",
                  "deathData",
                  "healthZoneCoordinates",
                  "variableCovarianceFunction",
                  "forecastError.cov.sdBackground",
                  "forecastError.cor.length",
                  "neighbourhood.Bayes",
                  "psi.diagonal"),
         pos = modelArguments)
      modelArguments <- as.list(modelArguments)
    } else {
      modelArguments$healthZoneCoordinates <- openDataFile(req(input$healthZoneCoordinates))
      if (!is.null(input$dataAssimilation_I))
        modelArguments$incidenceData <- openDataFile(req(input$dataAssimilation_I))
      if (!is.null(input$dataAssimilation_D))
        modelArguments$deathData <- openDataFile(req(input$dataAssimilation_D))
    }
    model <- do.call(spatialEpisim.foundation::SVEIRD.BayesianDataAssimilation, modelArguments)

    print(model)

    ## NOTE: when the user has run a simulation, scroll to the top of the page so
    ## that they can see the tab panel buttons and the output. TODO: include a
    ## scroll to top button in the UI when appropriate, and otherwise make the
    ## model configuration panel scrollable without scrolling the main output
    ## area (make them scroll separately).
    ## shinyjs::show(id = "tabsetContainer")
    ## updateTabsetPanel(inputId = 'tabSet', selected = 'Input Summary')
    ## runjs(r"[window.scrollTo(0, 0);]")
  }) |> bindEvent(input$go)

  ## TODO: validate the go button...
}
