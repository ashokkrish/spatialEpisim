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

  ## FIXME: this is not necessary when things are done properly. I'll figure out
  ## paths later.
  temporaryDirectory <- tempdir()
  dir.create(temporaryDirectory, showWarnings = FALSE)

  observe_helpers(help_dir = "markdown", withMathJax = TRUE)

  ISO3C <- reactive(countrycode(sourcevar = input$selectedCountry, "country.name", "iso3c"))
  ###########################################################################
  ## The following objects are specific to the visualizer component of the ##
  ## application.                                                          ##
  ###########################################################################

  ##########################################################################
  ## "provinces" is a SpatVector for the currently selected country, with ##
  ## resolution of the first (top-level) administrative boundaries. The   ##
  ## names are in provinceNames, and if a subregion of the country is     ##
  ## desired then it is in subregionsSpatVector.                          ##
  ##########################################################################
  ##########################################################################
  ## The following objects and expressions are specific to the simulation ##
  ## component of the application.                                        ##
  ##########################################################################
  provinces <- reactive(getCountrySubregions.SpatVector(req(ISO3C()), folder = tempdir()))
  provinceNames <- reactive(provinces()$NAME_1)
  observe(updateSelectizeInput(session, "provinces", choices = provinceNames()))

  ## NOTE: this is not aggregated. The aggregated form is only available from getSVEIRD.SpatRaster().
  populationSpatRaster <- reactive({
    shiny::validate(need(ISO3C(), "A country must be selected."))
    susceptibleSpatRaster <- getCountryPopulation.SpatRaster(ISO3C())
  })
  observeEvent(input$resetAll, shinyjs::reset("dashboard"))

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

  observe({
    dplyr::filter(recommendations, Country == input$selectedCountry) %>%
      dplyr::select(reco_rasterAgg) %>%
      dplyr::slice_head() %>%
      updateSliderInput(inputId = "agg", value = .)
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
                                                     ## FIXME: Error in eval: object 'input' not found; I cannot access the input object here, but it is accessible immediately outside this?
                                                     input$agg)

    print(openDataFile(input$seedData))

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
        seedData = openDataFile(input$seedData),
        neighbourhood.order = input$seedRadius,
        input$lambda,
        layers = SVEIRD.SpatRaster,
        aggregationFactor = input$agg,
        startDate = input$date,
        countryCodeISO3C = ISO3C(),
        input$dataAssimilation_I, # incidence
        input$dataAssimilation_D, # death

        simulationIsDeterministic = input$stochasticSelect == "Deterministic",
        dataAssimilationEnabled = input$dataAssim,

        input$dataAssimZones,

        ## Special parameters
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
