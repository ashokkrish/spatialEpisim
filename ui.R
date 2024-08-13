## Override the default arguments
wellPanel <- \(...) div(..., class = "well", style = "margin-bottom: 0.75rem;")
plotlyOutput <- \(...) plotly::plotlyOutput(..., width = 800, height = 600)
leafletOutput <- \(...) leaflet::leafletOutput(..., width = 1024, height = 768)

titleHTML <-
  paste("<em>spatialEpisim</em>",
        "Spatial Tracking of Infectious Diseases using Mathematical Models",
        sep = ": ")

authors <- tabPanel("Authors", includeMarkdown(here("markdown", "authors.md")))

sidebar <-
  sidebarPanel(
    div(id = "dashboard",
        radioButtons(inputId = "appMode",
                     label = strong("Application mode"),
                     choices = list(`Population visualization` = "Visualizer",
                                    `Compartmental epidemic modelling` = "Simulator"),
                     selected = "Simulator",
                     inline = TRUE),

        wellPanel(
          id = "country-raster-selection",
          pickerInput("selectedCountry",
                      strong("Country"),
                      dplyr::select(filter(read_xlsx(here("data", "misc", "recommendedRasterAggregationFactors.xlsx")),
                                           shortList == "TRUE"),
                                    Country),
                      options = pickerOptions(title = "Please select a country")),
          conditionalPanel("input.selectedCountry != ''",
                           helper(checkboxInput(
                             inputId = "cropLev1",
                             label = strong("Limit simulation to state(s)/province(s) (crop to selection)"),
                             value = FALSE),
                             content = "cropSelectedCountry"),
                           conditionalPanel(
                             "input.cropLev1",
                             selectizeInput("level1List", NULL, NA, NA, TRUE, TRUE,
                                            options = list(placeholder = "Select state(s)/province(s)")))),
          conditionalPanel(
            "input.appMode == 'Simulator' && input.selectedCountry !== ''",
            helper(sliderInput(inputId = "agg",
                               label = "Aggregation Factor",
                               min = 1,
                               max = 100,
                               step = 1,
                               value = 50),
                   content = "rasterAggregationFactor"))),

        conditionalPanel("input.appMode === 'Visualizer'",
                         conditionalPanel(r"--(input.selectedCountry !== null && input.selectedCountry.length > 0)--",
                                          tagList(fileInput(inputId = "latLonData",
                                                            label = strong("Health Zone centroid coordinates"),
                                                            placeholder = "Upload Lat-Lon data",
                                                            accept = acceptedFileTypes),
                                                  fileInput(inputId = "incidenceData",
                                                            label = strong("Incidence & Deaths"),
                                                            placeholder = "Upload Incidence/Death data",
                                                            accept = acceptedFileTypes))),
                         uiOutput("transmissionPathDateInput"),
                         actionButton("resetVisualizer", "Reset Values")),

        conditionalPanel(
          "input.appMode === 'Simulator' && input.selectedCountry !== ''",

          checkboxGroupButtons(
            inputId = "enabledCompartments",
            label = h5("Enabled SEI-type compartment(s)"),
            choiceNames = c("Susceptible",
                            "Vaccinated",
                            "Exposed",
                            "Infected",
                            "Recovered",
                            "Dead"),
            choiceValues = c("S", "V", "E", "I", "R", "D"),
            ## TODO: enforce that these are always selected (i.e., disable them).
            selected = c("S", "E", "I"),
            checkIcon =
              withTags(list(yes = i(class = "fa fa-check-square"),
                            no = i(class = "fa fa-square-o")))),

          radioButtons(inputId = "stochasticSelect",
                       label = strong("Model Stochasticity"),
                       choiceValues = list("Deterministic", "Stochastic"),
                       choiceNames = list("Deterministic", "Stochastic"),
                       selected = "Deterministic",
                       inline = TRUE,
                       width = "1000px"),

          h5("Model Parameters"),
          conditionalPanel(
            r"--{input.modelSelect.includes('V')}--",
            id = "vaccination-enabled",
            numericInput(
              "alpha",
              HTML("Daily Vaccination Rate (&#945;)"),
              1.5e-4, 0, 1, 0.00001)),
          numericInput("beta",
                       HTML("Daily Exposure Rate (&#946;)"),
                       5.5e-2, 0, 1, 0.00001),
          numericInput("gamma",
                       HTML("Daily Infection Rate (&#947;)"),
                       9e-3, 0, 1, 0.00001),
          numericInput("sigma",
                       HTML("Daily Recovery Rate (&#963;)"),
                       5.6e-2, 0, 1, 0.00001),
          numericInput("delta",
                       HTML("Daily Death Rate (&#948;)"),
                       1.5e-3, 0, 1, 0.00001),
          ## LAMBDA λ
          sprintf(r"--[Distance Parameter (&#955; %s)]--",
                  r"--[\(\frac{\Delta \overline{km}}{day}\)]--") %>%
          HTML() %>%
          numericInput("lambda", ., 1.5e1, 1, 50, 1) %>%
          withMathJax() %>%
          helper(content = "lambda"),

          h5("Time and date options"),
          dateInput('date',
                    "Choose simulation start date",
                    value = "1970-01-01",
                    max = Sys.Date(),
                    format = "yyyy-mm-dd",
                    startview = "month",
                    weekstart = 0,
                    language = "en",
                    width = NULL),
          numericInput(inputId = "timestep",
                       label = "Number of Iterations (days)",
                       min = 1, max = 3650, value = 100, step = 1),

          wellPanel(helper(fileInput(inputId = "seedData",
                                     label = "Upload Seed Data",
                                     placeholder = "Upload seed data (.csv or .xls or .xlsx)",
                                     accept = acceptedFileTypes),
                           content = "seedData"),
                    downloadButton('downloadData',
                                   label = "Generate Seed Data Template",
                                   style = "length:800px"),
                    radioButtons(inputId = "seedRadius",
                                 label = strong("Insert infection data in"),
                                 choiceNames = list("a single cell", "a Moore neighbourhood of cells"),
                                 choiceValues = list(0, 1),
                                 selected = 0,
                                 inline = TRUE)),

          wellPanel(id = "data-assimilation",
                    ## FIXME: the material switch isn't interactive.
                    materialSwitch(
                      inputId = "data-assimilation",
                      label = h5("Bayesian data assimilation"),
                      value = FALSE,
                      status = "primary",
                      inline = TRUE),

                    ## FIXME: the condition panel isn't hiding its children when
                    ## the condition doesn't evaluate to true.
                    conditionalPanel(
                      "input.enableDataAssimilation == true",

                      helper(fileInput(inputId = "dataAssimZones",
                                       label = "Reporting health zones coordinates",
                                       accept = acceptedFileTypes),
                             type = "inline",
                             content = "Latitute-longitude coordinates of reporting health zones."),

                      h5(HTML("Model error covariance matrix (&#936;) formulation")),
                      selectInput("covarianceSelect",
                                  "Choose variance-covariance function",
                                  list(`Distance-Based Decay` = "DBD",
                                       Balgovind = "Balgovind",
                                       Exponential = "Exponential",
                                       Gaussian = "Guassian",
                                       Spherical = "Spherical"),
                                  "DBD",
                                  width = "1000px"),
                      numericInput("QCorrLength",
                                   "Choose correlation length parameter for generating Q",
                                   0.675, 0,
                                   step = 0.001),
                      numericInput("QVar",
                                   "Choose variance parameter for generating Q",
                                   0.55, 0,
                                   step = 0.01),
                      numericInput("nbhd",
                                   "Choose neighborhood parameter for generating Q",
                                   3, 0,
                                   step = 1),
                      helper(content = "psi",
                             numericInput("psidiag",
                                          HTML("Replacement value for elements of &#936; equal to zero"),
                                          0.001, 0,
                                          step = 0.001)),

                      checkboxGroupButtons(
                        inputId = "selectedCompartments",
                        label = h5("Observed compartment(s)"),
                        choiceNames = c("Vaccinated",
                                        "Exposed",
                                        "Infected",
                                        "Recovered",
                                        "Dead"),
                        choiceValues = c("V", "E", "I", "R", "D"),
                        selected = NA,
                        checkIcon =
                          withTags(list(yes = i(class = "fa fa-check-square"),
                                        no = i(class = "fa fa-square-o")))),

                      uiOutput("dataAssimilationCompartmentDataUploaders"))),

          div(id = "actionButtons",
              actionButton(inputId = "go",
                           label = "Run Simulation",
                           class = "act-btn"),

              actionButton(inputId = "resetAll",
                           label = "Reset Values",
                           class = "act-btn")))))
visualizer <-
  conditionalPanel(
    "input.appMode === 'Visualizer'",
    div(id = "maptabPanels",
        tabsetPanel(id = 'vizTabSet',
                    tabPanel(id = "main",
                             title ="Leaflet Plot",
                             leafletOutput("leafletMap")),

                    tabPanel(title = "Leaflet Cropped Plot",
                             value = "Leaflet Cropped Plot",
                             leafletOutput("croppedLeafletMap")),

                    tabPanel(title = "terra Plot",
                             imageOutput("terraOutputImage")),

                    ## FIXME TODO: this is broken until I have JavaScript to check the input validator.
                    conditionalPanel(input.iv_dataupload$is_valid(),
                                     tabPanel(title = "Transmission Path",
                                              leafletOutput("transmission")),

                                     tabPanel(title = "Lollipop Chart",
                                              plotlyOutput("lollipop")),

                                     tabPanel(title = "Time-Series Graph",
                                              uiOutput("timeSeriesOptions"),
                                              plotlyOutput("timeSeries"))))))

simulator <-
  conditionalPanel(
    "input.appMode == 'Simulator'",
    div(id = "tabsetContainer",
        tabsetPanel(id = "tabSet", selected = "Input Summary",
                    tabPanel(title = "Input Summary",
                             verbatimTextOutput("summary"),
                             DTOutput("summaryTable"),
                             imageOutput("inputSummaryPlotBelowDataTable")),

                    tabPanel(title = "Model", id = "modelTab",
                             h3("Schematic Diagram"),
                             conditionalPanel(r"[input.modelSelect === "SVEIRD"]",
                                              img(src = "SVEIRD.png",
                                                  height = 400,
                                                  contentType = "image/png")),
                             conditionalPanel(r"[input.modelSelect === "SEIRD"]",
                                              img(src = "SEIRD.png",
                                                  height = 400,
                                                  contentType = "image/png")),

                             h3("Mathematical Model"),
                             img(src = "ModelEquations.png",
                                 height = 400,
                                 contentType = "image/png")),

                    tabPanel(title = "Initial Seed Data",
                             DTOutput("tableSeed"),
                             leafletOutput("seedPlot")),

                    tabPanel(title = "MP4 Animation",
                             uiOutput("outputVideo")),

                    tabPanel(title = "Output Summary",
                             DTOutput("outputSummary")),

                    tabPanel(title = "Plot", id = "plotTab",
                             plotlyOutput("infectedExposedPlot"),
                             plotlyOutput("cumulativeDeaths"),
                             plotlyOutput("dailyIncidence"),
                             plotlyOutput("cumulativeIncidence"),
                             plotlyOutput("fullPlot")))))

model <- tabPanel(title = "Model", sidebarLayout(sidebar, mainPanel(visualizer, simulator)))

ui <- fluidPage(withTags(head(link(rel = "stylesheet",
                                   type="text/css",
                                   href="spatialEpisimBanner.css"))),
                useShinyjs(), # PROG: this is required for much of our functionality.
                navbarPage(HTML(titleHTML), model, authors),
                add_busy_spinner("cube-grid",
                                 "#18536F",
                                 margins = c("50%","50%")),
                title = "spatialEpisim",
                theme = bs_theme(version = 4, primary = "#18536F"))
