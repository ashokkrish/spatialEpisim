lightTheme <- bs_theme(
  version = 4,
  bg = "#ffffff",
  fg = "#000000",
  primary = "#003352",
  secondary = "#007fb5",
  base_font = font_google("Open Sans"),
  info = "#003352",
  "navbar-bg" = "#003352", # Navbar background color
  "navbar-color" = "#ffffff", # Navbar text color
  "navbar-border-color" = "#003352", # Navbar border color
  "navbar-padding-y" = "1.5rem" # Increase navbar padding (adjust height)
)

## NOTE: override the default arguments.
wellPanel <- \(...) div(..., class = "well", style = "margin-bottom: 0.75rem;")
plotlyOutput <- \(...) plotly::plotlyOutput(..., width = 800, height = 600)
leafletOutput <- \(...) leaflet::leafletOutput(..., width = 1024, height = 768)

titleHTML <-
  paste0("<em>spatialEpisim:</em>",
         "Spatial Tracking of Infectious Diseases using Mathematical Models")

authors <- tabPanel("Authors", includeMarkdown(here("markdown", "authors.md")))

simulatorUI <-
  div(
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
      selected = c("S", "V", "E", "I", "R", "D"),
      checkIcon =
        withTags(list(yes = i(class = "fa fa-check-square"),
                      no = i(class = "fa fa-square-o")))),

    conditionalPanel(
      r"[input.enabledCompartments.includes('V')]",
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

    wellPanel(helper(fileInput(inputId = "uploadedSeedData",
                               label = "Upload Seed Data",
                               placeholder = "Initial compartment values",
                               accept = mimetypes),
                     content = "seedData"),
              radioButtons(inputId = "seedRadius",
                           label = strong("Seed (spread) initial infections and exposures across a Moore neighbourhood of cells?"),
                           choiceNames = list("No (terra::cellFromXY only)", "Yes (equitably spread)"),
                           choiceValues = list(0, 1),
                           selected = 0,
                           inline = FALSE)),

    wellPanel(id = "data-assimilation",
              h5("Bayesian data assimilation"),
              checkboxInput("enablebayes", label = "Enable data assimilation"),

              conditionalPanel("input.enablebayes == 1",

                               div(helper(fileInput(inputId = "healthZoneCoordinates",
                                                    label = "Reporting health zones coordinates",
                                                    accept = mimetypes),
                                          type = "inline",
                                          content = "Latitute-longitude coordinates of reporting health zones."),

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

                                   uiOutput("dataAssimilationCompartmentDataUploaders")),

                               div(
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
                                                     step = 0.001))
                               ))),

    wellPanel(
      h5("Determinism"),
      radioButtons(inputId = "stochasticSelect",
                   label = NULL,
                   choiceValues = list("Deterministic", "Stochastic"),
                   choiceNames = list("Deterministic", "Stochastic"),
                   selected = "Deterministic",
                   inline = TRUE,
                   width = "100%")),

    wellPanel(id = "actionButtons",
        actionButton(inputId = "go",
                     label = "Run Simulation",
                     class = "act-btn"),

        actionButton(inputId = "resetAll",
                     label = "Reset Values",
                     class = "act-btn"))
  )

visualizer <-
  conditionalPanel(
    condition = "input.appMode === 'Visualizer'",
    div(id = "maptabPanels",
        tabsetPanel(id = 'vizTabSet',
                    tabPanel(id = "main", title ="Leaflet Plot", leafletOutput("leafletMap")),

                    tabPanel(title = "Leaflet Cropped Plot",
                             value = "Leaflet Cropped Plot",
                             leafletOutput("croppedLeafletMap")),

                    tabPanel(title = "Terra Plot", imageOutput("terraOutputImage"))

                    ## FIXME TODO: this is broken until I have JavaScript to check the input validator.
                    ## conditionalPanel(condition = input.iv_dataupload$is_valid(),
                    ##                  ## TODO: integrate the new Leaflet.TimeDimension plot feature
                    ##                  ## which was pushed that was pushed to main.
                    ##                  ## tabPanel(title = "Transmission Path", leafletOutput("transmission")),

                    ##                  ## TODO
                    ##                  ## tabPanel(title = "Lollipop Chart", plotlyOutput("lollipop")),

                    ##                  ## tabPanel(title = "Time-Series Graph",
                    ##                  ##          uiOutput("timeSeriesOptions"),
                    ##                  ##          plotlyOutput("timeSeries"))
                    ##                  )
                    )))

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
                      dplyr::select(filter(shortlist, Shortlisted == TRUE),
                                    "English Name"),
                      options = pickerOptions(title = "Please select a country")),
          conditionalPanel(condition = "input.selectedCountry != ''",
                           helper(checkboxInput(
                             inputId = "cropLev1",
                             label = "Limit simulation to state(s)/province(s) (crop to selection)",
                             value = FALSE),
                             content = "cropSelectedCountry"),
                           conditionalPanel(
                             condition = "input.cropLev1 == 1",
                             selectizeInput("provinces", NULL, NA, NA, TRUE, TRUE,
                                            options = list(placeholder = "Select state(s)/province(s)")))),
          conditionalPanel(
            condition = "input.appMode === 'Simulator' && input.selectedCountry !== ''",
            helper(sliderInput(inputId = "agg",
                               label = "Aggregation Factor",
                               min = 1,
                               max = 100,
                               step = 1,
                               value = 50),
                   content = "rasterAggregationFactor"))),

        conditionalPanel(condition = "input.appMode === 'Visualizer'",
                         conditionalPanel(condition = r"--(input.selectedCountry !== null && input.selectedCountry.length > 0)--",
                                          tagList(fileInput(inputId = "stateObservationsLatitudeLongitude",
                                                            label = strong("Health Zone centroid coordinates"),
                                                            placeholder = "Upload Lat-Lon data",
                                                            accept = mimetypes),
                                                  fileInput(inputId = "incidenceData",
                                                            label = strong("Incidence & Deaths"),
                                                            placeholder = "Upload Incidence/Death data",
                                                            accept = mimetypes))),
                         uiOutput("transmissionPathDateInput"),
                         actionButton("resetVisualizer", "Reset Values")),

        conditionalPanel(
          condition = "input.appMode === 'Simulator' && input.selectedCountry !== ''",
          simulatorUI)))

simulator <-
  conditionalPanel(
    condition = "input.appMode == 'Simulator'",
    div(id = "simulator",
        tabsetPanel(id = "tabSet", selected = "Input Summary",
                    tabPanel(title = "Input Summary",
                             verbatimTextOutput("summary"),
                             DTOutput("summaryTable"),
                             imageOutput("inputSummaryPlotBelowDataTable")),

                    tabPanel(title = "Model", id = "modelTab",
                             h3("Schematic Diagram"),
                             conditionalPanel(condition = r"[input.enabledCompartments.includes('V')]",
                                              img(src = "SVEIRD.png",
                                                  height = 400,
                                                  contentType = "image/png")),
                             conditionalPanel(condition = r"[!input.enabledCompartments.includes('V')]",
                                              img(src = "SEIRD.png",
                                                  height = 400,
                                                  contentType = "image/png")),

                             h3("Mathematical Model"),
                             img(src = "ModelEquations.png",
                                 height = 400,
                                 contentType = "image/png")),

                    ## MAYBE TODO
                    ## tabPanel(title = "Initial Seed Data",
                    ##          DTOutput("tableSeed"),
                    ##          leafletOutput("seedPlot")),

                    ## TODO: waiting on №7 and №11 counterpart to №44; the
                    ## solution to №44 is being extended to this.
                    ## tabPanel(title = "MP4 Animation", uiOutput("outputVideo")),

                    tabPanel(title = "Output Summary", DTOutput("outputSummary")),

                    tabPanel(title = "Plot", id = "plotTab",
                             plotlyOutput("infectedExposedPlot"),
                             plotlyOutput("cumulativeDeaths"),
                             plotlyOutput("dailyIncidence"),
                             plotlyOutput("cumulativeIncidence"),
                             plotlyOutput("fullPlot")))))

model <- tabPanel(title = "Model", sidebarLayout(sidebar, mainPanel(visualizer, simulator)))

ui <- fluidPage(useShinyjs(),
                use_waitress(),
                navbarPage(HTML(titleHTML), model, authors),
                add_busy_spinner("cube-grid", "#18536F", margins = c("50%","50%")),
                title = "spatialEpisim",
                theme = lightTheme)
