titleHTML <-
  paste("<em>spatialEpisim</em>",
        "Spatial Tracking of Infectious Diseases using Mathematical Models",
        sep = ": ")

authors <- tabPanel("Authors", includeMarkdown(here("include", "authors.md")))

sidebar <- sidebarPanel(
             useShinyjs(),
             div(
               id = "dashboard",
               uiOutput("countryDropdown"),
               uiOutput("cropStateCheckbox"),
               conditionalPanel("input.selectedCountry != ''",
                                helper(checkboxInput(
                                  inputId = "cropLev1",
                                  label = strong("Limit simulation to state(s)/province(s) (crop to selection)"),
                                  value = FALSE),
                                  content = "cropSelectedCountry"),
                                conditionalPanel(
                                  condition = "input.cropLev1",
                                  uiOutput("Level1Ui")),

                                radioButtons(inputId = "appMode",
                                             label = strong("Application mode"),
                                             choices = list("Visualizer",
                                                            "Simulator"),
                                             selected = "Simulator",
                                             inline = TRUE)),

               conditionalPanel(
                 condition = "input.appMode == 'Visualizer'",

                 uiOutput("transPathFileInputs"),
                 uiOutput("transPathDateInput"),
                 uiOutput("resetButton")
               ),

               conditionalPanel(
                 condition = "input.appMode == 'Simulator'",

                 helper(uiOutput("aggInput"),
                        content = "rasterAggregationFactor"),
                 uiOutput("modelRadio"),
                 uiOutput("stochasticRadio"),

                 conditionalPanel(
                   condition = "input.selectedCountry != ''",

                   withMathJax(),

                   wellPanel(h5("Model Parameters", style="font-weight: bold; font-size:11.5pt"),

                             conditionalPanel(id = "SVEIRD",
                                              withMathJax(),
                                              condition = "input.modelSelect == 'SVEIRD'",

                                              uiOutput("alphaInput")),

                             uiOutput("betaInput"),
                             uiOutput("gammaInput"),
                             uiOutput("sigmaInput"),
                             uiOutput("deltaInput"),
                             helper(uiOutput("lambdaInput"), content = "lambda")),

                   wellPanel(helper(uiOutput("seedUpload"),
                                    content = "seedData"),
                             uiOutput("seedDataButton"),
                             uiOutput("seedRadiusInput")),

                   uiOutput("startDateInput"),
                   uiOutput("timestepInput")),



                 uiOutput("dataAssimCheckbox"),

                 conditionalPanel(
                   condition = "input.dataAssim == '1'",
                   fileInput(inputId = "dataAssimZones",
                             label = "Upload the lat/lon coordinates of reporting health zones (.csv or .xls or .xlsx)",
                             accept = c(
                               "text/csv",
                               "text/comma-separated-values",
                               "text/plain",
                               ".csv",
                               ".xls",
                               ".xlsx")),
                   checkboxGroupButtons(
                     inputId = "selectedCompartments",
                     label = "Observed compartment(s)",
                     choiceNames = c("Vaccinated",
                                     "Exposed",
                                     "Infected",
                                     "Recovered",
                                     "Dead"),
                     choiceValues = c("V", "E", "I", "R", "D"),
                     selected = c("I"),
                     checkIcon = withTags(
                       list(
                         yes = i(class = "fa fa-check-square",
                                 style = "color: steelblue"),
                         no = i(class = "fa fa-square-o",
                                style = "color: steelblue")
                       )
                     )
                   ),

                   ## TODO: replace the usage of these inputs with the new
                   ## uploader:
                   ## - uiOutput("dataAssimFileI"),
                   ## - uiOutput("dataAssimFileD"),
                   ## TODO: permit use of the new compartments that were
                   ## previously unimplemented: VER.
                   uiOutput("dataAssimilationCompartmentDataUploaders"),

                   h5("Model error covariance matrix (Q) formulation",
                      style="font-weight: bold; font-size:11.5pt"),
                   uiOutput("varCovarFunc"),
                   uiOutput("selectRho"),
                   uiOutput("selectSigma"),
                   uiOutput("selectNbhd"),

                   h5(HTML("Model error covariance matrix (&#936;) formulation"),
                      style="font-weight: bold; font-size:11.5pt"),
                   uiOutput("selectPsiDiag")),
                 actionButton(inputId = "go",
                              label = "Run Simulation",
                              class = "act-btn"),
                 actionButton(inputId = "resetAll",
                              label = "Reset Values",
                              class = "act-btn"))))

mainPanel <- mainPanel(conditionalPanel(
               condition = "input.appMode == 'Visualizer'",

               div(id = "maptabPanels",
                   tabsetPanel(id = 'vizTabSet',
                               tabPanel(id = "main",
                                        title ="Leaflet Plot",

                                        leafletOutput("leafletMap",
                                                      width = 1024,
                                                      height = 768)),
                               tabPanel(title = "Leaflet Cropped Plot",
                                        value = "Leaflet Cropped Plot",

                                        leafletOutput("croppedLeafletMap",
                                                      width = 1024,
                                                      height = 768)),
                               tabPanel(title = "terra Plot",

                                        imageOutput("terraOutputImage")),
                               tabPanel(title = "Transmission Path",

                                        leafletOutput("transmission",
                                                      width = 1024,
                                                      height = 768)),
                               tabPanel(title = "Lollipop Chart",

                                        plotlyOutput("lollipop",
                                                     height = 1000)),
                               tabPanel(title = "Time-Series Graph",

                                        uiOutput("timeSeriesOptions"),
                                        plotlyOutput("timeSeries",
                                                     height = 800))))),

             conditionalPanel(
               condition = "input.appMode == 'Simulator'",
               div(id = "tabsetContainer",
                   tabsetPanel(id = "tabSet", selected = "Input Summary",
                               tabPanel(title = "Input Summary",
                                        verbatimTextOutput("summary"),
                                        DTOutput("summaryTable"),
                                        imageOutput("outputImage",
                                                    height = "768px",
                                                    width = "768px")),

                               tabPanel(title = "Model", id = "modelTab",
                                        h3("Schematic Diagram"),
                                        imageOutput("flowchartImg",
                                                    height = "400px"),
                                        h3("Mathematical Model"),
                                        imageOutput("modelImg",
                                                    height = "400px")),


                               tabPanel(title = "Initial Seed Data",
                                        DTOutput("tableSeed"),
                                        leafletOutput("seedPlot",
                                                      width = 1024,
                                                      height = 768)),

                               tabPanel(title = "MP4 Animation",
                                        uiOutput("outputVideo")),

                               tabPanel(title = "Output Summary",
                                        DTOutput("outputSummary")),

                               tabPanel(title = "Plot", id = "plotTab",
                                        plotlyOutput("infectedExposedPlot", width = 800, height = 600),
                                        plotlyOutput("cumulativeDeaths", width = 800, height = 600),
                                        plotlyOutput("dailyIncidence", width = 800, height = 600),
                                        plotlyOutput("cumulativeIncidence", width = 800, height = 600),
                                        plotlyOutput("fullPlot", width = 800, height = 600))))))

model <- tabPanel(title = "Model", sidebarLayout(sidebar, mainPanel))

ui <- fluidPage(withTags(head(link(rel = "stylesheet",
                                   type="text/css",
                                   href="spatialEpisimBanner.css"))),
                navbarPage(title = HTML(titleHTML),
                           model, authors),
                add_busy_spinner("cube-grid", "#18536F",
                                 margins = c("50%","50%")),
                title = "spatialEpisim",
                theme = bs_theme(version = 4, primary = "#18536F"))
