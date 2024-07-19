#------------------------- #
#      UI Components
#------------------------- #
developer <- function(lastName, firstName, degree = "", affiliationIndex = 1, href = "") {
  tag("address",
      list(a(style = "font-weight: bold;",
             href = href,
             target = "_blank",
             lastName,
             ",",
             firstName,
             degree,
             tag("sup", affiliationIndex))))
}

ui <- fluidPage( # UI ----
  tags$head(
    tags$style(HTML("
      .label-text {
        font-size: 16px; /* Adjust label font size as needed */
      }

      .option-text {
        font-size: 12px; /* Adjust option font size as needed */
      }
    "))
  ),
  theme = bs_theme(version = 4,
                   primary = "#18536F"),
  tags$head(
    tags$link(rel = "stylesheet",
              type="text/css",
              href="spatialEpisimBanner.css")
  ),

  div(
    class = "navTitle",

    span(
      class = "pageTitle",
      tags$em("spatialEpisim"),
      ": Spatial Tracking of Infectious Diseases using Mathematical Models",
      br()
    )
  ),

  add_busy_spinner(spin = "cube-grid",
                   color = "#18536F",
                   margins = c("50%","50%")),

  navbarPage(title = "",

             tabPanel(title = "Model",
                      sidebarLayout(
                        sidebarPanel( ## sidebar panel ----
                          shinyjs::useShinyjs(),
                          shinyjs::inlineCSS(appCSS),
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

                            # -------------------------------------------- #
                            # Visualizer Inputs                            #
                            # -------------------------------------------- #
                            conditionalPanel(
                              condition = "input.appMode == 'Visualizer'",

                              conditionalPanel(condition = "input.selectedCountry == 'Democratic Republic of Congo'",
                                               fileInput(inputId = "latLonData",
                                                           label = strong("Upload Lat-Lon Data:"),
                                                           placeholder = "Upload Lat-Lon data (.csv or .xls or .xlsx)",
                                                           accept = c(
                                                             "text/csv",
                                                             "text/comma-separated-values,text/plain",
                                                             ".csv",
                                                             ".xls",
                                                             ".xlsx")),
                                                 fileInput(inputId = "incidenceData",
                                                           label = strong("Upload Incidence/Death Data:"),
                                                           placeholder = "Upload Incidence/Death data (.csv or .xls or .xlsx)",
                                                           accept = c(
                                                             "text/csv",
                                                             "text/comma-separated-values,text/plain",
                                                             ".csv",
                                                             ".xls",
                                                             ".xlsx"))),
                              uiOutput("transPathDateInput"),
                              br(),
                              uiOutput("resetButton")
                            ),

                            # -------------------------------------------- #
                            # Simulation Inputs                            #
                            # -------------------------------------------- #
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

                              # actionButton("go","Run Simulation",
                              #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                              # actionButton("resetAll","Reset Values",
                              #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),

                              br(),
                              br(),

                              uiOutput("dataAssimCheckbox"),

                              # --------------------------------------------- #
                              # Bayesian Assimilation Inputs                  #
                              # --------------------------------------------- #
                              conditionalPanel(
                                condition = "input.dataAssim == '1'",
                                uiOutput("dataAssimCmpts"),
                                uiOutput("dataAssimZones"),
                                uiOutput("dataAssimFileI"),
                                uiOutput("dataAssimFileD"),

                                h5("Model error covariance matrix (Q) formulation", style="font-weight: bold; font-size:11.5pt"),
                                uiOutput("varCovarFunc"),
                                uiOutput("selectRho"),
                                uiOutput("selectSigma"),
                                uiOutput("selectNbhd"),

                                h5(HTML("Model error covariance matrix (&#936;) formulation"), style="font-weight: bold; font-size:11.5pt"),
                                uiOutput("selectPsiDiag"),

                                # actionButton("goDA","Run Simulation with DA",
                                #               style ="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                              ),
                              # br(),
                              # actionButton("resetAllDA","Reset Values",
                              #               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                              actionButton(inputId = "go",
                                           label = "Run Simulation",
                                           class = "act-btn"),
                              actionButton(inputId = "resetAll",
                                           label = "Reset Values",
                                           class = "act-btn"),
                            )), # div "dashboard"
                        ), # sidebarPanel

                        mainPanel( ## main panel ----

                          conditionalPanel(
                            condition = "input.appMode == 'Visualizer'",

                            div(id = "maptabPanels",
                                tabsetPanel(id = 'vizTabSet',
                                            tabPanel(id = "main",
                                                     title ="Leaflet Plot",

                                                     br(),
                                                     leafletOutput("leafletMap",
                                                                   width = 1024,
                                                                   height = 768),
                                                     br(),
                                                     br()
                                                     #downloadButton('downloadPlot', 'Save Image')
                                            ),
                                            tabPanel(title = "Leaflet Cropped Plot",
                                                     value = "Leaflet Cropped Plot",

                                                     br(),
                                                     leafletOutput("croppedLeafletMap",
                                                                   width = 1024,
                                                                   height = 768),
                                                     br(),
                                                     br()
                                            ),
                                            tabPanel(title = "terra Plot",

                                                     br(),
                                                     imageOutput("terraOutputImage"),
                                                     br(),
                                                     br()
                                            ),
                                            tabPanel(title = "Transmission Path",

                                                     br(),
                                                     leafletOutput("transmission",
                                                                   width = 1024,
                                                                   height = 768),
                                                     br(),
                                                     br()
                                            ),
                                            tabPanel(title = "Lollipop Chart",

                                                     br(),
                                                     plotlyOutput("lollipop",
                                                                  height = 1000),
                                                     br(),
                                                     br()
                                            ),
                                            tabPanel(title = "Time-Series Graph",

                                                     br(),
                                                     uiOutput("timeSeriesOptions"),
                                                     br(),
                                                     plotlyOutput("timeSeries",
                                                                  height = 800),
                                                     br(),
                                                     br())
                                            # tabPanel(title ="Population Count by State/Province",
                                            #          DT::dataTableOutput("aggTable")
                                            #          )
                                ) # tabSet
                            ), # maptabPanels
                          ),

                          conditionalPanel(
                            condition = "input.appMode == 'Simulator'",

                            div(id = "tabsetContainer",
                                tabsetPanel(id = "tabSet", selected = "Input Summary",

                                            tabPanel(title = "Input Summary",
                                                     verbatimTextOutput("summary"),
                                                     br(),
                                                     DTOutput("summaryTable"),
                                                     br(),
                                                     br(),
                                                     imageOutput("outputImage",
                                                                 height = "768px",
                                                                 width = "768px"),
                                                     #imageOutput("croppedOutputImage"),
                                                     #imageOutput("seededOutputImage"),
                                                     #downloadButton(outputId = "downloadSummary", label = "Save Input Summary as a PDF File")
                                            ),

                                            tabPanel(title = "Model", id = "modelTab",
                                                     h3("Schematic Diagram"),
                                                     br(),
                                                     imageOutput("flowchartImg",
                                                                 height = "400px"),
                                                     br(),
                                                     br(),
                                                     h3("Mathematical Model"),
                                                     br(),
                                                     imageOutput("modelImg",
                                                                 height = "400px")
                                            ),

                                            # tabPanel(title = "Schematic Diagram", id = "flowchartTab",
                                            #
                                            # ),

                                            tabPanel(title = "Initial Seed Data",
                                                     DTOutput("tableSeed"),
                                                     br(),
                                                     leafletOutput("seedPlot",
                                                                   width = 1024,
                                                                   height = 768)
                                            ),

                                            tabPanel(title = "MP4 Animation",
                                                     br(),
                                                     br(),
                                                     uiOutput("outputVideo")#,
                                                     #downloadButton(outputId = "downloadMP4", label = "Save MP4 Animation")
                                            ),

                                            tabPanel(title = "Output Summary",
                                                     DTOutput("outputSummary") ,
                                                     downloadButton(outputId = "downloadOutputSummary", label = "Download as Excel")
                                            ),

                                            tabPanel(title = "Plot", id = "plotTab",
                                                     plotlyOutput("infectedExposedPlot", width = 800, height = 600),
                                                     plotlyOutput("cumulativeDeaths", width = 800, height = 600),
                                                     plotlyOutput("dailyIncidence", width = 800, height = 600),
                                                     plotlyOutput("cumulativeIncidence", width = 800, height = 600),
                                                     plotlyOutput("fullPlot", width = 800, height = 600),
                                                     # imageOutput("fracSusPlot"),
                                                     #downloadButton(outputId = "downloadPlot", label = "Save Image")
                                            )
                                ) # tabsetPanel
                            )
                          )
                        ), # mainPanel
                      ) # sidebarLayout
             ), # Model tabPanel
             
             ModelAuthorshipTab <- nav_panel(
               title = "Authors",
               h2("Supervisor", style = "font-weight:bold"),
               ## TODO: Reformat Ashok's information so the digital links aren't ugly.
               tag("address",
                   list(
                     p(a("Ashok Krishnamurthy, Ph.D.", href = "https://bit.ly/2YKrXjX", target = "_blank", style = "font-weight: bold;"), br(),
                       "Mount Royal University", br(),
                       "Department of Mathematics & Computing,", br(),
                       "Calgary, AB, Canada", br(),
                       a("akrishnamurthy@mtroyal.ca", href = "mailto:akrishnamurthy@mtroyal.ca"), br(),
                       a("Episim GitHub", href = "https://github.com/ashokkrish/episim", target = "_blank")),
                     style = r"(a[href^='mailto']::before {content: '📧 ';} a[href^='tel']::before {content: '📞 ';})")),
               
               #TODO: Need the link for research assistants who have not had
               h2("Research Assistants", style = "font-weight:bold"),
               developer("Myer", "Michael", href = "https://github.com/m-myer"),
               developer("Carson", "Bryce", "B.Sc", href = "https://github.com/bryce-carson/"),
               developer("Le", "Khanh", href = "https://github.com/kle6951/"),
               developer("Wondwossen", "Tobias", href = "https://github.com/Toby-exe"),
               developer("Wai", "Crystal", href = "https://github.com/cwai097"),
               developer("Dhaliwal", "Gursimran", href = "https://github.com/dhaliwalgurs"),
               developer("Pulfer", "Timothy", href = ""), 
               developer("Darby", "Ryan", href = "https://github.com/Ryan-Darby"),
               developer("Szeto", "Jason", href = "https://github.com/jason-szeto"),
               developer("White", "Tom Bayliss", href = "https://github.com/tombaylisswhite"),
               developer("Doody", "Jake", href = ""),
               
               h2("Collaborators", style = "font-weight:bold"),
               
               h2("Acknowledgment", style = "font-weight:bold"),
               list(p("Dr. Loren Cobb, Dr. Bedrich Sousedik", br(), 
                      )),
               
               h3("Affiliations", style = "font-weight:bold"),
               p(tag("sup", 1), "Mount Royal University", br(),
                 "4825 Mount Royal Gate SW", br(),
                 "Calgary, Alberta, Canada", br(),
                 "T3E 6K6"),
             ),

             tabPanel("Authors",
                      h3("Research Team", style= "font-weight:bold"),
                      br(),
                      p(span("Ashok Krishnamurthy, PhD", style= "font-weight:bold")),
                      p("Project PI,"),
                      p("Associate Professor, Department of Mathematics and Computing,"),
                      p("Faculty of Science and Technology,"),
                      p("Mount Royal University,"),
                      p("Calgary, AB, CANADA"),

                      br(),

                      p("Email:",a("akrishnamurthy@mtroyal.ca", href="mailto:akrishnamurthy@mtroyal.ca")),
                      p("Website:", a(href="https://bit.ly/2YKrXjX","https://bit.ly/2YKrXjX", target="_blank")),
                      p("GitHub:", a(href="https://github.com/ashokkrish/spatialEpisim","https://github.com/ashokkrish/spatialEpisim", target="_blank")),

                      br(),

                      p(span("Michael Myer, Tobias Wondwossen, Khanh Le, Bryce Carson, Crystal Wai, Gursimran Dhaliwal, Timothy Pulfer, Ryan Darby, and Jason Szeto", style= "font-weight:bold" )),
                      p("Undergraduate Student, Mount Royal University, Calgary, AB, CANADA"),

                      br(),

                      p(span("Tom Bayliss White", style= "font-weight:bold" )),
                      p("Undergraduate Student, University of Exeter, Exeter, Devon, UK"),
                      p("Mitacs Globalink Research Intern (2023)"),

                      br(),

                      p(span("Jake Doody", style= "font-weight:bold" )),
                      p("Undergraduate Student, University of Maryland Baltimore County, MD, USA"),

                      br(),

                      p(span("Acknowledgement", style= "font-weight:bold" )),
                      p("Dr. Loren Cobb, Dr. Bedrich Sousedik"),

                      br(),

                      p("This interactive R Shiny app is maintained by Dr. Ashok Krishnamurthy. We welcome questions, insights, and feedback."),

                      br(),

                      h3("Disclaimer"),
                      p("This tool uses a mathematical model to simulate a variety of COVID-19/Ebola/Measles outcomes based on user-defined parameters.
                           The output of the model depends on model assumptions, parameter choices, and human mobility patterns.
                           It is not a medical predictor, and should be used for informational and research purposes only.
                           Please carefully consider the parameters you choose. Interpret and use the simulated results responsibly.
                           Authors are not liable for any direct or indirect consequences of this usage.")
             ) # Authors tabPanel
  ) # navbarPage
)