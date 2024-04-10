#------------------------- #
#      UI Components
#------------------------- #
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
              href="SE-banner.css")
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
                            
                            # radioButtons(inputId = "modellingApproach",
                            #              label = strong("Modelling Approach"),
                            #              choiceValues = list("1", "2"),
                            #              choiceNames = list("Non-spatial Modelling", "Spatial Modelling"),
                            #              selected = "2",
                            #              inline = TRUE),
                            
                            # conditionalPanel( #### Non-spatial Modeling ----
                            #                   condition = "input.modellingApproach == '1'",
                            #                   
                            #                   pickerInput(
                            #                     inputId = "nonspatialmodelSelect",
                            #                     label = strong(("Epidemic Model")),
                            #                     choices = list("SIR", "SIRD", "SEIR", "SEIRD", "SIR-Stochastic"),
                            #                     multiple = FALSE,
                            #                     selected = "SIR", #NULL,
                            #                     options = pickerOptions(
                            #                       actionsBox = TRUE,
                            #                       title = "Please select a model")
                            #                   ),
                                              
                                              # radioButtons(inputId = "qValue",
                                              #              label = strong("Model Formulation"),
                                              #              choiceValues = list("1", "2"),
                                              #              choiceNames = list("True-Mass Action", "Pseudo-Mass Action"),
                                              #              selected = "2",
                                              #              inline = TRUE,
                                              #              width = "1000px"),
                                              # 
                                              # radioButtons(inputId = "nonspatialstochasticSelect",
                                              #              label = strong("Model Stochasticity"),
                                              #              choiceValues = list("Deterministic", "Stochastic"),
                                              #              choiceNames = list("Deterministic", "Stochastic"),
                                              #              selected = "Deterministic",
                                              #              inline = TRUE,
                                              #              width = "1000px"),
                                              
                                              # checkboxInput(
                                              #   inputId = "muValue",
                                              #   label = strong("Include Vital Dynamics"),
                                              #   value = FALSE,
                                              # ),
                                              # 
                                              # withMathJax(),
                                              # conditionalPanel(
                                              #   condition = "input.muValue == '1'",
                                              #         numericInput(
                                              #           inputId = "muBirth",
                                              #           label = "Birth Rate (\\( \\mu_B\\))",
                                              #           min = 0,
                                              #           #max = 1,
                                              #           step = 0.0001,
                                              #           value = 0.00,
                                              #         ),
                                              #   
                                              #         numericInput(
                                              #           inputId = "muDeath",
                                              #           label = "Death Rate due to Natural Causes (\\( \\mu_D\\))",
                                              #           min = 0,
                                              #           #max = 1,
                                              #           step = 0.0001,
                                              #           value = 0.00,
                                              #         )
                                              # ),
                                              
                                              # conditionalPanel(
                                              #   id = "SI_versus_SEI",
                                              
                                              # conditionalPanel(
                                              #   withMathJax(),
                                              #   condition = "input.nonspatialmodelSelect == 'SIR'", 
                                              
                                              # withMathJax(),
                                              # h5("Model Parameters:", style="font-weight: bold; font-size:11.5pt"),
                                              # 
                                              # numericInput(
                                              #   inputId = "betaSIR",
                                              #   label = "Transmission Rate (\\( \\beta\\))",
                                              #   min = 0,
                                              #   #max = 1,
                                              #   step = 0.00001,
                                              #   value = 0.001,
                                              # ),
                                              # 
                                              # numericInput(
                                              #   inputId = "gammaSIR",
                                              #   label = "Removal Rate  (\\( \\gamma\\))",
                                              #   min = 0,
                                              #   #max = 5,
                                              #   step = 0.00001,
                                              #   value = 0.1,
                                              # ),
                                              # 
                                              # h5("Model Inputs:", style="font-weight: bold; font-size:11.5pt"),
                                              
                                              # numericInput(
                                              #   inputId = "populationSIR",
                                              #   label = "Total Population (N)",
                                              #   value = 500,
                                              #   min = 1,
                                              #   #max = maxPopulation,
                                              #   step = 1,
                                              # ),
                                              # numericInput(
                                              #   inputId = "susceptibleSIR",
                                              #   label = "Susceptible (S)",
                                              #   value = 499,
                                              #   min = 1,
                                              #   #max = maxPopulation,
                                              #   step = 1,
                                              # ),
                                              # numericInput(
                                              #   inputId = "infectedSIR",
                                              #   label = "Infected (I)",
                                              #   value = 1,
                                              #   min = 1,
                                              #   #max = maxPopulation,
                                              #   step = 1,
                                              # ),
                                              # numericInput(
                                              #   inputId = "recoveredSIR",
                                              #   label = "Recovered (R)",
                                              #   value = 0,
                                              #   min = 0,
                                              #   #max = maxPopulation,
                                              #   step = 1,
                                              # ),
                                              #),
                                              #),
                                              
                                              # actionButton("nonspatialgo","Run Simulation", 
                                              #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                              # actionButton("nonspatialresetAll","Reset Values", 
                                              #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                              
                            # ), #"input.modellingApproach == '1'"
                            
                            # conditionalPanel( #### Spatial Modeling ----
                            #                   condition = "input.modellingApproach == '2'",
                                              
                            uiOutput("countryDropdown"),
                                            
                            # checkboxInput(
                            #   inputId = "filterLMIC", 
                            #   label = strong("Show LMIC only"), 
                            #   value = FALSE),
                                              
                            uiOutput("cropStateCheckbox"),
                                            
                            conditionalPanel(
                              condition = "input.cropLev1",  
                                                
                              uiOutput("Level1Ui")),
                            
                            radioButtons(inputId = "appMode",
                                         label = strong("Mode"),
                                         choices = list("Visualizer", 
                                                        "Simulator"),
                                         selected = "Simulator",
                                         inline = TRUE),
                            
                            # -------------------------------------------- #
                            # Visualizer Inputs                            #
                            # -------------------------------------------- #
                            conditionalPanel(
                              condition = "input.appMode == 'Visualizer'",
                              
                              uiOutput("transPathFileInputs"),
                              uiOutput("transPathDateInput"),
                              br(),
                              uiOutput("resetButton")
                            ),
                            
                            # -------------------------------------------- #
                            # Simulation Inputs                            #
                            # -------------------------------------------- #
                            conditionalPanel(
                              condition = "input.appMode == 'Simulator'",
                              
                              uiOutput("aggInput"),
                              uiOutput("modelRadio"),
                              uiOutput("stochasticRadio"),
                              
                              conditionalPanel(
                                condition = "input.selectedCountry != ''",
                                
                                withMathJax(),
                                
                                h5("Model Parameters:", style="font-weight: bold; font-size:11.5pt"),
                                
                                conditionalPanel(id = "SVEIRD",
                                  withMathJax(),
                                  condition = "input.modelSelect == 'SVEIRD'", 
                                                 
                                  uiOutput("alphaInput")),
                                
                                uiOutput("betaInput"),
                                uiOutput("gammaInput"),
                                uiOutput("sigmaInput"),
                                uiOutput("deltaInput"),
                                uiOutput("lambdaInput"),
                                br(),
                                uiOutput("seedUpload"),
                                uiOutput("seedDataButton"),
                                uiOutput("seedRadiusInput"),
                                br(),
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
                                
                                h5(HTML(paste0("Model error covariance matrix (", TeX("&#936"), ") formulation")), style="font-weight: bold; font-size:11.5pt"),
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
                            ),
                                              
                            
                            # ), # "input.modellingApproach == '2'",
                          ), # div "dashboard"
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
                                                     # downloadButton(outputId = "downloadOutputSummary", label = "Save Output Summary")
                                            ),
                                            
                                            tabPanel(title = "Plot", id = "plotTab",
                                                     plotlyOutput("infectedExposedPlot", width = 800, height = 600),
                                                     br(),
                                                     br(),
                                                     plotlyOutput("cumDeathsPlot", width = 800, height = 600),
                                                     br(),
                                                     br(),
                                                     plotlyOutput("cumIPlot", width = 800, height = 600),
                                                     br(),
                                                     br(),
                                                     plotlyOutput("fullPlot", width = 800, height = 600),
                                                     br(),
                                                     br()
                                                     # imageOutput("fracSusPlot"),
                                                     #downloadButton(outputId = "downloadPlot", label = "Save Image")
                                            )
                                ) # tabsetPanel
                            )
                          )       
                        ), # mainPanel
                      ) # sidebarLayout
             ), # Model tabPanel
             
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
                      
                      p(span("Michael Myer, Tobias Wondwossen, Khanh Le, Michael Walsh, Crystal Wai, Gursimran Dhaliwal, Timothy Pulfer, Ryan Darby, and Jason Szeto", style= "font-weight:bold" )),    
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