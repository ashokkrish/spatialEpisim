options(conflicts.policy = list(warn = FALSE))
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(av))
shhh(library(bslib))
shhh(library(cptcity))
shhh(library(countrycode))
shhh(library(deSolve))
shhh(library(dplyr))
shhh(library(ggplot2))
shhh(library(latex2exp))
shhh(library(lattice))
shhh(library(latticeExtra))
shhh(library(maps))
shhh(library(markdown))
shhh(library(purrr))
options("rgdal_show_exportToProj4_warnings"="none")
# shhh(library(raster, warn.conflicts=FALSE)) # classes and functions for raster data
shhh(library(rasterVis))
shhh(library(readxl))
shhh(library(writexl))
shhh(library(rgdal, warn.conflicts=FALSE))
shhh(library(sf))     # classes and functions for vector data
shhh(library(shiny))
shhh(library(shinyalert))
shhh(library(shinyhelper))
shhh(library(shinyjs))
shhh(library(shinyvalidate))
shhh(library(shinyWidgets))
shhh(library(sp))
shhh(library(stringr))
shhh(library(terra, warn.conflicts=FALSE))  # suppressWarnings(suppressMessages(library(terra)))
shhh(library(tidyverse))
shhh(library(tinytex))

population <- read_excel("misc/population.xlsx", 1)
epiparms <- read_excel("misc/epiparms.xlsx", 1)
#print(epiparms)

fieldsMandatory <- c("selectedCountry", "seedData")

#hoverDrop <- "selectedCountry"

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

highlightDrop <- function(menu) {
  tagList(
    menu, 
    span(class = "dropDown")
  )
}

appCSS <- ".mandatory_star {color: red;}"
appCSS <- ".invisible {display:none;}"
appCSS <- ".dropDown:hover {color:ADD8E6;background-color: #000000}"

ui <- fluidPage(
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
  theme = bs_theme(version = 4, bootswatch = "minty"),
  #theme = bs_theme(bootswatch = "slate"),
  div(
    class = "invisible",
    titlePanel("Mathematical Modelling of Infectious Diseases")
    #titlePanel("Spatial Tracking of Infectious Diseases using Mathematical Models")
  ),
  
  navbarPage(title = span("Mathematical Modelling of Infectious Diseases", style = "color:#000000; font-weight:bold; font-size:15pt"),
             
             tabPanel(title = "Model",
                      sidebarLayout(
                        sidebarPanel(
                          shinyjs::useShinyjs(),
                          shinyjs::inlineCSS(appCSS),
                          div(
                            id = "dashboard",
                            
                            radioButtons(inputId = "modellingApproach",
                                         label = strong("Modelling Approach"),
                                         choiceValues = list("1", "2"),
                                         choiceNames = list("Non-spatial Modelling", "Spatial Modelling"),
                                         selected = "2",
                                         inline = TRUE),
                            
                            conditionalPanel( #### Non-spatial Modeling ----
                                              condition = "input.modellingApproach == '1'",
   
                                pickerInput(
                                  inputId = "nonspatialmodelSelect",
                                  label = strong(("Epidemic Model")),
                                  choices = list("SIR", "SIRD", "SEIR", "SEIRD", "SIR-Stochastic"),
                                  multiple = FALSE,
                                  selected = "SIR", #NULL,
                                  options = pickerOptions(
                                    actionsBox = TRUE,
                                    title = "Please select a model")
                                ),
                                
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
                                
                                withMathJax(),
                                h5("Model Parameters:", style="font-weight: bold; font-size:11.5pt"),
                                
                                    numericInput(
                                      inputId = "betaSIR",
                                      label = "Transmission Rate (\\( \\beta\\))",
                                      min = 0,
                                      #max = 1,
                                      step = 0.00001,
                                      value = 0.001,
                                    ),
                                
                                    numericInput(
                                      inputId = "gammaSIR",
                                      label = "Removal Rate  (\\( \\gamma\\))",
                                      min = 0,
                                      #max = 5,
                                      step = 0.00001,
                                      value = 0.1,
                                    ),
                                
                                h5("Model Inputs:", style="font-weight: bold; font-size:11.5pt"),
                                
                                    numericInput(
                                      inputId = "populationSIR",
                                      label = "Total Population (N)",
                                      value = 500,
                                      min = 1,
                                      #max = maxPopulation,
                                      step = 1,
                                    ),
                                    numericInput(
                                      inputId = "susceptibleSIR",
                                      label = "Susceptible (S)",
                                      value = 499,
                                      min = 1,
                                      #max = maxPopulation,
                                      step = 1,
                                    ),
                                    numericInput(
                                      inputId = "infectedSIR",
                                      label = "Infected (I)",
                                      value = 1,
                                      min = 1,
                                      #max = maxPopulation,
                                      step = 1,
                                    ),
                                    numericInput(
                                      inputId = "recoveredSIR",
                                      label = "Recovered (R)",
                                      value = 0,
                                      min = 0,
                                      #max = maxPopulation,
                                      step = 1,
                                    ),
                                  #),
                                #),
                                
                                actionButton("nonspatialgo","Run Simulation", 
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                actionButton("nonspatialresetAll","Reset Values", 
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                              
                            ), #"input.modellingApproach == '1'"
                            
                            conditionalPanel( #### Spatial Modeling ----
                                              condition = "input.modellingApproach == '2'",

                                uiOutput("countryDropdown"),
                                
                                checkboxInput(inputId = "filterLMIC", label = strong("Show LMIC only"), value = FALSE),
                                
                                uiOutput("clipStateCheckbox"),
                                
                                conditionalPanel(condition = "input.clipLev1 == '1'",  uiOutput("Level1Ui")),
                                
                                uiOutput("aggInput"),
                                
                                uiOutput("modelRadio"),
                                
                                uiOutput("stochasticRadio"),
                                
                                conditionalPanel(
                                  id = "SEIRD_SVEIRD",
                                  withMathJax(),
                                  
                                  h5("Model Parameters:", style="font-weight: bold; font-size:11.5pt"),
                                  
                                  conditionalPanel(
                                    id = "SVEIRD",
                                    withMathJax(),
                                    condition = "input.modelSelect == 'SVEIRD'", 
                                    
                                    uiOutput("alphaInput")
                                  ),
                                  
                                  condition = "input.modelSelect == 'SEIRD' || input.modelSelect == 'SVEIRD'", 
                                  
                                  uiOutput("betaInput"),
                                  
                                  uiOutput("gammaInput"),
                                  
                                  uiOutput("sigmaInput"),
                                  
                                  uiOutput("deltaInput"),
                                  
                                  uiOutput("lambdaInput"),
                                  
                                  uiOutput("seedUpload"),
                                  
                                  uiOutput("seedDataButton"),
                                  br(),
                                  
                                  uiOutput("startDateInput"),
                                  
                                  uiOutput("timestepInput")
                                  ),
                                
                                # actionButton("go","Run Simulation", 
                                #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                # actionButton("resetAll","Reset Values", 
                                #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                
                                br(),
                                br(),
                                
                                uiOutput("dataAssimCheckbox"),
                                
                                conditionalPanel(condition = "input.dataAssim == '1'",  
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
                                actionButton("go","Run Simulation", 
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                actionButton("resetAll","Reset Values", 
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                            ), # "input.modellingApproach == '2'",
                          ),
                        ), 
                        
                        mainPanel(
                          tabsetPanel(id = "tabSet", selected = "Input Summary",
                                      
                                      tabPanel(title = "Input Summary",
                                               verbatimTextOutput("summary"), 
                                               #tableOutput("table"),
                                               imageOutput("outputImage"),
                                               #imageOutput("croppedOutputImage"),
                                               #imageOutput("seededOutputImage"),
                                               #downloadButton(outputId = "downloadSummary", label = "Save Input Summary as a PDF File")
                                      ),
                                      
                                      tabPanel(title = "Mathematical Model", id = "modelTab",
                                               imageOutput("modelImg")),
                                      
                                      tabPanel(title = "Schematic Diagram", id = "flowchartTab",
                                               imageOutput("flowchartImg")),
                                      
                                      tabPanel(title = "Initial Seed Data", 
                                               dataTableOutput("tableSeed")),
                                      
                                      tabPanel(title = "Seed Data Map",
                                               imageOutput("seedPlot")
                                      ),
                                      
                                      tabPanel(title = "MP4 Animation",
                                               uiOutput("outputVideo")#,
                                               #downloadButton(outputId = "downloadMP4", label = "Save MP4 Animation")
                                      ),
                                      
                                      tabPanel(title = "Output Summary",
                                               dataTableOutput("outputSummary") ,
                                               # downloadButton(outputId = "downloadOutputSummary", label = "Save Output Summary")
                                      ),
                                      
                                      tabPanel(title = "Plot", id = "plotTab",
                                               imageOutput("infectedExposedPlot"),
                                               imageOutput("cumulativePlot"),
                                               imageOutput("fullPlot"),
                                               imageOutput("fracSusPlot"),
                                               #downloadButton(outputId = "downloadPlot", label = "Save Image")
                                      )
                                  ) # tabsetPanel
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
                      
                      p(span("Michael, Myer, Crystal Wai, Gursimran Dhaliwal, Timothy Pulfer, Ryan Darby, and Jason Szeto", style= "font-weight:bold" )),    
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

server <- function(input, output, session){
  
  iv <- InputValidator$new()
  iv_alpha <- InputValidator$new()
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
  
  iv_seeddataupload$add_rule("seedData", sv_required())
  iv_seeddataupload$add_rule("seedData", ~ if(is.null(fileInputs$smStatus) || fileInputs$smStatus == 'reset') "Required")
  
  iv_alpha$condition(~ isTRUE(input$modelSelect == "SVEIRD"))
  
  iv$add_validator(iv_alpha)
  iv$add_validator(iv_seeddataupload)
  
  iv$enable()
  iv_alpha$enable()
  iv_seeddataupload$enable()
  
  observe({
    print(iv$validate())
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
    smStatus = NULL
  )
  # values$df <- data.frame(Variable = character(), Value = character()) 
  # output$table <- renderTable(values$df)
  
  susceptible <- reactive({
    req(!is.null(input$selectedCountry) && input$selectedCountry != "")
    
    createSusceptibleLayer(input$selectedCountry, 0, FALSE, level1Names = NULL)
  })
  
  ############################################################################    
  # Create a country plot cropped by level1Identifier and output to UI       #
  ############################################################################ 
  # observeEvent(input$go, {
  #   if(input$clipLev1 == TRUE){
  #     output$croppedOutputImage <- renderImage({
  #       source("R/clippingBaseRasterHaxby.R")
  #       outfile <- tempfile(fileext = '.png')
  #       
  #       png(outfile, width = 800, height = 600)
  #       createClippedRaster(selectedCountry = input$selectedCountry, level1Region = input$level1List, rasterAgg = input$agg, directOutput = T)
  #       dev.off()
  #       
  #       list(src = outfile, contentType = 'image/png', width = 600, height = 400, alt = "Base plot image not found")
  #     }, deleteFile = TRUE)
  #   }
  # })
  
  ############################################################################    
  # Output population base plot image to the app UI                          #
  ############################################################################ 
  observeEvent(input$go, {
    req(iv$is_valid())
    output$outputImage <- renderImage({
      source("R/rasterBasePlot.R")
      outfile <- tempfile(fileext = '.png')
      
      #createBasePlot(input$selectedCountry, input$agg, FALSE) # print the susceptible plot to www/
      png(outfile, width = 1068, height = 768)
      #createBasePlot(selectedCountry = input$selectedCountry, rasterAgg = input$agg, directOutput = TRUE)  # print the susceptible plot direct to UI
      isolate(createBasePlot(selectedCountry = input$selectedCountry, susceptible()$Susceptible, directOutput = TRUE))  # print the susceptible plot direct to UI
      dev.off()
      
      list(src = outfile, contentType = 'image/png', width = 1024, height = 768, alt = "Base plot image not found")
      # The above line adjusts the dimensions of the base plot rendered in UI
    }, deleteFile = TRUE)
  })
  
  ############################################################################    
  # Output IDE equations image to the app UI                                 #
  ############################################################################ 
  observeEvent(input$go, {
    req(iv$is_valid())
    output$modelImg <- renderImage({
      return(list(src= "www/ModelEquations.png",
                  contentType = "image/png"))
    }, deleteFile = FALSE)
  })
  
  ############################################################################    
  # Output flowchart image to the app UI                                     #
  ############################################################################ 
  observeEvent(input$go, {
    req(iv$is_valid())
    output$flowchartImg <- renderImage({
      if (input$modelSelect == "SEIRD"){
        return(list(src= "www/SEIRD.png",
                    contentType = "image/png"))
      }
      else if (input$modelSelect == "SVEIRD"){
        return(list(src = "www/SVEIRD.png",
                    contentType = "image/png"))
      }
    }, deleteFile = FALSE)
  })
  
  ############################################################################    
  # Reset all parameter sliders, country selection, etc.                     #
  ############################################################################ 
  observeEvent(input$resetAll, {
    shinyjs::reset("dashboard")
    
    shinyjs::disable(id = "go")
    values$allow_simulation_run <- FALSE
  })
  
  ############################################################################    
  # Checks to see that a new file has been uploaded (helper func)            #
  ############################################################################ 
  observeEvent(input$seedData, {
    values$allow_simulation_run <- TRUE
    fileInputs$smStatus <- 'uploaded'
  })
  
  ############################################################################    
  # Check if all mandatory fields have a value                               #
  ############################################################################   
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
  
  ##############################################################################
  #highlight drop down item when hovering                                      #
  ##############################################################################
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
  
  ############################################################################    
  # This static ui field is in server since other dynamic ui elements need it#
  ############################################################################
  output$countryDropdown <- renderUI({
    pickerInput(
      inputId = "selectedCountry",
      labelMandatory (strong("Country")), 
      choices = population$Country,
      multiple = FALSE,
      selected = NULL, # "Democratic Republic of Congo", #
      options = pickerOptions(
        actionsBox = TRUE,
        title = "Please select a country")
    )
  })
  
  ############################################################################    
  # Dynamically display the checkbox option to select for states/provinces   #
  ############################################################################
  output$clipStateCheckbox <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      checkboxInput(inputId = "clipLev1", label = strong("Clip State(s)/Province(s)"), value = FALSE
      )}
  })
  
  ############################################################################     
  # Checkbox for Data Assimilation                                           #
  ############################################################################ 
  output$dataAssimCheckbox <- renderUI({
    validate(need(!is.null(input$selectedCountry), ""))
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      checkboxInput(inputId = "dataAssim", label = strong("Include data assimilation?"), value = FALSE)
    }
  })
  
  ############################################################################    
  # Create select box for choosing input country                             #
  ############################################################################      
  output$Level1Ui <- renderUI({
    validate(need(input$clipLev1 == TRUE, "")) # catches UI warning
    
    isoCode <- countrycode(input$selectedCountry, origin = "country.name", destination = "iso3c")
    
    if (file.exists(paste0("gadm/", "gadm36_", toupper(isoCode), "_1_sp.rds"))){
      level1Options <<- readRDS(paste0("gadm/", "gadm36_", toupper(isoCode), "_1_sp.rds"))$NAME_1 
    } else {
      level1Options <<- getData("GADM", download = TRUE, level = 1, country = toupper(isoCode))$NAME_1 
    }
    
    selectizeInput(inputId = "level1List", "",
                   choices = level1Options,
                   selected = c("Kwara", "Oyo"),
                   multiple = TRUE,
                   options = list(placeholder = "Select state(s)/province(s)"))
    
  })
  
  ############################################################################     
  # Radio button for SEIRD vs SVEIRD Model                                   #
  ############################################################################   
  output$modelRadio <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      radioButtons(inputId = "modelSelect",
                   label = strong("Epidemic Model"),
                   choiceValues = list("SEIRD","SVEIRD"),
                   choiceNames = list("SEIRD","SVEIRD"),
                   selected = "SEIRD", #character(0), # 
                   inline = TRUE,
                   width = "1000px")
    }
  })
  
  ############################################################################     
  # Radio button for Deterministic vs Stochastic Model                       #
  ############################################################################  
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
  
  ############################################################################    
  # TODO: refactor numericInputs into single function                        #
  ############################################################################ 
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
                   label = HTML(paste("Daily Vaccination Rate ", TeX("&#945"))),
                   value = alphaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  ############################################################################    
  #                                                                          #
  ############################################################################ 
  output$betaInput <- renderUI({
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
                   label = HTML(paste("Daily Exposure Rate ", TeX("&#946"))), 
                   value = betaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  ############################################################################    
  #                                                                          #
  ############################################################################ 
  output$gammaInput <- renderUI({
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
                   label = HTML(paste("Daily Vaccination Rate ", TeX("&#947"))), 
                   value = gammaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  ############################################################################    
  #                                                                          #
  ############################################################################ 
  output$sigmaInput <- renderUI({
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
                   label = HTML(paste("Daily Recovery Rate ", TeX("&#963"))), 
                   value = sigmaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  ############################################################################    
  #                                                                          #
  ############################################################################ 
  output$deltaInput <- renderUI({
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
                   label = HTML(paste("Daily Death Rate ", TeX("&#948"))),
                   value = deltaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  ############################################################################    
  #                                                                          #
  ############################################################################ 
  
  #helpText('NOTE: Radius of 1 is called the Moore neighbourhood.'),
  #HTML("<p>NOTE: Radius of 1 is called the <a href='https://en.wikipedia.org/wiki/Moore_neighborhood'>Moore neighbourhood</a></p>", target = "_blank"),
  #p("NOTE:Radius of 1 is called the",a("Moore neighbourhood", href="https://en.wikipedia.org/wiki/Moore_neighborhood", target="_blank")),
  
  output$lambdaInput <- renderUI({
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
      
      numericInput(inputId = "lambda",
                   label = HTML(paste("Distance Parameter", TeX("&#955"))),
                   value = lambdaValue,min = 1, max = 50, step = 1)
    }
  })
  
  ############################################################################    
  #                     Upload Seed Data                                     #
  ############################################################################ 
  output$seedUpload <- renderUI({
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      fileInput(inputId = "seedData", labelMandatory("Upload Seed Data:"), placeholder = "Upload seed data (.csv or .xls or .xlsx)",
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
  
  ############################################################################    
  #                                                                          #
  ############################################################################ 
  output$startDateInput <- renderUI({
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
      
      dateInput('date', "Choose simulation start date:", value = startDateInput, max = Sys.Date(),
                format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                language = "en", width = NULL)
    }
  })
  
  ############################################################################     
  # numeric input for number of iterations                                   #
  ############################################################################  
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
  ############################################################################     
  # Data Assimilation settings                                               #
  ############################################################################
  
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
      fileInput(inputId = "dataAssimZones", labelMandatory ("Upload the lat/lon coordinates of reporting health zones (.csv or .xls or .xlsx)"),
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
      fileInput(inputId = "assimIData", labelMandatory ("Upload infection data to be assimilated with the model (.csv or .xls or .xlsx)"),
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
      fileInput(inputId = "assimDData", labelMandatory ("Upload death data to be assimilated with the model (.csv or .xls or .xlsx)"),
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
  
  ############################################################################    
  # Change the function which generates the Q matrix     #
  ############################################################################  
  output$varCovarFunc <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      selectInput(inputId = "covarianceSelect",
                  label = HTML("<span class='label-text'>Choose variance-covariance function:</span>"),
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
  
  ############################################################################    
  # Adjust parameter values for the variance=covariance function     #
  ############################################################################  
  
  output$selectRho <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "QCorrLength",
                   label = "Choose correlation length parameter for generating Q:",
                   value = 0.675,
                   step = 0.001,
                   min = 0)
    }
  })
  
  output$selectSigma <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "QVar",
                   label = "Choose variance parameter for generating Q:",
                   value = 0.55,
                   step = 0.01,
                   min = 0)
    }
  })
  
  output$selectNbhd <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "nbhd",
                   label = "Choose neighborhood parameter for generating Q:",
                   value = 3,
                   step = 1,
                   min = 0)
    }
  })
  
  output$selectPsiDiag <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "psidiag",
                   label = HTML(paste("Choose a value for the zero elements of", TeX("&#936"), "to be set to:")),
                   value = 0.001,
                   step = 0.001,
                   min = 0)
    }
  })
  
  ############################################################################    
  # Change the recommended aggregation factor for slider dynamically         #
  ############################################################################  
  output$aggInput <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      sliderInput(inputId = "agg",
                  label = "Aggregation Factor",
                  min = 0, max = 100, step = 1, value = population$reco_rasterAgg[match(input$selectedCountry, population$Country)])
    }
  })
  
  ############################################################################    
  # Output the .mp4 video from www/ to the app UI                            #
  ############################################################################  
  output$outputVideo <- renderUI({
    tags$video(
      id = "video", 
      type = "video/mp4",
      src = "MP4/Infected_MP4.mp4",  # TODO: dynamically change which mp4 is printed
      controls = "controls"
    )
  })
  
  ############################################################################    
  # Output bubble plot with initial seed data directly to the app UI         #
  ############################################################################ 
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
    source("R/makePlots.R")
    output$infectedExposedPlot <- makePlot(compartments = c("E", "I"), input = input, plotTitle = paste0("Time-series plot of Exposed and Infectious compartments in ", input$selectedCountry), xTitle = paste0("Day (from ", input$date, ")"), "Compartment Value", lineThickness = lineThickness)
    
    output$cumulativePlot <- makePlot(compartments = c("D"), input = input, plotTitle = paste0("Estimated Cumulative COVID-19 Deaths in ", input$selectedCountry), xTitle = paste0("Day (from ", input$date, ")"), yTitle = "Cumulative Deaths", lineThickness = lineThickness)
    
    if (input$modelSelect == "SVEIRD"){
      output$fullPlot <- makePlot(compartments = c("S", "V", "E", "I", "R", "D"), input = input, plotTitle = paste0("Time-series plot of epidemic compartments in ", input$selectedCountry), xTitle = paste0("Day (from ", input$date, ")"), yTitle = "Compartment Value", lineThickness = lineThickness)
    } else {
      output$fullPlot <- makePlot(compartments = c("S", "E", "I", "R", "D"), input = input, plotTitle = paste0("Time-series plot of epidemic compartments in ", input$selectedCountry), xTitle = paste0("Day (from ", input$date, ")"), yTitle = "Compartment Value", lineThickness = lineThickness)
    }
    
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
  
  ##########################################################################    
  # Allow the user to download the time-series plots from UI               #
  ########################################################################## 
  # observeEvent(input$go, {
  #   # TODO: implement downloading of files
  # })
  
  ############################################################################    
  # Generate seed data and have an option to download the file locally       #
  ############################################################################ 
  
  output$seedDataButton <- renderUI({
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      downloadButton('downloadData', label = "Generate Seed Data Template", 
                     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
                     style = "length:800px")
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
    }
    
    fileInputs$smStatus <- 'reset'
  })
  
  ############################################################################    
  # Multiple functionality when 'Run Simulation' is pressed                  #
  ############################################################################ 
  observeEvent(input$go, {
    req(iv$is_valid())
    
    isCropped <- FALSE
    
    if(input$clipLev1 == TRUE)
    {
      isCropped <- TRUE
    }
    else
    {
      isCropped <- FALSE
    }
    
    print(paste0(c("isCropped", isCropped)))
    
    source("R/rasterStack.R")
    #rs <- createRasterStack(input$selectedCountry, input$agg, isCropped, level1Names = input$level1List)
    
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
    
    output$tableSeed <- renderDataTable({ # print initial seed data to UI
      req(input$seedData)
      if(is.null(data())){return ()}
      data()
    })
    
    output$outputSummary <- renderDataTable({ # print output summary to UI
      outputSummaryTable <- read_excel(paste0("www/MP4/", countrycode(input$selectedCountry, "country.name", "iso3c"), "_summary.xlsx"))
      outputSummaryTable
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
    
    #source("R/rasterSimulation.R")
    source("R/rasterSimulation_DA.R")
    
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
    
    SpatialCompartmentalModelWithDA(model = input$modelSelect, startDate = input$date, selectedCountry = input$selectedCountry, directOutput = FALSE, rasterAgg = input$agg, 
                                    alpha, beta, gamma, sigma, delta, radius = radius, lambda = input$lambda, timestep = input$timestep, seedFile = input$seedData$datapath, seedRadius = 0,
                                    deterministic = isDeterministic, isCropped = input$clipLev1, level1Names = input$level1List, DA = input$dataAssim, sitRepData = input$dataAssimZones$datapath, 
                                    dataI = input$assimIData$datapath, dataD = input$assimDData$datapath, varCovarFunc = input$covarianceSelect, QVar = input$QVar, 
                                    QCorrLength = input$QCorrLength, nbhd = input$nbhd, psiDiag = input$psidiag)

    # row1  <- data.frame(Variable = "Country", Value = input$selectedCountry)
    # row2  <- data.frame(Variable = "WorldPop Raster Dimension", Value = paste0(rs$nRows, " rows x ", rs$nCols, " columns = ", rs$nCells, " grid cells"))
    # row3  <- data.frame(Variable = "Aggregation Factor", Value = input$agg)
    # row4  <- data.frame(Variable = "Aggregated Raster Dimension", Value = paste0(nrow(rs$rasterStack), " rows x ", ncol(rs$rasterStack), " columns = ", ncell(rs$rasterStack), " grid cells"))
    # row5  <- data.frame(Variable = "Compartmental Model", Value = input$modelSelect)
    # row6  <- data.frame(Variable = "Model Parameters", Value = paste("Alpha:", alpha,"Beta:", beta,"Gamma:", gamma, "Sigma:", sigma,"Delta:", delta))
    # row7  <- data.frame(Variable = "Average Distance Travelled/Day (in km)", Value = input$lambda)
    # row8  <- data.frame(Variable = "Radius (1 = Moore neighbourhood)", Value = radius)
    # row9  <- data.frame(Variable = "Uploaded Seed Data", Value = input$seedData$name)
    # row10 <- data.frame(Variable = "Number of iterations (days)", Value = input$timestep)
    # 
    #values$df <- rbind(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10)
    
    #########################################    
    # Output seed plot image to the app UI  #
    #########################################
    
    output$seedPlot <- renderImage({
      source("R/rasterClipSeedPlot.R")
      
      outfile <- tempfile(fileext = '.png')
      
      png(outfile, width = 1024, height = 768)
      createClippedSeedPlot(selectedCountry = input$selectedCountry, rasterAgg = input$agg, isCropped, level1Names = input$level1List, seedData = input$seedData$datapath, seedNeighbourhood = 0)  # print the seed plot direct to UI
      dev.off()
      
      list(src = outfile, contentType = 'image/png', width = 1024, height = 768, alt = "Seed plot image not found")
      # The above line adjusts the dimensions of the base plot rendered in UI
    }, deleteFile = TRUE)
  })
  
  observeEvent(input$filterLMIC,{
    updateCheckboxInput(session, inputId = "clipLev1", value = FALSE) # uncheck the clip box first
    if(input$filterLMIC){
      population <- population[population$LMIC == 'TRUE',]
    } else {
      population <- population #[population$LMIC == 'TRUE' || population$LMIC == 'FALSE']
    }
    updatePickerInput(session, inputId = 'selectedCountry', choices = population$Country, selected = "Nigeria")
  })
  
  ##########################
  #Input Summary Tab Panel #
  ##########################
  
  observeEvent(input$resetAll,{
    hideTab(inputId = 'tabSet', target = 'Input Summary')
    fileInputs$smStatus <- 'reset'
  })
  
  observe(
    hideTab(inputId = 'tabSet', target = 'Input Summary')
  )
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'Input Summary')
  })
  
  ###############################
  #Mathematical Model Tab Panel #
  ###############################
  
  observe(
    hideTab(inputId = 'tabSet', target = 'Mathematical Model')
  )
  
  observeEvent(input$resetAll,{
    hideTab(inputId= 'tabSet', target = 'Mathematical Model')
  })
  
  observeEvent(input$go,{
    showTab(inputId= 'tabSet', target = 'Mathematical Model')
  })
  
  ##############################
  #Schematic Diagram Tab Panel #
  ##############################
  
  observe(
    hideTab(inputId ='tabSet', target = 'Schematic Diagram')
  )
  
  observeEvent(input$resetAll,{
    hideTab(inputId = 'tabSet', target = 'Schematic Diagram')
  })
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'Schematic Diagram')
  })
  
  ##############################
  #Initial Seed Data Tab Panel #
  ##############################
  
  observeEvent(input$resetAll,{
    hideTab(inputId = 'tabSet', target = 'Initial Seed Data')
  })
  
  observe(
    hideTab(inputId = 'tabSet', target = 'Initial Seed Data')
  )
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'Initial Seed Data')
  })
  
  ###################################
  #Initial Seed Data Plot Tab Panel #
  ###################################
  
  observeEvent(input$resetAll,{
    hideTab(inputId = 'tabSet', target = 'Seed Data Map')
  })
  
  observe(
    hideTab(inputId = 'tabSet', target = 'Seed Data Map')
  )
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'Seed Data Map')
  })
  
  ##########################
  #MP4 Animation Tab Panel #
  ##########################
  
  observeEvent(input$resetAll,{
    hideTab(inputId = 'tabSet', target = 'MP4 Animation')
  })
  
  observe(
    hideTab(inputId = 'tabSet', target = 'MP4 Animation')
  )
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'MP4 Animation')
  })
  
  ###########################
  #Output Summary Tab Panel #
  ###########################
  
  observeEvent(input$resetAll,{
    hideTab(inputId = 'tabSet', target = 'Output Summary')
  })
  
  observe(
    hideTab(inputId = 'tabSet', target = 'Output Summary')
  )
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'Output Summary')
  })
  
  ####################
  #Plot Tab Panel    #
  ####################
  
  observeEvent(input$resetAll,{
    hideTab(inputId = 'tabSet', target = 'Plot')
  })
  
  observe(
    hideTab(inputId = 'tabSet', target = 'Plot')
  )
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'Plot')
  })
  
  # output$downloadOutputSummary <- downloadHandler(
  #   filename = function() {"output.csv"},
  #   content = function(file){
  #     write.csv(data(), file, row.names = FALSE)
  #   }
  #   
  # )
}

shinyApp(ui, server)