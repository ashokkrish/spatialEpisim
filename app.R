shhh <- suppressPackageStartupMessages # It's a library, so shhh!

shhh(library(lattice))
shhh(library(latticeExtra))
options("rgdal_show_exportToProj4_warnings"="none")
shhh(library(sp))
shhh(library(sf))     # classes and functions for vector data
shhh(library(rgdal))
shhh(library(shiny))
shhh(library(shinyjs))
shhh(library(shinyhelper))
shhh(library(shinyWidgets))
shhh(library(terra))  # suppressWarnings(suppressMessages(library(terra)))
shhh(library(raster)) # classes and functions for raster data
shhh(library(ggplot2))
shhh(library(markdown))
shhh(library(cptcity))
shhh(library(rasterVis))
shhh(library(purrr))
shhh(library(stringr))
shhh(library(xlsx))
shhh(library(countrycode))
shhh(library(av))
shhh(library(dplyr))
shhh(library(maps))
shhh(library(shinyalert))
shhh(library(shinyvalidate))

population <- read.xlsx("misc/population.xlsx", 1)
epiparms <- read.xlsx("misc/epiparms.xlsx", 1)

fieldsMandatory <- c("selectedCountry", "modelSelect", "stochasticSelect", "seedData")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star {color: red;}"
appCSS <- ".invisible {display:none;}"

ui <- fluidPage(
  
  div(
    class = "invisible",
    titlePanel("Spatial Tracking of Infectious Diseases using Mathematical Models")
  ),
  
  navbarPage(title = span("Spatial Tracking of Infectious Diseases using Mathematical Models", style = "color:#000000; font-weight:bold; font-size:15pt"),
             
             tabPanel(title = "Model",
                      sidebarLayout(
                        sidebarPanel(
                          shinyjs::useShinyjs(),
                          shinyjs::inlineCSS(appCSS),
                          div(
                            id = "dashboard",
                            
                            uiOutput("countryDropdown"),
                            
                            checkboxInput(inputId = "filterLMIC", label = strong("Show LMIC only"), value = FALSE),
                            
                            uiOutput("clipStateCheckbox"),
                            
                            conditionalPanel(condition = "input.clipLev1 == '1'",
                                             uiOutput("Level1Ui")),

                            uiOutput("aggSlider"),
                            
                            uiOutput("modelRadio"),
                            
                            uiOutput("stochasticRadio"),
                            
                            conditionalPanel(
                              #condition = "input.modelSelect == 'SEIR' || input.modelSelect == 'SEIRD' || input.modelSelect == 'SVEIRD'", 
                              condition = "input.modelSelect == 'SEIRD' || input.modelSelect == 'SVEIRD'", 
                              h5("Model Parameters:", style="font-weight: bold; font-size:11.5pt")
                            ),
                            
                            conditionalPanel(
                              id = "SVEIRD",
                              withMathJax(),
                              condition = "input.modelSelect == 'SVEIRD'", 
                              
                              uiOutput("alphaSlider"),
                            ),
                            
                            # conditionalPanel(
                            #     id = "SEIR",
                            #     withMathJax(),
                            #     condition = "input.modelSelect == 'SEIR'",
                            # 
                            # ),
                            
                            conditionalPanel(
                              id = "SEIR_SEIRD_SVEIRD",
                              withMathJax(),
                              #condition = "input.modelSelect == 'SEIR' || input.modelSelect == 'SEIRD' || input.modelSelect == 'SVEIRD'", 
                              condition = "input.modelSelect == 'SEIRD' || input.modelSelect == 'SVEIRD'", 
                              
                              uiOutput("betaSlider"),
   
                              uiOutput("gammaSlider"),
                              
                              uiOutput("sigmaSlider"),
                              
                              uiOutput("deltaSlider"),
                            ),
                            
                            conditionalPanel(
                              withMathJax(),
                              #condition = "input.modelSelect == 'SEIR' || input.modelSelect == 'SEIRD' || input.modelSelect == 'SVEIRD'", 
                              condition = "input.modelSelect == 'SEIRD' || input.modelSelect == 'SVEIRD'", 
                              
                              #helpText('NOTE: Radius of 1 is called the Moore neighbourhood.'),
                              #HTML("<p>NOTE: Radius of 1 is called the <a href='https://en.wikipedia.org/wiki/Moore_neighborhood'>Moore neighbourhood</a></p>", target = "_blank"),
                              #p("NOTE:Radius of 1 is called the",a("Moore neighbourhood", href="https://en.wikipedia.org/wiki/Moore_neighborhood", target="_blank")),
                              
                              uiOutput("lambdaSlider"),

                              fileInput(inputId = "seedData", labelMandatory ("Upload initial seed data (.csv or .xls or .xlsx)"),
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv",
                                          ".xls",
                                          ".xlsx"),
                              ),
                              
                              p("Click ", a("here", href="https://docs.google.com/spreadsheets/d/1aEfioSNVVDwwTt6ky7MrOQj5uGO7QQ1NTB2TdwOBhrM/edit?usp=sharing", target="_blank"), "for a template of initial seed data"),
                              
                              uiOutput("startDateInput"),

                              uiOutput("timestepInput")
                              
                              # numericInput(inputId = "timestep", 
                              #              label = "Number of Iterations (days)",
                              #              min = 1, max = 3650, value = 3, step = 1)
                            ),
                            
                            actionButton("go","Run Simulation", 
                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                            actionButton("resetAll","Reset Values", 
                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          ),
                        ), 
                        
                        mainPanel(
                          tabsetPanel(id = 'tabSet',
                                      tabPanel(title = "Input Summary", verbatimTextOutput("summary"), 
                                               tableOutput("table"),
                                               imageOutput("outputImage"),
                                               imageOutput("croppedOutputImage"),
                                               #imageOutput("seededOutputImage"),
                                               #downloadButton(outputId = "downloadSummary", label = "Save Input Summary as a PDF File")
                                      ),
                                      
                                      tabPanel(title = "Initial Seed Data", 
                                               dataTableOutput("tableSeed")
                                      ),
                                      
                                      tabPanel(title = "MP4 Animation",
                                               id = "mp4Tab",
                                               uiOutput("outputVideo"),
                                               downloadButton(outputId = "downloadMP4", label = "Save MP4 Animation")),
                                      
                                      tabPanel(title = "Output Summary",
                                               tableOutput("outputSummary"),
                                               # downloadButton(outputId = "downloadOutputSummary", label = "Save Output Summary")
                                      ),
                                      
                                      tabPanel(title = "Plot", id = "plotTab",
                                               imageOutput("infectedExposedPlot"),
                                               imageOutput("cumulativePlot"),
                                               imageOutput("fullPlot"),
                                               imageOutput("fracSusPlot"),
                                               downloadButton(outputId = "downloadPlot", label = "Save Image"))
                          )
                        ),
                      )
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
                      
                      p(span("Gursimran Dhaliwal, Crystal Wai, Timothy Pulfer, and Ryan Darby", style= "font-weight:bold" )),    
                      p("Undergraduate Student, Mount Royal University, Calgary, AB, CANADA"),
                      
                      br(), 
                      
                      p(span("Jake Doody", style= "font-weight:bold" )),
                      p("Undergraduate Student, University of Maryland Baltimore Country, MD, USA"),
                      
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
             )
  )
)

server <- function(input, output, session){
  values <- reactiveValues()
  values$allow_simulation_run <- TRUE
  values$df <- data.frame(Variable = character(), Value = character()) 
  output$table <- renderTable(values$df)
  
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
  })
  
  ############################################################################    
  # Check if all mandatory fields have a value                               #
  ############################################################################   
  observe({
        mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      # enable/disable the submit button
      if (isolate(values$allow_simulation_run) == TRUE){
        shinyjs::toggleState(id = "go", condition = mandatoryFilled)
    }
  })
  
  ############################################################################    
  # This static ui field is in server since other dynamic ui elements need it#
  ############################################################################
  output$countryDropdown <- renderUI({
    pickerInput(
      inputId = "selectedCountry",
      labelMandatory ("Country"), 
      choices = population$Country,
      multiple = FALSE,
      select = NULL,
      options = pickerOptions(
        actionsBox = TRUE,
        title = "Please select a country")
    )
  })
  
  ############################################################################    
  # Dynamically display the checkbox option to select for states/provinces   #
  ############################################################################
  output$clipStateCheckbox <- renderUI({
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      checkboxInput(inputId = "clipLev1", label = strong("Clip State(s)/Province(s)"), value = FALSE)
      }
  })
  
  ############################################################################    
  # Create select box for choosing input country                             #
  ############################################################################      
  output$Level1Ui <- renderUI({
    validate(need(input$clipLev1 == TRUE, "Loading App...")) # catches UI warning

    isoCode <- countrycode(input$selectedCountry, origin = "country.name", destination = "iso3c")

    if (file.exists(paste0("gadm/", "gadm36_", toupper(isoCode), "_1_sp.rds"))){
      level1Options <<- readRDS(paste0("gadm/", "gadm36_", toupper(isoCode), "_1_sp.rds"))$NAME_1 
    } else {
      level1Options <<- getData("GADM", download = TRUE, level = 1, country = toupper(isoCode))$NAME_1 
    }
    
    selectizeInput(inputId = "level1List", "",
                   choices = level1Options,
                   selected = "", multiple = TRUE,
                   options = list(placeholder = "Select state(s)/province(s)"))
  })
  
  ############################################################################    
  # Change the recommended aggregation factor for slider dynamically         #
  ############################################################################  
  output$aggSlider <- renderUI({
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning

     if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      sliderInput(inputId = "agg",
                  label = "Aggregation Factor",
                  min = 0, max = 100, step = 1, value = population$reco_rasterAgg[match(input$selectedCountry, population$Country)])
                  }
  })
  
  ############################################################################     
  # Radio button for SEIRD vs SVEIRD Model                                   #
  ############################################################################   
  output$modelRadio <- renderUI({
       validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
       
       if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
            radioButtons(inputId = "modelSelect",
                         labelMandatory ("Epidemic Model"),
                         choiceValues = list("SEIRD","SVEIRD"),
                         choiceNames = list("SEIRD","SVEIRD"),
                         selected = "SVEIRD", #character(0), # 
                         inline = TRUE,
                         width = "1000px")
       }
  })
  
  ############################################################################     
  # Radio button for Deterministic vs Stochastic Model                       #
  ############################################################################  
  output$stochasticRadio <- renderUI({
       validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
       
       if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
            radioButtons(inputId = "stochasticSelect",
                         labelMandatory ("Model Stochasticity"),
                         choiceValues = list("Deterministic","Stochastic"),
                         choiceNames = list("Deterministic","Stochastic"),
                         selected = "Deterministic", #character(0), #
                         inline = TRUE,
                         width = "1000px")
            }
  })
  
  ############################################################################    
  # TODO: refactor numericInouts into single function #
  ############################################################################ 
  output$alphaSlider <- renderUI({
    alphaValue <- 0.00015
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    if(input$modelSelect == "SEIRD"){
      if (input$selectedCountry == "Czech Republic"){
        alphaValue <- filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"alpha"]
      } else if (input$selectedCountry == "Nigeria"){
        alphaValue <- filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"alpha"]
      }
    } else if (input$modelSelect == "SVEIRD"){
      if (input$selectedCountry == "Czech Republic"){
        alphaValue <- filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"alpha"]
      } else if (input$selectedCountry == "Nigeria"){
        alphaValue <- filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"alpha"]
      }
    }
     
    numericInput(inputId = "alpha",
                label = "Daily Vaccination Rate (\\( \\alpha\\)):",
                value = alphaValue, min = 0, max = 1, step = 0.00001)
  })
  
  ############################################################################    
  #                                                                          #
  ############################################################################ 
  output$betaSlider <- renderUI({
    betaValue <- 0.00001
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    if(input$modelSelect == "SEIRD"){
      if (input$selectedCountry == "Czech Republic"){
        betaValue <- filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"beta"]
      } else if (input$selectedCountry == "Nigeria"){
        betaValue <- filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"beta"]
      }
    } else if (input$modelSelect == "SVEIRD"){
      if (input$selectedCountry == "Czech Republic"){
        betaValue <- filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"beta"]
      } else if (input$selectedCountry == "Nigeria"){
        betaValue <- filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"beta"]
      }
    }
    
    numericInput(inputId = "beta",
                label = "Daily Exposure Rate (\\( \\beta\\))", 
                value = betaValue, min = 0, max = 1, step = 0.00001)
  })
  
  ############################################################################    
  #                                                                          #
  ############################################################################ 
  output$gammaSlider <- renderUI({
    gammaValue <- 0.008
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    if(input$modelSelect == "SEIRD"){
      if (input$selectedCountry == "Czech Republic"){
        gammaValue <- filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"gamma"]
      } else if (input$selectedCountry == "Nigeria"){
        gammaValue <- filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"gamma"]
      }
    } else if (input$modelSelect == "SVEIRD"){
      if (input$selectedCountry == "Czech Republic"){
        gammaValue <- filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"gamma"]
      } else if (input$selectedCountry == "Nigeria"){
        gammaValue <- filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"gamma"]
      }
    }

    numericInput(inputId = "gamma",
                label = "Daily fraction that move out of the exposed compartment to the Infected compartment  (\\( \\gamma\\))", 
                value = gammaValue, min = 0, max = 1, step = 0.00001)
  })
  
  ############################################################################    
  #                                                                          #
  ############################################################################ 
  output$sigmaSlider <- renderUI({
    sigmaValue <- 0.065
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    if(input$modelSelect == "SEIRD"){
      if (input$selectedCountry == "Czech Republic"){
        sigmaValue <- filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"sigma"]
      } else if (input$selectedCountry == "Nigeria"){
        sigmaValue <- filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"sigma"]
      }
    } else if (input$modelSelect == "SVEIRD"){
      if (input$selectedCountry == "Czech Republic"){
        sigmaValue <- filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"sigma"]
      } else if (input$selectedCountry == "Nigeria"){
        sigmaValue <- filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"sigma"]
      }
    }
    
    numericInput(inputId = "sigma",
                label = "Daily fraction that move out of the Infected compartment to the recovered compartment (\\( \\sigma \\))", 
                value = sigmaValue, min = 0, max = 1, step = 0.00001)
  })
  
  ############################################################################    
  #                                                                          #
  ############################################################################ 
  output$deltaSlider <- renderUI({
    deltaValue <- 0.0015
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    if(input$modelSelect == "SEIRD"){
      if (input$selectedCountry == "Czech Republic"){
        deltaValue <- filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"delta"]
      } else if (input$selectedCountry == "Nigeria"){
        deltaValue <- filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"delta"]
      }
    } else if (input$modelSelect == "SVEIRD"){
      if (input$selectedCountry == "Czech Republic"){
        deltaValue <- filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"delta"]
      } else if (input$selectedCountry == "Nigeria"){
        deltaValue <- filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"delta"]
      }
    }

    numericInput(inputId = "delta",
                "Daily fraction that move out of the Infected compartment to the dead compartment (\\(\\delta\\)):",
                value = deltaValue, min = 0, max = 1, step = 0.00001)
  })
  
  ############################################################################    
  #                                                                          #
  ############################################################################ 
  output$lambdaSlider <- renderUI({
    lambdaValue <- 15
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    if(input$modelSelect == "SEIRD"){
      if (input$selectedCountry == "Czech Republic"){
        lambdaValue <- filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"lambda"]
      } else if (input$selectedCountry == "Nigeria"){
        lambdaValue <- filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"lambda"]
      }
    } else if (input$modelSelect == "SVEIRD"){
      if (input$selectedCountry == "Czech Republic"){
        lambdaValue <- filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"lambda"]
      } else if (input$selectedCountry == "Nigeria"){
        lambdaValue <- filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"lambda"]
      }
    }
    
    numericInput(inputId = "lambda",
                "Distance parameter (\\( \\lambda\\), in km):",
                value = lambdaValue,min = 1, max = 50, step = 1)
  })
  
  ############################################################################    
  #                                                                          #
  ############################################################################ 
  output$startDateInput <- renderUI({
    startDateInput <- NULL
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    if(input$modelSelect == "SEIRD"){
      if (input$selectedCountry == "Czech Republic"){
        startDateInput <- filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"startDate"]
      } else if (input$selectedCountry == "Nigeria"){
        startDateInput <- filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"startDate"]
      }
    } else if (input$modelSelect == "SVEIRD"){
      if (input$selectedCountry == "Czech Republic"){
        startDateInput <- filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"startDate"]
      } else if (input$selectedCountry == "Nigeria"){
        startDateInput <- filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"startDate"]
      }
    }
    
    dateInput('date', "Choose simulation start date:", value = startDateInput, max = Sys.Date(),
              format = "yyyy-mm-dd", startview = "month", weekstart = 0,
              language = "en", width = NULL)
  })
  
  
  ############################################################################     
  # numeric input for number of iterations                       #
  ############################################################################  
  output$timestepInput <- renderUI({
       validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
       
       if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
            numericInput(inputId = "timestep",
                         label = "Number of Iterations (days)",
                         min = 1, max = 3650, value = 3, step = 1)
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
  # Output population base plot image to the app UI                          #
  ############################################################################ 
  observeEvent(input$go, {
    output$outputImage <- renderImage({
      source("R/rasterBasePlot.R")
      outfile <- tempfile(fileext = '.png')
      
      #createBasePlot(input$selectedCountry, input$agg, FALSE) # print the susceptible plot to www/
      png(outfile, width = 800, height = 600)
      createBasePlot(input$selectedCountry, input$agg, TRUE)  # print the susceptible plot direct to UI
      dev.off()
      
      list(src = outfile, contentType = 'image/png', width = 600, height = 400, alt = "Base plot image not found")
      # The above line adjusts the dimensions of the base plot rendered in UI
    }, deleteFile = TRUE)
  })
  
  ############################################################################    
  # Create a country plot cropped by level1Identifier and output to UI       #
  ############################################################################ 
  observeEvent(input$go, {
    if(input$clipLev1 == TRUE){
      output$croppedOutputImage <- renderImage({
        #source("R/clippingBaseRaster.R")
        #print(getwd())
        source("R/clippingBaseRasterHaxby.R")
        outfile <- tempfile(fileext = '.png')
        
        png(outfile, width = 800, height = 600)
        createClippedRaster(selectedCountry = input$selectedCountry, level1Region = input$level1List, rasterAgg = input$agg)
        dev.off()
        
        list(src = outfile, contentType = 'image/png', width = 600, height = 400, alt = "Base plot image not found")
      }, deleteFile = TRUE)
    }
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
  #     # print(input$selectedCountry)
  #     # print(input$date)
  #     # createSeedPlot(countryName = "Czech Republic", seedData = "seeddata/CZE_InitialSeedData.csv", startDate = "2021-07-01", source = "testSource") 
  #     dev.off()
  #     
  #     list(src = outfile, contentType = 'image/png', width = 1024, height = 768, alt = "Seed image not found")
  #   }, deleteFile = TRUE)
  # })
  
  lineThickness <- 1.5
  
  observeEvent(input$go, {
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
  # Multiple functionality when 'Run Simulation' is pressed                  #
  ############################################################################ 
  observeEvent(input$go, {
    source("R/rasterStack.R")
    rs <- createRasterStack(input$selectedCountry, input$agg)
    
    # ============= TAB TO SHOW SEED DATA IN TABLE ===========
    data <- reactive({               # read seed data from .csv or .xlsx
      req(input$seedData)
      ext <- tools::file_ext(input$seedData$datapath)
      seedData <- input$seedData
      if(ext == 'xlsx'){
        readxl::read_excel(input$seedData$datapath)
      } else {
          read.csv(input$seedData$datapath)
      }
    })
    
    output$tableSeed <- renderDataTable({ # output initial seed data to UI
      req(input$seedData)
      if(is.null(data())){return ()}
      data()
    })
    
    output$outputSummary <- renderTable({ # print output summary table to UI
      # req(input$seedData)
      # TODO: make sure the file exists
      outputSummaryTable <- read.xlsx(paste0("www/MP4/", countrycode(input$selectedCountry, "country.name", "iso3c"), "_summary.xlsx"), sheetIndex = 1)
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
    
    #print(data())          # This prints the entire seed data
    
    #print(names(data()))   # This prints the column names of the seed data

    alpha <- ifelse(input$modelSelect == "SVEIRD", input$alpha, 0) # DO NOT DELETE
    beta  <- input$beta  # DO NOT DELETE
    gamma <- input$gamma # DO NOT DELETE
    sigma <- input$sigma # DO NOT DELETE
    delta <- input$delta # ifelse(input$modelSelect == "SEIR", 0, input$delta) # DO NOT DELETE
    
    source("R/rasterSimulation.R")
    
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
    
    SpatialCompartmentalModel(model = input$modelSelect, startDate = input$date, selectedCountry = input$selectedCountry, directOutput = FALSE, rasterAgg = input$agg, alpha, beta, gamma, sigma, delta, radius = radius, lambda = input$lambda, timestep = input$timestep, seedFile = data(), deterministic = isDeterministic)
    
    row1 <- data.frame(Variable = "Country", Value = input$selectedCountry)
    row2 <- data.frame(Variable = "Aggregation Factor", Value = input$agg)
    row3 <- data.frame(Variable = "Aggregated Raster Dimension", Value = paste0(nrow(rs$rasterStack) , " rows x ", ncol(rs$rasterStack), " columns = ", ncell(rs$rasterStack), " grid cells" ))
    row4 <- data.frame(Variable = "Compartmental Model", Value = input$modelSelect)
    row5 <- data.frame(Variable = "Model Parameters", Value = paste("Alpha:", alpha,"Beta:", beta,"Gamma:", gamma, "Sigma:", sigma,"Delta:", delta))
    row6 <- data.frame(Variable = "Average Distance Travelled/Day (in km)", Value = input$lambda)
    row7 <- data.frame(Variable = "Radius (1 = Moore neighbourhood)", Value = radius)
    row8 <- data.frame(Variable = "Uploaded Seed Data", Value = input$seedData$name)
    row9 <- data.frame(Variable = "Number of iterations (days)", Value = input$timestep)
    
    values$df <- rbind(row1, row2, row3, row4, row5, row6, row7, row8, row9)
    
    # source("R/rasterBasePlot.R")
    # createBasePlot(input$selectedCountry, input$agg, FALSE)
  })
  
  observeEvent(input$filterLMIC,{
    updateCheckboxInput(session, inputId = "clipLev1", value = FALSE) # uncheck the clip box first
    if(input$filterLMIC){
      population <- population[population$LMIC == 'TRUE',]
    } else {
      population <- population[population$LMIC == 'TRUE' || population$LMIC == 'FALSE']
    }
    updatePickerInput(session, inputId = 'selectedCountry', choices = population$Country)
  })
  
  observe(
    hideTab(inputId = 'tabSet', target = 'Input Summary')
  )
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'Input Summary')
  })
  
  observe(
    hideTab(inputId = 'tabSet', target = 'Initial Seed Data')
  )
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'Initial Seed Data')
  })
  
  observe(
    hideTab(inputId = 'tabSet', target = 'MP4 Animation')
  )
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'MP4 Animation')
  })
  
  observe(
    hideTab(inputId = 'tabSet', target = 'Output Summary')
  )
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'Output Summary')
  })
  
  observe(
    hideTab(inputId = 'tabSet', target = 'Plot')
  )
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'Plot')
  })
  
  observeEvent(input$resetAll,{
    hideTab(inputId = 'tabSet', target = 'Input Summary')
  })
  
  observeEvent(input$resetAll,{
    hideTab(inputId = 'tabSet', target = 'Initial Seed Data')
  })
  
  observeEvent(input$resetAll,{
    hideTab(inputId = 'tabSet', target = 'MP4 Animation')
  })
  
  observeEvent(input$resetAll,{
    hideTab(inputId = 'tabSet', target = 'Output Summary')
  })
  
  observeEvent(input$resetAll,{
    hideTab(inputId = 'tabSet', target = 'Plot')
  })
  
  # output$downloadOutputSummary <- downloadHandler(
  #   filename = function() {"output.csv"},
  #   content = function(file){
  #     write.csv(data(), file, row.names = FALSE)
  #   }
  #   
  # )
}

shinyApp(ui,server)