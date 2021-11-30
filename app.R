shhh <- suppressPackageStartupMessages # It's a library, so shhh!

shhh(library(lattice))
shhh(library(latticeExtra))
shhh(library(sp))
shhh(library(sf))     # classes and functions for vector data
shhh(library(shiny))
shhh(library(shinyjs))
shhh(library(shinyhelper))
shhh(library(shinyWidgets))
shhh(library(terra))  # suppressWarnings(suppressMessages(library(terra)))
shhh(library(raster)) # classes and functions for raster data
shhh(library(rgdal))
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

population <- read.xlsx("misc/population.xlsx", 1)

#fieldsMandatory <- c("selectedCountry", "modelSelect")

fieldsMandatory <- c("selectedCountry", "modelSelect", "seedData")

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
      titlePanel("Spatial Tracking of Infectious Disease Epidemics using Mathematical Models")
    ),
    
    navbarPage(title = span("Spatial Tracking of Infectious Disease Epidemics using Mathematical Models", style = "color:#000000; font-weight:bold; font-size:15pt"),
               
               tabPanel(title = "Model",
                        sidebarLayout(
                            sidebarPanel(
                                shinyjs::useShinyjs(),
                                shinyjs::inlineCSS(appCSS),
                                div(
                                    id = "dashboard",
                                    
                                    uiOutput("countryDropdown"),
                                    
                                    checkboxInput(inputId = "filterLMIC", label = strong("Filter LMIC"), value = FALSE),
                                    
                                    uiOutput("clipStateCheckbox"),
                                     
                                    conditionalPanel(condition = "input.clipLev1 == '1'",
                                                    uiOutput("Level1Ui")),
                                    
                                    # pickerInput(
                                    #   inputId = "targetYear",
                                    #   labelMandatory ("Year"), 
                                    #   choices = seq(2000, 2020),
                                    #   multiple = FALSE,
                                    #   selected = character(0),#
                                    #   options = pickerOptions(
                                    #     actionsBox = TRUE,
                                    #     title = "Please select a year")
                                    # ),
                                    
                                    uiOutput("aggSlider"),

                                    radioButtons(inputId = "modelSelect",
                                                 labelMandatory ("Epidemic Model"),
                                                 choiceValues = list("SEIRD","SVEIRD"),
                                                 choiceNames = list("SEIRD","SVEIRD"),
                                                 selected = "SVEIRD", # character(0),# 
                                                 inline = TRUE,
                                                 width = "1000px"),
                                    radioButtons(inputId = "stochasticSelect",
                                                 labelMandatory ("Model Stochasticity"),
                                                 choiceValues = list("Deterministic","Stochastic"),
                                                 choiceNames = list("Deterministic","Stochastic"),
                                                 selected = "Deterministic",
                                                 inline = TRUE,
                                                 width = "1000px"),

                                    conditionalPanel(
                                        #condition = "input.modelSelect == 'SEIR' || input.modelSelect == 'SEIRD' || input.modelSelect == 'SVEIRD'", 
                                        condition = "input.modelSelect == 'SEIRD' || input.modelSelect == 'SVEIRD'", 
                                        h5("Model Parameters:", style="font-weight: bold; font-size:11.5pt")
                                    ),
                                    
                                    
                                    conditionalPanel(
                                        id = "SVEIRD",
                                        withMathJax(),
                                        condition = "input.modelSelect == 'SVEIRD'", 
                                        
                                        sliderInput(inputId = "alpha",
                                                    label = "Daily Vaccination Rate (\\( \\alpha\\)):",
                                                    min = 0, max = 1, step = 0.0001, value = 0.00015
                                        )
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
                                        
                                        sliderInput(inputId = "beta", 
                                                    label = "Daily Exposure Rate (\\( \\beta\\))", 
                                                    min = 0, max = 1, step = 0.001, value = 0.025
                                        ),
                                        
                                        # checkboxInput(inputId = "varyingBeta", label = "Time-varying \\( \\beta\\)", value = FALSE),
                                        # 
                                        # conditionalPanel(
                                        #     condition = "input.varyingBeta == '1'",
                                        #     
                                        #     radioButtons("changeBeta", "increase/decrease  beta?",
                                        #                  #labelMandatory ("Vary"),
                                        #                  choiceValues = list("increaseBeta", "decreaseBeta"),
                                        #                  choiceNames = list("increase","decrease"),
                                        #                  selected = character(0),
                                        #                  inline = TRUE,
                                        #                  width = "1000px"),
                                        #     conditionalPanel(
                                        #         condition = "input.changeBeta == 'increaseBeta'",
                                        #         titlePanel("increase beta by ??? every"),
                                        #         numericInput(inputId = "increaseBetaFrequency", 
                                        #                      label = "days",
                                        #                      min = 0, max = 365, value = 1, step = 1)
                                        #     ),
                                        #     conditionalPanel(
                                        #         condition = "input.changeBeta == 'decreaseBeta'",
                                        #         titlePanel("decrease beta by ??? every"),
                                        #         numericInput(inputId = "decreaseBetaFrequency", 
                                        #                      label = "days",
                                        #                      min = 0, max = 365, value = 1, step = 1)
                                        #     ),
                                        # ),
                                        
                                        sliderInput(inputId = "gamma",
                                                    label = "Daily fraction that move out of the exposed compartment to the Infected compartment  (\\( \\gamma\\))", 
                                                    min = 0, max = 1, step = 0.001, value = 0.008
                                        ),
                                        
                                        sliderInput(inputId = "sigma",
                                                    label = "Daily fraction that move out of the Infected compartment to the recovered compartment (\\( \\sigma \\))", 
                                                    min = 0, max = 1, step = 0.001, value = 0.065
                                        ),
                                        
                                        sliderInput(inputId = "delta",
                                                    "Daily fraction that move out of the Infected compartment to the dead compartment (\\(\\delta\\)):",
                                                    min = 0, max = 1,step = 0.001, value = 0.0015
                                        )
                                    ),
                                    
                                    # conditionalPanel(
                                    #     id = "SEIRD_SVEIRD",
                                    #     withMathJax(),
                                    #     condition = "input.modelSelect == 'SEIRD' || input.modelSelect == 'SVEIRD'", 
                                    # 
                                    #     sliderInput(inputId = "delta",
                                    #                 "Daily fraction that move out of the Infected compartment to the dead compartment (\\(\\delta\\)):",
                                    #                 min = 0, max = 1,step = 0.001, value = 0.002
                                    #                 )
                                    # ),

                                    conditionalPanel(
                                        #condition = "input.modelSelect == 'SEIR' || input.modelSelect == 'SEIRD' || input.modelSelect == 'SVEIRD'", 
                                        condition = "input.modelSelect == 'SEIRD' || input.modelSelect == 'SVEIRD'", 
                                        
                                        # sliderInput(inputId = "radius",
                                        #             "Radius:", 
                                        #             min = 1, max = 4, step = 1, value = 2),
                                        #helpText('NOTE: Radius of 1 is called the Moore neighbourhood.'),
                                        #HTML("<p>NOTE: Radius of 1 is called the <a href='https://en.wikipedia.org/wiki/Moore_neighborhood'>Moore neighbourhood</a></p>", target = "_blank"),
                                        
                                        #p("NOTE:Radius of 1 is called the",a("Moore neighbourhood", href="https://en.wikipedia.org/wiki/Moore_neighborhood", target="_blank")),
                                        
                                        sliderInput(inputId = "lambda",
                                                    "Distance parameter (\\( \\lambda\\), in km):",
                                                    min = 5, max = 30, step = 5, value = 15),
                                        
                                        # helpText("Note: while the data view will show only",
                                        #          "the specified number of observations, the",
                                        #          "summary will be based on the full dataset."),
                                        # div(  
                                        #     class = "input-group",
                                        #     tags$span(
                                        #         style = "display: inline-block",
                                        #         sliderInput("radius",
                                        #                     " Radius:", 
                                        #                     min = 1, max = 4,step = 1, value = 1),
                                        #     ),
                                        #     tags$span(
                                        #         style = "vertical-align: bottom;",
                                        #         actionButton("b", "", icon = icon("question"))
                                        #     )
                                        # ),
                                        
                                        # div(#comment out, pull slider out 
                                        #     class = "input-group",
                                        #     tags$span(
                                        #         style = "display: inline-block",
                                        #         sliderInput("lambda",
                                        #                     " Distance parameter (\\( \\lambda\\), in km):", #Distance parameter (Lambda symbol, in km
                                        #                     min = 5, max = 30,step = 5, value = 5),
                                        #     ),
                                        #     tags$span(
                                        #         style = "vertical-align:bottom",
                                        #         actionButton("c", "", icon = icon("question"))
                                        #     )
                                        # ),
                                        # 
                                        
                                        fileInput("seedData", labelMandatory ("Upload initial seed data (.csv or .xls or .xlsx)"),
                                                  accept = c(
                                                      "text/csv",
                                                      "text/comma-separated-values,text/plain",
                                                      ".csv",
                                                      ".xls",
                                                      ".xlsx"),
                                        ),
                                        
                                        p("Click ",a("here", href="https://docs.google.com/spreadsheets/d/1WzCGqKiISuE_RvM5N4qK53W4Z_ivNA7b2Pq1Y4SZ4sY/edit#gid=0", target="_blank"), "for a template"),
                                        
                                        dateInput('date', "Choose simulation start date:", value = NULL, max = Sys.Date(),
                                                  format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                                  language = "en", width = NULL),
 
                                        # selectInput("Columns","Epidemic Variable to Plot",
                                        #             choices= c("Susceptible", "Vaccinated", "Exposed", "Infected", "Recovered", "Dead"), 
                                        #             selected = "Infected",
                                        #             multiple = TRUE),
                                        
                                        # Horizontal line ----
                                        # tags$hr(),
                                        
                                        numericInput(inputId = "timestep", 
                                                     label = "Number of Iterations (days)",
                                                     min = 5, max = 3650, value = 5, step = 1)

                                        # div(
                                        #     class = "input-group", #pull the csv file, comment out 
                                        #     tags$span(
                                        #         style = "display: inline-block",
                                        #         fileInput("seedData", "Choose .csv file",
                                        #                   accept = c(
                                        #                       "text/csv",
                                        #                       "text/comma-separated-values,text/plain",
                                        #                       ".csv")
                                        #         ),
                                        #     ),
                                        #     tags$span(
                                        #         style = "vertical-align: bottom;",
                                        #         actionButton("a", "", icon = icon("question"))
                                        #     )
                                        # ),
                                    ),
                                    
                                    actionButton("go","Run Simulation"),     # make button text bold
                                    actionButton("resetAll","Reset Values"), # make button text bold
                                ),
                            ), 
                            mainPanel(
                                tabsetPanel(id = 'tabSet',
                                    tabPanel(title = "Input Summary", verbatimTextOutput("summary"), 
                                             imageOutput("croppedOutputImage"),
                                             imageOutput("outputImage"),
                                             #imageOutput("seededOutputImage"),
                                             tableOutput("table"),
                                          #  downloadButton(outputId = "downloadSummary", label = "Save Input Summary as a PDF File")
                                          ),
                                    
                                    tabPanel(title = "Initial Seed Data", 
                                          
                                             dataTableOutput("tableSeed")
                                    ),
                                    
                                    tabPanel(title = "MP4 Animation",
                                           
                                             id = "mp4Tab",
                                            # tags$video(
                                            #     id="video", 
                                            #     type = "video/mp4",
                                            #     src = "susceptible_MP4.mp4", 
                                            #     controls = "controls"
                                            # ),
                                            uiOutput("outputVideo"),
                                            downloadButton(outputId = "downloadMP4", label = "Save MP4 Animation")),
                                    
                                    tabPanel(title = "Output Summary",
                                            
                                            tableOutput("outputSummary"),
                                            # downloadButton(outputId = "downloadOutputSummary", label = "Save Output Summary")
                                            ),
                                    
                                    tabPanel(title = "Plot", id = "plotTab",
                                    
                                             imageOutput("cumulativePlot"),
                                             imageOutput("fullPlot"),
                                             imageOutput("infectedExposedPlot"),
                                             imageOutput("fracSusPlot"),
                                             downloadButton(outputId = "downloadPlot", label = "Save Image"))
                                )
                            ),

                            # tabPanel("Model Introduction",
                            #          h1("Assumption"),
                            #          img(source = "SVEIRD.png"),
                            #          downloadButton(outputId = "downloadFigure", label = "Download Figure"))
                            # 
                            # Header: Background information
                            #
                            # Text: ABC
                            #
                            # Header: Assumptions
                            #
                            # Text: ABC
                            #
                            # Header: Schematic Diagrams
                            #
                            # Add SEIRD.png and SVEIRD.png with a Download figure for each
                            #
                            # Header: Integro-Differential Equations
                            #
                            # Text: ABC
                            #
                            # Header: References
                            #
                            # Text: ABC
                        )
               ),
               
               tabPanel("Authors",
                        h3("Research Team", style= "font-weight:bold"),
                        br(),   
                        p(span("Ashok Krishnamurthy, PhD", style= "font-weight:bold")),
                        p("Project PI,"),
                        p("Associate Professor, Department of Mathematics and Computing,"),
                        p("Mount Royal University,"), 
                        p("Calgary, AB, CANADA"),
                        
                        br(),
                        
                        p("Email:",a("akrishnamurthy@mtroyal.ca", href="mailto:akrishnamurthy@mtroyal.ca")), 
                        p("Website:", a(href="https://bit.ly/2YKrXjX","https://bit.ly/2YKrXjX", target="_blank")),
                        
                        br(),
                        
                        p(span("Crystal Wai, Gursimran Dhaliwal and Timothy Pulfer", style= "font-weight:bold" )),    
                        p("Undergraduate Student, Mount Royal University"),
                        
                        br(), 

                        p(span("Jake Doody", style= "font-weight:bold" )),
                        p("Undergraduate Student, University of Maryland Baltimore Country, MD, USA"),
                        
                        br(),
                        
                        p(span("Acknowledgement", style= "font-weight:bold" )),
                        p("Dr. Loren Cobb and Dr. Bedrich Sousedik "),
                        
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
    values$df <- data.frame(Variable = character(), Value = character()) 
    output$table <- renderTable(values$df)
    
    ############################################################################    
    # Reset all parameter sliders, country selection, etc.                     #
    ############################################################################ 
    observeEvent(input$resetAll, {
      shinyjs::reset("dashboard")
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
      shinyjs::toggleState(id = "go", condition = mandatoryFilled)
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
      validate(
        need(!is.null(input$selectedCountry), "Loading App...") # catches UI warning
      )
      if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
        checkboxInput(inputId = "clipLev1", label = strong("Clip State(s)/Province(s)"), value = FALSE)
      }
    })
    
    ############################################################################    
    # Create select box for choosing input country                             #
    ############################################################################      
    output$Level1Ui <- renderUI({
      validate(
        need(input$clipLev1 == TRUE, "Loading App...") # catches UI warning
      )
      isoCode <- countrycode(input$selectedCountry, origin = "country.name", destination = "iso3c")
      
      if (file.exists(paste0("gadm/", "gadm36_", isoCode, "_1_sp.rds"))){
        level1Options <<- readRDS(paste0("gadm/", "gadm36_", isoCode, "_1_sp.rds"))$NAME_1 
      } else {
        level1Options <<- getData("GADM", download = TRUE, level = 1, country = isoCode)$NAME_1 
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
      validate(
        need(!is.null(input$selectedCountry), "Loading App...") # catches UI warning
      )
      if (input$selectedCountry == ""){
        sliderInput(inputId = "agg", 
                    label = "Aggregation Factor", 
                    min = 0, max = 100, step = 1, value = 10)
      } else {
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
    # Output population base plot image to the app UI                          #
    ############################################################################ 
    observeEvent(input$go, {
      output$outputImage <- renderImage({
        source("#rasterBasePlot.R")
        outfile <- tempfile(fileext = '.png')
        
        createBasePlot(input$selectedCountry, input$agg, FALSE) # print the susceptible plot to www/
        png(outfile, width = 600, height = 400)
        createBasePlot(input$selectedCountry, input$agg, TRUE)  # print the susceptible plot direct to UI
        dev.off()
        
        list(src = outfile, contentType = 'image/png', width = 600, height = 400, alt = "Base plot image not found")
      }, deleteFile = TRUE)
    })
    
    ############################################################################    
    # Create a country plot cropped by level1Identifier and output to UI       #
    ############################################################################ 
    observeEvent(input$go, {
      if(input$clipLev1 == TRUE){
        output$croppedOutputImage <- renderImage({
          source("#clippingBaseRaster.R")
          outfile <- tempfile(fileext = '.png')
          
          png(outfile, width = 600, height = 400)
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
    #     source("#plotSeedData_RevisedV2_Ashok.R")
    #     outfile <- tempfile(fileext = '.png')
    #     
    #     # print the seed plot direct to UI
    #     png(outfile, width = 600, height = 400)
    #     plot(c(1,3,6,9,12), c(1.5,2,7,8,15), main = "Bubble Plot Placeholder") # TODO: example plot, below lines don't work due to "Regions defined for each Polygons" warning
    #     # print(input$selectedCountry)
    #     # print(input$date)
    #     # createSeedPlot(countryName = "Czech Republic", seedData = "seeddata/CZE_InitialSeedData.csv", startDate = "2021-07-01", source = "testSource") 
    #     dev.off()
    #     
    #     list(src = outfile, contentType = 'image/png', width = 600, height = 400, alt = "Seed image not found")
    #   }, deleteFile = TRUE)
    # })
    
    ############################################################################    
    # Output cumulative deaths as well as the fraction susceptible plot to UI  #
    ############################################################################ 
    lineThickness <- 1.5
    
    source("#makePlots.R")
    
    observeEvent(input$go, {
      output$cumulativePlot <- makePlot(compartments = c("D"), input = input, plotTitle = paste0("Estimated Cumulative COVID-19 Deaths in ", input$selectedCountry), xTitle = paste0("Day (from ", input$date, ")"), yTitle = "Cumulative Deaths", lineThickness = lineThickness)
      
      if (input$modelSelect == "SVEIRD"){
        output$fullPlot <- makePlot(compartments = c("S", "V", "E", "I", "R", "D"), input = input, plotTitle = paste0("Time-series plot of epidemic compartments in ", input$selectedCountry), xTitle = paste0("Day (from ", input$date, ")"), yTitle = "Compartment Value", lineThickness = lineThickness)
      } else {
        output$fullPlot <- makePlot(compartments = c("S", "E", "I", "R", "D"), input = input, plotTitle = paste0("Time-series plot of epidemic compartments in ", input$selectedCountry), xTitle = paste0("Day (from ", input$date, ")"), yTitle = "Compartment Value", lineThickness = lineThickness)
      }
      
      output$infectedExposedPlot <- makePlot(compartments = c("E", "I"), input = input, plotTitle = paste0("Time-series plot of Exposed and Infectious compartments in ", input$selectedCountry), xTitle = paste0("Day (from ", input$date, ")"), "Compartment Value", lineThickness = lineThickness)
        
      output$fracSusPlot <- renderImage({
        outfile <- tempfile(fileext = '.png')
        
        png(outfile, width = 600, height = 400)
        df <- read.xlsx(paste0("www/MP4/", countrycode(input$selectedCountry, "country.name", "iso3c"), "_summary.xlsx"), sheetIndex = 1)
        plotData = data.frame(X = df[,"S"]/df[,"N"], Y = df[,"I"]/df[,"N"])
        p = ggplot(plotData, mapping = aes(X, Y, group = 1)) +
          geom_line(aes(X, Y), size=lineThickness, color="black") +
          labs(title = paste0(input$selectedCountry, " SI Phase Plane (", input$date, ", ", input$timestep, " timesteps)"),
               x = "Fraction susceptible", y = "Fraction Infected")
        plot(p)
        dev.off()
        
        list(src = outfile, contentType = 'image/png', width = 600, height = 400, alt = "Image not found")
      }, deleteFile = TRUE)
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
        source("#rasterStack.R")
        rs <- createRasterStack(input$selectedCountry, input$agg)
        
        # ============= TAB TO SHOW SEED DATA IN TABLE ===========
        data <- reactive({               # read seed data from .csv or .xlsx
          req(input$seedData)
          ext <- tools::file_ext(input$seedData$datapath)
          seedData <- input$seedData
          if(ext == 'xlsx')
            readxl::read_excel(input$seedData$datapath)
          else 
            read.csv(input$seedData$datapath)
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

        #------------#
        # Parameters #
        #------------#

        alpha <- ifelse(input$modelSelect == "SVEIRD", input$alpha, 0) # DO NOT DELETE
        beta  <- input$beta  # DO NOT DELETE
        gamma <- input$gamma # DO NOT DELETE
        sigma <- input$sigma # DO NOT DELETE
        delta <- input$delta # ifelse(input$modelSelect == "SEIR", 0, input$delta) # DO NOT DELETE

        #print(paste(input$modelSelect, c(alpha, beta, gamma, sigma, delta)))

        source("#rasterSimulation.R")  # This code runs the spatial compartmental model

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
        row4 <- data.frame(Variable = "Compartment Model", Value = input$modelSelect)
        row5 <- data.frame(Variable = "Model Parameters", Value = paste("Alpha:", alpha,"Beta:", beta,"Gamma:", gamma, "Sigma:", sigma,"Delta:", delta))
        row6 <- data.frame(Variable = "Average Distance Travelled/Day (in km)", Value = input$lambda)
        row7 <- data.frame(Variable = "Radius (1 = Moore neighbourhood)", Value = radius)
        row8 <- data.frame(Variable = "Uploaded Seed Data", Value = input$seedData$name)
        row9 <- data.frame(Variable = "Number of iterations (days)", Value = input$timestep)

        values$df <- rbind(row1, row2, row3, row4, row5, row6, row7, row8, row9)
        
        # source("#rasterBasePlot.R")
        # createBasePlot(input$selectedCountry, input$agg, FALSE)
    })
    
    
    observeEvent(input$filterLMIC,{
      if(input$filterLMIC) value <- 'YES' else value <- 'NO'
      population <- population[population$LMIC == value,]
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
