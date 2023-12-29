library(dplyr)
library(DT)
library(readxl)
library(shiny)
library(shinyjs)
library(shinyWidgets)

population <- read_excel("misc/population.xlsx", 1)

source("R/rasterBasePlot.R")
source("R/clippingBaseRasterHaxby.R")
source("R/rasterStack.R")

ui <- fluidPage(
  
  navbarPage(title = span("WorldPop Visualizer", style = "color:#000000; font-weight:bold; font-size:15pt"),
             
             tabPanel(title = "Plotting a GeoTIFF raster",
                      sidebarLayout(
                        sidebarPanel(
                          div(
                            id = "dashboard",
                            
                            uiOutput("countryDropdown"),
                            
                            uiOutput("clipStateCheckbox"),
                            
                            conditionalPanel(condition = "input.clipLev1 == '1'",  uiOutput("Level1Ui")),
                            
                            # uiOutput("aggSlider"),

                            conditionalPanel(id = "listCheck", condition = "input.level1List != null", uiOutput("clippedPlotButton")),
                            
                            br(),

                            # uiOutput("seedDataButton"),
                            # 
                            # br(),
                            # 
                            # uiOutput("tableButton"),
                          
                            # uiOutput("downloadTableButton")
                           
                            # , radioButtons(
                            #      inputId = "qValue",
                            #      label = ("Image Size"),
                            #      choiceValues = list(1, 0),
                            #      choiceNames = list("800 x 600", "1024 x 768"),
                            #      inline = TRUE,
                            #      width = "1000px",
                            #      selected = "0"
                            #        )
                            )
                           , width = 3
                        ), 
                        
                        mainPanel(
                          tabsetPanel(id = 'tabSet',
                                      tabPanel(id = "main", title ="2020 UN-Adjusted Population Count Map", imageOutput("outputImage"),
                                               #downloadButton('downloadPlot', 'Save Image')
                                               ),
                                      tabPanel(title ="Selected State/Province Map", imageOutput("croppedOutputImage"),

                                               ),
                                      # tabPanel(title ="Population Count by State/Province",
                                      #          DT::dataTableOutput("aggTable")
                                      #          )
                         )
                    ) #mainPanel
               ) # sidebarLayout
          ) #tabPanel
     ) #navbarPage
) # fluidPage

server <- function(input, output, session){
  
  output$countryDropdown <- renderUI({
    pickerInput(
      inputId = "selectedCountry",
      label = strong("Country"),
      choices = population$Country,
      multiple = FALSE,
      select = NULL, #"Democratic Republic of Congo", #
      options = pickerOptions(
        actionsBox = TRUE,
        title = "Please select a country"),
      width = "240px"
    )
  })
  
  ############################################################################    
  # Dynamically display the checkbox option to select for states/provinces   #
  ############################################################################
  output$clipStateCheckbox <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      checkboxInput(inputId = "clipLev1", label = strong("Clip State(s)/Province(s)"), value = TRUE)
    }
  })

  #output$aggSlider <- renderUI({
  #validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
  
  #if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
  # sliderInput(inputId = "agg",
  #            label = "Aggregation Factor",
  #           min = 0, max = 100, step = 1, value = population$reco_rasterAgg[match(input$selectedCountry, population$Country)])
  #}
  #})
  
  output$outputImage <- renderImage({
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
    
    if (input$selectedCountry == ""){
      list(src = "", width = 0, height = 0)
    } else {
      outfile <- tempfile(fileext = '.png')
      
      createBasePlot(input$selectedCountry, 1, FALSE) # print the susceptible plot to www/
      
      png(outfile, width = 800, height = 600)
      #png(outfile, width = 1024, height = 768)
      createBasePlot(input$selectedCountry, 1, TRUE)   # print the susceptible plot direct to UI
      dev.off()
      
      list(src = outfile, contentType = 'image/png', width = 800, height = 600, alt = "Base plot image not found")
      #list(src = outfile, contentType = 'image/png', width = 1024, height = 768, alt = "Base plot image not found")
    }
  }, deleteFile = TRUE)
  
  # output$downloadPlot <- downloadHandler(
  #   isoCode <- countrycode(input$selectedCountry, origin = "country.name", destination = "iso3c"),
  #   filename = sprintf("%s_2020PopulationCount.png", isoCode),
  #   content = function(outfile) {
  #     if (input$selectedCountry != ""){
  #       png(outfile, width = 800, height = 600)
  #       #png(outfile, width = 1024, height = 768)
  #       createBasePlot(input$selectedCountry, 1, TRUE)
  #       dev.off()
  #     }
  #   })
  
  ######################################################################################    
  # Create a table of population count stratified by states/provinces and output to UI #
  ###################################################################################### 
  
  # output$tableButton <- renderUI({
  #   validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
  #   
  #   if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
  #     actionButton("table","Population Count Table", 
  #                  style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", 
  #                  style = "length:800px")
  #   }
  # })
  
  # observeEvent(input$table, {
  #   isCropped <- FALSE
  #   
  #   if(input$clipLev1 == TRUE)
  #   {
  #     isCropped <- TRUE
  #   }
  #   else
  #   {
  #     isCropped <- FALSE
  #   }
  # 
  #   rs <- createRasterStack(input$selectedCountry, 0, isCropped)
  #   sus <- rs$rasterStack$Susceptible
  #   lvOne <- rs$rasterStack$Level1Raster
  #   names <- rs$Level1Identifier$NAME_1
  #   #print(rs$rasterStack)
  #   #print(sus)
  #   #print(lvOne)
  #   #print(names)
  #   #popCount <- crosstab(sus,lvOne)
  #   #print(popCount)
  #   
  #   lvMatrix <- as.matrix(lvOne)
  #   susMatrix <- as.matrix(sus)
  #   nMatrix <- as.matrix(names)
  #   
  #   sumMatrix <- round(aggregate(c(susMatrix) ~ c(lvMatrix), FUN = sum))
  #   nameFrame <- data.frame(nMatrix)
  #   tableFrame <- data.frame(sumMatrix)
  #   testFrame <- data.frame(sumMatrix)
  #   #tableFrame <- tail(tableFrame, -1)
  #   tableFrame <- tableFrame %>% slice(-1)
  #   colnames(tableFrame) <- c("Nums", "Values") # renaming Columns to make it easier to reference them
  #   colnames(nameFrame) <- c("Names")
  #   tableFrame$Nums <- nameFrame$Names
  #   colnames(tableFrame) <- c("State/Province", "Population Count") # Changing Names so it matches to Spec
  #   #print(nameFrame)
  #   #print(rsMatrix)
  #   #print(testFrame)
  #   
  #   # output$aggTable = DT::renderDataTable({DT::datatable(tableFrame,
  #   #                                                      options = list(paging = FALSE,
  #   #                                                                     pageLength = nrow(tableFrame),
  #   #                                                                     #autoWidth = TRUE,
  #   #                                                                     #scrollX = TRUE,
  #   #                                                                     columnDefs = list(list(width = '20%', targets = "_all"))
  #   #                                                      ),
  #   #                                                      selection = 'single',
  #   #                                                      rownames = FALSE
  #   # )})
  # })
  
  ################################################    
  # Create select box for choosing input country #
  ################################################ 
  
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
                   selected = 1, multiple = TRUE,
                   options = list(placeholder = "Select a state/province"))
  })
  
  ############################################################################    
  # Create a country plot cropped by level1Identifier and output to UI       #
  ############################################################################ 
  
  output$clippedPlotButton <- renderUI({
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      actionButton("go","Plot clipped raster", 
                   style ="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                   style ="length:800px")
    }
  })
  
  observeEvent(input$go, {
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
     if(input$clipLev1 == TRUE){

       if(!is.null(input$level1List) || input$level1List != "" ){
          output$croppedOutputImage <- renderImage({
  
            outfile <- tempfile(fileext = '.png')
            
            #png(outfile, width = 800, height = 600)
            #png(outfile, width = 1024, height = 768)
            print(input$level1List)
            
            if(!is.null(input$level1List)){
              #createClippedRaster(selectedCountry = input$selectedCountry, level1Region = input$level1List, rasterAgg = 0, directOutput = FALSE)
              png(outfile, width = 800, height = 600)
              createClippedRaster(selectedCountry = input$selectedCountry, level1Region = input$level1List, rasterAgg = 0, directOutput = TRUE) # Why is rasterAgg set to 0?
              dev.off()
            }
            
            list(src = outfile, contentType = 'image/png', width = 800, height = 600, alt = "Select at least one state/province to plot")
  
          }, deleteFile = TRUE)
        }
     }
    }
  })

  ############################################################################    
  # Generate seed data and have an option to download the file locally       #
  ############################################################################ 
  
  # output$seedDataButton <- renderUI({
  #   validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
  #   
  #   if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
  #     downloadButton('downloadData', "Generate Seed Data", 
  #                    style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
  #                    style = "length:800px")
  #   }
  # })
  
  # observeEvent(input$selectedCountry, {
  #   validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
  #   
  #   if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
  #     
  #     inputISO <- countrycode(input$selectedCountry, origin = 'country.name', destination = 'iso3c') # Converts country name to ISO Alpha
  #     
  #     gadmFileName <- paste0("gadm36_", inputISO, "_1_sp.rds")  # name of the .rds file
  # 
  #     gadmFolder <- "gadm/" # .rds files should be stored in local gadm/ folder
  # 
  #     # if (file.exists(paste0(gadmFolder, gadmFileName)))
  #     # {
  #         Level1Identifier <- readRDS(paste0(gadmFolder, gadmFileName))
  #     # }
  #     # else
  #     # {
  #     #   Level1Identifier <- getData("GADM", level = 1, country = inputISOLower)
  #     # }
  #     #print(coordinates(Level1Identifier)) # coords of the region
  #     #print(Level1Identifier$NAME_1) # List of all states/provinces/regions
  #     
  #     seedNames <- Level1Identifier$NAME_1
  #     seedCoords <- coordinates(Level1Identifier)
  #     #print(seedCoords)
  #     seedVaxx <- c(0)
  #     seedExpo <- c(0)
  #     seedInfect <- c(0)
  #     seedRec <- c(0)
  #     seedDead <- c(0)
  #     seedCombine <- cbind(seedNames, seedCoords, seedVaxx, seedExpo, seedInfect, seedRec, seedDead)
  #     frameCombine <- data.frame(seedCombine)
  # 
  #     frameCombine <- frameCombine[c("seedNames", "V3", "V2", "seedVaxx", "seedExpo", "seedInfect", "seedRec", "seedDead")]
  #     
  #     colnames(frameCombine) <- c("Location", "lat", "lon", "InitialVaccinated", "InitialExposed", "InitialInfections", "InitialRecovered", "InitialDead")
  #     #print(frameCombine)
  #     
  #     isoCode <- countrycode(input$selectedCountry, origin = "country.name", destination = "iso3c")
  #     sheetName <- sprintf("%s_initialSeedData", isoCode)
  #     
  #     output$downloadData <- downloadHandler(
  #       filename = function() {
  #         paste(sheetName, Sys.Date(), ".csv",  sep = "")
  #       },
  #       content = function(sheetName) {
  #         write.csv(frameCombine, sheetName, row.names = FALSE)
  #       }
  #     )
  #   }
  # })

  ########################################
  #Selected State/Province Map Tab Panel #
  ########################################
  
  observe(
    hideTab(inputId = 'tabSet', target = 'Selected State/Province Map')
  )
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'Selected State/Province Map')
  })

  # observe(
  #   hideTab(inputId = 'tabSet', target = 'Population Count by State/Province')
  # )
  # 
  # observeEvent(input$table,{
  #   showTab(inputId = 'tabSet', target = 'Population Count by State/Province')
  # })
}

shinyApp(ui,server)