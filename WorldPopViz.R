library(bslib)
library(dplyr)
library(DT)
library(htmltools)
library(leaflet)
library(readr)
library(readxl)
library(shiny)
library(shinybusy)
library(shinyjs)
library(shinyvalidate)
library(shinyWidgets)

population <- read_excel("misc/population.xlsx", 1)
shortlist <- filter(population, shortList == "TRUE")

source("R/cropBaseRasterHaxby.R")
source("R/makeLollipop.R")
source("R/rasterBasePlot.R")
source("R/rasterLeafletPlot.R")
source("R/rasterStack.R")
source("R/seedDataBubblePlot.R")

ui <- fluidPage( # UI ----
  theme = bs_theme(version = 4, 
                   primary = "#18536F"),
  tags$head(
    tags$link(rel = "stylesheet", 
              type="text/css", 
              href="SE-banner.css")
  ),
  shinyjs::useShinyjs(),
  
  add_busy_spinner(spin = "cube-grid",
                   color = "#18536F",
                   margins = c("50%","50%")),
  
  navbarPage(title = span("WorldPop Visualizer", class = "pageTitle"),
             
             tabPanel(title = "Plotting a GeoTIFF raster",
                      sidebarLayout(
                        sidebarPanel( # sidebar ----
                          div(
                            id = "dashboard",
                            
                            uiOutput("countryDropdown"),
                            uiOutput("cropStateCheckbox"),
                            
                            conditionalPanel(id = "cropPlot",
                              condition = "input.cropLev1 == '1'",  
                                             
                              uiOutput("Level1Ui"),
                              
                              conditionalPanel(
                                condition = "input.level1List != ''",
                                
                                uiOutput("croppedPlotButton"),
                                br()
                              ),
                            ),
                            
                            # uiOutput("aggSlider"),
                            
                            br(),
                            uiOutput("transPathFileInputs"),
                            br(),
                            uiOutput("transPathDateInput"),
                            br(),
                            uiOutput("resetButton"),

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
                        
                        mainPanel( # mainpanel ----
                          div(id = "maptabPanels",
                            tabsetPanel(id = 'tabSet',
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
                                        tabPanel(title ="Leaflet Cropped Plot", 

                                                 br(),
                                                 leafletOutput("croppedLeafletMap",
                                                               width = 1024, 
                                                               height = 768),
                                                 br(),
                                                 br()
                                                 ),
                                        tabPanel(title = "terra Plot",
                                                   
                                                 br(),
                                                 imageOutput("outputImage"),
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
                                                 plotOutput("lollipop",
                                                            height = 1000),
                                                 br(),
                                                 br()
                                                 )
                                      # tabPanel(title ="Population Count by State/Province",
                                      #          DT::dataTableOutput("aggTable")
                                      #          )
                            ) # tabSet
                          ) # maptabPanels
                        ) #mainPanel
               ) # sidebarLayout
          ) #tabPanel
     ) #navbarPage
) # fluidPage

server <- function(input, output, session){ # Server ----
  
  valueRange <- c(0, 5, 10, 25, 50, 100, 250, 1000)
  ramp <- c('#FFFFFF', 
            '#D0D8FB', 
            '#BAC5F7', 
            '#8FA1F1', 
            '#617AEC', 
            '#0027E0', 
            '#1965F0', 
            '#0C81F8', 
            '#18AFFF', 
            '#31BEFF', 
            '#43CAFF', 
            '#60E1F0', 
            '#69EBE1', 
            '#7BEBC8', 
            '#8AECAE', 
            '#ACF5A8', 
            '#CDFFA2', 
            '#DFF58D', 
            '#F0EC78', 
            '#F7D767', 
            '#FFBD56', 
            '#FFA044', 
            '#EE4F4D')
  pal <- colorRampPalette(ramp)
  colorPalette <- colorBin(pal(8)[-1], domain = valueRange, bins = valueRange)
  
  #---------------------------------------#
  ##          Input Validators         ----
  #---------------------------------------#
  iv <- InputValidator$new()
  iv_dataupload <- InputValidator$new() 
  
  iv_dataupload$add_rule("latLonData", sv_required())
  iv_dataupload$add_rule("latLonData", ~ if(is.null(fileInputs$latLonStatus) || fileInputs$latLonStatus == 'reset') "Required")
  iv_dataupload$add_rule("incidenceData", sv_required())
  iv_dataupload$add_rule("incidenceData", ~ if(is.null(fileInputs$incidenceStatus) || fileInputs$incidenceStatus == 'reset') "Required")
  
  iv$add_validator(iv_dataupload)
  
  iv$enable()
  iv_dataupload$enable()
  
  
  #----------------------------------------------------------------------#
  # Values to flag when the lat/lon & incidence file inputs have files 
  # uploaded
  #----------------------------------------------------------------------#
  fileInputs <- reactiveValues(
    latLonStatus = NULL,
    incidenceStatus = NULL
  )
  
  
  output$countryDropdown <- renderUI({ ## countryDropdown ----
    pickerInput(
      inputId = "selectedCountry",
      label = strong("Country"),
      choices = shortlist$Country,
      multiple = FALSE,
      select = NULL, #"Democratic Republic of Congo", #
      options = pickerOptions(
        actionsBox = TRUE,
        title = "Please select a country"),
      width = "240px"
    )
  })
  
  observeEvent(input$selectedCountry, {
    if(!is.null(input$selectedCountry) && input$selectedCountry != "") {
      shinyjs::show(id = "maptabPanels")
    } else {
      shinyjs::hide(id = "maptabPanels")
    }
    
    fileInputs$latLonStatus <- 'reset'
    fileInputs$incidenceStatus <- 'reset'
  })

  
  #---------------------------------------#
  # Reactively rasterize selected country #
  #---------------------------------------#
  susceptible <- reactive({
    req(!is.null(input$selectedCountry) && input$selectedCountry != "")

    createSusceptibleLayer(input$selectedCountry, 0)
  })
  
  
  #--------------------------------------------------------------------------#    
  # Dynamically display the checkbox option to select for states/provinces   #
  #--------------------------------------------------------------------------#
  output$cropStateCheckbox <- renderUI({ # cropStateCheckbox ----
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      checkboxInput(
        inputId = "cropLev1", 
        label = strong("Crop State(s)/Province(s)"), 
        value = FALSE)
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
  
  #--------------------------------------------------------------------------#    
  # Display the file inputs for generating the transmission path             #
  #--------------------------------------------------------------------------#
  output$transPathFileInputs <- renderUI({
    req(!is.null(input$selectedCountry) && input$selectedCountry != "")
    
    tagList(
      fileInput(inputId = "latLonData", 
                label = strong("Upload Lat-Lon Data:"), 
                placeholder = "Upload Lat-Lon data (.csv or .xls or .xlsx)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xls",
                  ".xlsx"),
      ),
      
      br(),
      
      fileInput(inputId = "incidenceData", 
                label = strong("Upload Incidence/Death Data:"), 
                placeholder = "Upload Incidence/Death data (.csv or .xls or .xlsx)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xls",
                  ".xlsx"),
      )
    )
  })
  
  #--------------------------------------------------------------------------#    
  # Dynamically generate a date slider that contains the dates for all the 
  # observed data in the incidence/death file
  #--------------------------------------------------------------------------# 
  output$transPathDateInput <- renderUI({
    req(iv_dataupload$is_valid())
    
    dateInfo <- colnames(transPathData())[4:length(colnames(transPathData()))]
    
    sliderTextInput(
      inputId = "transPathDate",
      label = strong("Date"),
      choices = dateInfo,
      selected = dateInfo[1],
      animate = animationOptions(interval = 250, loop = FALSE)
    )
  })
  
  
  #--------------------------------------------------------------------------#    
  # Checks to see that files have been uploaded (helper func)                #
  #--------------------------------------------------------------------------# 
  observeEvent(input$latLonData, {
    fileInputs$latLonStatus <- 'uploaded'
  })
  
  observeEvent(input$incidenceData, {
    fileInputs$incidenceStatus <- 'uploaded'
  })
  
  
  #---------------------------------------------------------------------------#    
  # Combine the lat/long data with the observed infection into a single table #
  #---------------------------------------------------------------------------# 
  transPathData <- reactive({
    req(iv_dataupload$is_valid())

    incidenceData <- openDataFile(input$incidenceData)
    latLonData <- openDataFile(input$latLonData)

    incidence <- as.data.frame(t(incidenceData))
    incidenceCols <- incidence[2,]
    incidence <- incidence[3:nrow(incidence),]
    colnames(incidence) <- incidenceCols
    
    plotData <- cbind(latLonData, lapply(incidence, as.numeric))
  })
  
  
  
  output$outputImage <- renderImage({ ## outputImage ----
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
    
    if (input$selectedCountry == ""){
      list(src = "", width = 0, height = 0)
    } else {
      outfile <- tempfile(fileext = '.png')
      
      # createBasePlot(input$selectedCountry, 1, FALSE) # print the susceptible plot to www/
      
      # png(outfile, width = 800, height = 600)
      png(outfile, width = 1024, height = 768)
      createBasePlot(input$selectedCountry, susceptible()$Susceptible, TRUE)   # print the susceptible plot direct to UI
      dev.off()
      
      # list(src = outfile, contentType = 'image/png', width = 800, height = 600, alt = "Base plot image not found")
      list(src = outfile, contentType = 'image/png', width = 1024, height = 768, alt = "Base plot image not found")
    }
  }, deleteFile = TRUE)
  
  
  output$leafletMap <- renderLeaflet({
    req(!is.null(input$selectedCountry))
    
    susc <- susceptible()$Susceptible
    level1Names <- NULL
    
    createLeafletPlot(input$selectedCountry, level1Names, susc)
  })
  
  
  
  output$transmission <- renderLeaflet({
    req(!is.null(input$selectedCountry))
    req(iv_dataupload$is_valid())
    
    level1Names <- NULL
    
    if(input$cropLev1 == TRUE){
      
      if(!is.null(input$level1List) && !("" %in% input$level1List)){
        level1Names <- input$level1List
      }
    }
    
    createLeafletBubblePlot(input$selectedCountry, level1Names, transPathData(), 1)
  })
  
  
  output$lollipop <- renderPlot({
    req(iv_dataupload$is_valid())
    
    plotLolliChart(input$incidenceData$datapath)
  })
  
  
  #--------------------------------------------------------------------------#    
  # Proxy map for the leaflet plot to dynamically update the transmission
  # path data
  #--------------------------------------------------------------------------# 
  observe({
    req(!is.null(input$transPathDate))
    
    # which date (column of data) to plot
    transDate <- input$transPathDate
    
    plotData <- transPathData()
    
    # To access a column inside the leafletProxy function the column name must 
    # be called directly (can't use a variable storing the column name) so we
    # must set the column we want to a known name ("Current")
    colnames(plotData)[colnames(plotData) == transDate] <- "Current"
    
    labelText <- paste0(
      "Health Zone: ", plotData$HealthZone, "<br/>",
      "Count: ", plotData["Current"], "<br/>") %>%
      lapply(htmltools::HTML)
    
    # To update the map, clear out the old markers and draw new ones using the 
    # data from the newly selected date
    leafletProxy("transmission",
                 data = plotData) %>%
      clearMarkers() %>%
      addCircleMarkers(lng = ~Longitude,
                       lat = ~Latitude,
                       radius = ~Current,
                       weight = 1,
                       opacity = 1,
                       color = ~ifelse(Current > 0, "black", "transparent"),
                       fillColor = ~ifelse(Current > 0, colorPalette(Current), "transparent"),
                       fillOpacity = 0.8,
                       label = labelText)
  })
  
  
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
  
  #------------------------------------------------------------------------------------#    
  # Create a table of population count stratified by states/provinces and output to UI #
  #------------------------------------------------------------------------------------# 
  
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
  #   if(input$cropLev1 == TRUE)
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
  
  #----------------------------------------------#    
  # Create select box for choosing input country #
  #----------------------------------------------# 
  
  output$Level1Ui <- renderUI({ # level1Ui ----
    validate(need(input$cropLev1 == TRUE, "")) # catches UI warning
    
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
  
  #--------------------------------------------------------------------------#    
  # Create a country plot cropped by level1Identifier and output to UI       #
  #--------------------------------------------------------------------------# 
  
  output$croppedPlotButton <- renderUI({ # croppedPlotButton ----
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
    
    # if (!is.null(input$level1List) && input$level1List != ""){
      actionButton("go","Plot cropped raster", 
                   style ="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                   style ="length:800px")
    # }
  })
  
  
  output$resetButton <- renderUI({ # resetButton ----
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      actionButton(
        inputId = "reset",
        label = "Reset Values",
        style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"
      )
    }
  })
  
  observeEvent(input$go, {
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
     if(input$cropLev1 == TRUE){

       if(!is.null(input$level1List) && !("" %in% input$level1List)){
          output$croppedOutputImage <- renderImage({
  
            outfile <- tempfile(fileext = '.png')
            
            #png(outfile, width = 800, height = 600)
            #png(outfile, width = 1024, height = 768)
            print(input$level1List)
            
            # if(!is.null(input$level1List)){
              #createCroppedRaster(selectedCountry = input$selectedCountry, level1Region = input$level1List, rasterAgg = 0, directOutput = FALSE)
              png(outfile, width = 1024, height = 768)
              req(!is.null(input$level1List))
              isolate(createCroppedRaster(selectedCountry = input$selectedCountry, level1Region = input$level1List, susceptible()$Susceptible, directOutput = TRUE)) # Why is rasterAgg set to 0?
              dev.off()
            # }
            
            list(src = outfile, contentType = 'image/png', width = 1024, height = 768, alt = "Select at least one state/province to plot")
  
          }, deleteFile = TRUE)
          
          output$croppedLeafletMap <- renderLeaflet({
            req(!is.null(input$level1List))
            
            susc <- susceptible()$Susceptible
            level1Names <- input$level1List

            createLeafletPlot(input$selectedCountry, level1Names, susc)
          })
        }
     }
    }
  })
  
  observeEvent(input$reset, priority = 10, {
    shinyjs::reset(id = "cropPlot")
    shinyjs::reset(id = "cropLev1")
    shinyjs::reset(id = "selectedCountry")
    
    fileInputs$latLonStatus <- 'reset'
    fileInputs$incidenceStatus <- 'reset'
  })

  #--------------------------------------------------------------------------#    
  # Generate seed data and have an option to download the file locally       #
  #--------------------------------------------------------------------------# 
  
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

  #--------------------------------------#
  #Selected State/Province Map Tab Panel #
  #--------------------------------------#
  
  observeEvent(input$cropLev1, {
    updateTabsetPanel(inputId = "tabSet", selected = "Leaflet Plot")
  })
  
  observeEvent({(input$cropLev1 == FALSE || is.null(input$level1List))
                input$selectedCountry}, priority = 10, {
    hideTab(inputId = 'tabSet', target = 'Leaflet Cropped Plot')
  })
  
  observeEvent(input$go, {
    showTab(inputId = 'tabSet', target = 'Leaflet Cropped Plot')
  })

  # observe(
  #   hideTab(inputId = 'tabSet', target = 'Population Count by State/Province')
  # )
  # 
  # observeEvent(input$table,{
  #   showTab(inputId = 'tabSet', target = 'Population Count by State/Province')
  # })
  
  #--------------------------------------------------------------------#
  # Helper function to open different files based on their format
  #--------------------------------------------------------------------#
  openDataFile <- function(datafile) {
    ext <- tools::file_ext(datafile$name)
    ext <- tolower(ext)
    
    switch(ext, 
           csv = read_csv(datafile$datapath, show_col_types = FALSE),
           xls = read_xls(datafile$datapath),
           xlsx = read_xlsx(datafile$datapath),
           txt = read_tsv(datafile$datapath, show_col_types = FALSE),
           
           validate("Improper file format.")
    )
  }
}

shinyApp(ui,server)