library(shiny)
library(shinyjs)
library(shinyWidgets)
library(readxl)
library(DT)
library(dplyr)

population <- read_excel("misc/population.xlsx", 1)
source("R/rasterBasePlot.R")

ui <- fluidPage(
  
  navbarPage(title = span("2020 UN-Adjusted Population Count", style = "color:#000000; font-weight:bold; font-size:15pt"),
             
             tabPanel(title = "Plots and Tables",
                      sidebarLayout(
                        sidebarPanel(
                          div(
                            id = "dashboard",
                            
                            uiOutput("countryDropdown"),
                            
                            # uiOutput("aggSlider"),
                            
                            uiOutput("goButton"),
                          
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
                           ,width = 3
                        ), 
                        
                        mainPanel(
                          tabsetPanel(id = 'tabSet',
                                      tabPanel(title ="Population Map", imageOutput("outputImage"),
                                               #downloadButton('downloadPlot', 'Save Image')
                                               ),
                                      tabPanel(title ="Population Count by State/Province",
                                               DT::dataTableOutput("aggTable"))
                         )
                    )
               )
          )
     )
)

server <- function(input, output, session){
  
  output$countryDropdown <- renderUI({
    pickerInput(
      inputId = "selectedCountry",
      label = strong("Country"),
      choices = population$Country,
      multiple = FALSE,
      select = NULL,
      options = pickerOptions(
        actionsBox = TRUE,
        title = "Please select a country"),
      width = "240px"
    )
  })
  
  #output$aggSlider <- renderUI({
   # validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
    
    #if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
     # sliderInput(inputId = "agg",
      #            label = "Aggregation Factor",
       #           min = 0, max = 100, step = 1, value = population$reco_rasterAgg[match(input$selectedCountry, population$Country)])
    #}
  #})
  
  output$goButton <- renderUI({
    validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      actionButton("go","Population Count by State/Province", 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })
  
  output$outputImage <- renderImage({
      validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
      
      if (input$selectedCountry == ""){
        list(src = "", width = 0, height = 0)
      } else {
        outfile <- tempfile(fileext = '.png')
        
        #createBasePlot(input$selectedCountry, 1, FALSE) # print the susceptible plot to www/
        png(outfile, width = 1024, height = 768)
        createBasePlot(input$selectedCountry, 1, TRUE)  # print the susceptible plot direct to UI
        dev.off()
        
        list(src = outfile, contentType = 'image/png', width = 800, height = 600, alt = "Base plot image not found")
      }
    }, deleteFile = TRUE)
    
  output$downloadPlot <- downloadHandler(
      isoCode <- countrycode(input$selectedCountry, origin = "country.name", destination = "iso3c"),
      filename = sprintf("%s_2020PopulationCount.png",isoCode),
      content = function(outfile) {
        if (input$selectedCountry != ""){
          png(outfile, width = 1024, height = 768)
          createBasePlot(input$selectedCountry, 1, TRUE)
          dev.off()
        }
      })

  observeEvent(input$go, {
    source("R/rasterStack.R")
    rs <- createRasterStack(input$selectedCountry, 0)
    sus <- rs$rasterStack$Susceptible
    lvOne <- rs$rasterStack$Level1Raster
    names <- rs$Level1Identifier$NAME_1
    #print(rs$rasterStack)
    #print(sus)
    #print(lvOne)
    #print(names)
    #print(susMatrix)
 
    popCount <- crosstab(sus,lvOne)
    #print(popCount)
   
    lvMatrix <- as.matrix(lvOne)
    susMatrix <- as.matrix(sus)
    nMatrix <- as.matrix(names)
    
    sumMatrix <- round(aggregate(c(susMatrix) ~ c(lvMatrix), FUN = sum))
    nameFrame <- data.frame(nMatrix)
    tableFrame <- data.frame(sumMatrix)
    testFrame <- data.frame(sumMatrix)
    #tableFrame <- tail(tableFrame, -1)
    tableFrame <- tableFrame %>% slice(-1)
    colnames(tableFrame) <- c("Nums", "Values") #renaming Columns to make it easier to reference them
    colnames(nameFrame) <- c("Names")
    tableFrame$Nums <- nameFrame$Names
    colnames(tableFrame) <- c("State/Province", "Population Count") #Changing Names So it matches to Spec
    #print(nameFrame)
    #print(rsMatrix)
    print(testFrame)
   
    output$aggTable = DT::renderDataTable({DT::datatable(tableFrame,
                                                         options = list(paging = FALSE,
                                                                        pageLength = nrow(tableFrame),
                                                                        #autoWidth = TRUE,
                                                                        #scrollX = TRUE,
                                                                        columnDefs = list(list(width = '20%', targets = "_all"))
                                                                        ),
                                                         selection = 'single',
                                                         rownames = FALSE
                                          )})
    })
}

shinyApp(ui,server)