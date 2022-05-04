library(shiny)
library(shinyjs)
library(shinyWidgets)
#library(xlsx)
library(readxl)
#library(echarts4r)
library(data.table)
population <- read_excel("misc/population.xlsx", 1)
source("R/rasterBasePlot.R")

ui <- fluidPage(
  
  navbarPage(title = span("2020 UN-Adjusted Population Count Rasters", style = "color:#000000; font-weight:bold; font-size:15pt"),
             
             tabPanel(title = "Raster Base Plot",
                      sidebarLayout(
                        sidebarPanel(

                          div(
                            id = "dashboard",
                            
                            pickerInput(
                                 inputId = "selectedCountry",
                                 choices = population$Country,
                                 multiple = FALSE,
                                 select = NULL,
                                 options = pickerOptions(
                                      actionsBox = TRUE,
                                      title = "Please select a country"),
                                 width = "240px")

                            # , radioButtons(
                            #      inputId = "qValue",
                            #      label = ("Image Size"),
                            #      choiceValues = list(1, 0),
                            #      choiceNames = list("800 x 600", "1024 x 768"),
                            #      inline = TRUE,
                            #      width = "1000px",
                            #      selected = "0"
                            #        )
                            ),
                        ), 
                        
                        mainPanel(
                          tabsetPanel(id = 'tabSet',
                                      tabPanel(#title = "Raster Base Plot", 
                                               imageOutput("outputImage")
                              )
                         )
                    ),
                  #print("tablePlot")
                   
               )
          )
     ),
  downloadButton('downloadPlot', 'Save Image'),
)

server <- function(input, output, session){
  # validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
    output$outputImage <- renderImage({
      if (input$selectedCountry == ""){
        list(src = "", width = 0, height = 0)
      } else {
        outfile <- tempfile(fileext = '.png')
        
        #createBasePlot(input$selectedCountry, 1, FALSE) # print the susceptible plot to www/
        png(outfile, width = 1024, height = 768)
        createBasePlot(input$selectedCountry, 1, TRUE)  # print the susceptible plot direct to UI
        dev.off()
        
        list(src = outfile, contentType = 'image/png', width = 1024, height = 768, alt = "Base plot image not found")
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
    
    #output$tablePlot <- data.table(group=c("Group 1","Group 1","Group 2","Group 2","Group 2"), subgroup = c("A","A","A","A","B"),value = c(2,2.5,1,2,1.5))
   
}

shinyApp(ui,server)