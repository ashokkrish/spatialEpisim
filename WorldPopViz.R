library(shiny)
library(shinyjs)
library(shinyWidgets)
library(xlsx)

population <- read.xlsx("misc/population.xlsx", 1)
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
                                 select = "Nigeria",#NULL, #
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
                                      tabPanel(title = "Raster Base Plot", 
                                               imageOutput("outputImage")
                              )
                         )
                    )
               )
          )
     ),
  downloadButton('downloadPlot', 'Save Image'),
)

server <- function(input, output, session){
  # validate(need(!is.null(input$selectedCountry), "Loading App...")) # catches UI warning
  # if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
    output$outputImage <- renderImage({
      outfile <- tempfile(fileext = '.png')
      
      #createBasePlot(input$selectedCountry, 1, FALSE) # print the susceptible plot to www/
      png(outfile, width = 800, height = 600)
      createBasePlot(input$selectedCountry, 1, TRUE)  # print the susceptible plot direct to UI
      dev.off()
      
      list(src = outfile, contentType = 'image/png', width = 800, height = 600, alt = "Base plot image not found")
    }, deleteFile = TRUE)
    
    output$downloadPlot <- downloadHandler(
      isoCode <- countrycode(input$selectedCountry, origin = "country.name", destination = "iso3c"),
      filename = sprintf("%s_2020PopulationCount.png",isoCode),
      content = function(outfile) {
        png(outfile, width = 800, height = 600)
        createBasePlot(input$selectedCountry, 1, TRUE)
        dev.off()
      }) 
}

shinyApp(ui,server)