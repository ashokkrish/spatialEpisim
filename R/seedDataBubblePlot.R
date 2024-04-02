library(countrycode)
library(htmltools)
library(leaflet)
library(terra)

createLeafletBubblePlot <- function(selectedCountry, level1Names, plotData, activeCol) {

  inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c') #Converts country name to ISO Alpha
  
  gadmFileName <- paste0("gadm36_", toupper(inputISO), "_1_sp.rds")   # name of the .rds file 
  gadmFolder <- "gadm/"
  level1Identifier <- readRDS(paste0(gadmFolder, gadmFileName))
  
  if(!is.null(level1Names)){
    level1Identifier <- level1Identifier[which(level1Identifier$NAME_1 %in% level1Names), ]}
  
  valueRange <- c(0, 5, 10, 25, 50, 100, 250, 1000, 10000)
  # x <- classify(susceptible, valueRange)
  
  # plot(x, col=pal(8)[-1], xlab = "Longitude", ylab = "Latitude")
  
  # levs <- levels(x)[[1]]
  #levs[7] <- "> 1000"
  # levels(x) <- levs
  
  #ramp <- c('#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
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
  
  leafletPlot <- leaflet(plotData,
                         width = 1024, 
                         height = 768,
                         options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%
    addPolygons(data = level1Identifier,
                color = "#444444", 
                weight = 1.5, 
                smoothFactor = 1,
                opacity = 1.0, 
                fillColor = "#F5F5F5",
                fillOpacity = 0.75,
                popup = paste(level1Identifier$NAME_1),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = FALSE)) %>%
    addLegend(pal = colorBin(palette = pal(9)[-1],
                             bins = valueRange,
                             domain = valueRange),
              values = valueRange,
              opacity = 0.75,
              title = "Obs. persons",
              position = "topright")
  
  leafletPlot
}


