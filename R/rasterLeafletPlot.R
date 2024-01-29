library(countrycode)
library(leaflet)
library(terra, warn.conflicts = FALSE)

createLeafletPlot <- function(selectedCountry, susceptible) {
  
  inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c') #Converts country name to ISO Alpha
  
  valueRange <- c(0, 10, 25, 50, 100, 250, 1000, 100000)
  x <- classify(susceptible, valueRange)
  
  # plot(x, col=pal(8)[-1], xlab = "Longitude", ylab = "Latitude")
  
  levs <- levels(x)[[1]]
  #levs[7] <- "> 1000"
  levels(x) <- levs
  
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
  
  if(selectedCountry == "Czech Republic"){
    aggrPlotTitle <- paste0("2020 UN-Adjusted Population Count \n for the ", 
                            selectedCountry, 
                            " (1 sq. km resolution)")
  }
  else if(selectedCountry == "Democratic Republic of Congo"){
    aggrPlotTitle <- paste0("2020 UN-Adjusted Population Count \n for the ", 
                            selectedCountry, 
                            " (1 sq. km resolution)")
  }
  else if(selectedCountry == "Gambia"){
    aggrPlotTitle <- paste0("2020 UN-Adjusted Population Count \n for the ", 
                            selectedCountry, 
                            " (1 sq. km resolution)")
  }
  else if(selectedCountry == "Netherlands"){
    aggrPlotTitle <- paste0("2020 UN-Adjusted Population Count \n for the ", 
                            selectedCountry, 
                            " (1 sq. km resolution)")
  }
  else{
    aggrPlotTitle <- paste0("2020 UN-Adjusted Population Count \n for ", 
                            selectedCountry, 
                            " (1 sq. km resolution)")
  }
  
  gadmFileName <- paste0("gadm36_", toupper(inputISO), "_1_sp.rds")   # name of the .rds file 
  gadmFolder <- "gadm/"                                               # .rds files should be stored in local gadm/ folder
  
  # print(paste0(gadmFolder, gadmFileName))
  # print(getwd())
  
  level1Identifier <- readRDS(paste0(gadmFolder, gadmFileName))

  
  leaflet(width = 1024, 
          height = 768) %>%
    addRasterImage(x, 
                   colors = pal(8)[-1]) %>%
    addPolygons(data = level1Identifier,
                color = "#444444", 
                weight = 1.5, 
                smoothFactor = 1,
                opacity = 1.0, 
                fillOpacity = 0,
                popup = paste(level1Identifier$NAME_1),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE)) %>%
    addLegend(pal = colorBin(palette = pal(8)[-1],
                             bins = valueRange,
                             domain = valueRange),
              values = valueRange,
              opacity = 0.75,
              title = "Est. persons per pixel",
              position = "topright")
  
}