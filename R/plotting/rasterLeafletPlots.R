createLeafletPlot <- function(selectedCountryISO3C, level1Names, susceptible, gadmSpatVector) {
  if (!is.null(level1Names)) {
    GADMdata <- GADMdata[GADMdata$NAME_1 %in% c(level1Names), ]
    GADMdata <- vect(GADMdata)
    GAMDdata <- rast(GADMdata)

    crs(GADMdata) <- crs(susceptible, proj = TRUE)

    lvl1Raster <- crop(susceptible, GADMdata, mask = TRUE)

    lvl1Rasterrast <- lvl1Raster

    x <- classify(lvl1Raster, valueRange)
  } else {
    x <- classify(susceptible, valueRange)
  }

  levels(x) <- levels(x)[[1]]

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

  leaflet(width = 1024,
          height = 768,
          options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%
    addRasterImage(x,
                   colors = pal(8)[-1]) %>%
    addPolygons(data = GADMdata,
                color = "#444444",
                weight = 1.5,
                smoothFactor = 1,
                opacity = 1.0,
                fillOpacity = 0,
                popup = paste(GADMdata$NAME_1),
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

createLeafletBubblePlot <- function(selectedCountry, level1Names, plotData, activeCol) {
  inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c') #Converts country name to ISO Alpha

  gadmFileName <- paste0("gadm36_", toupper(inputISO), "_1_sp.rds")   # name of the .rds file
  gadmFolder <- "gadm/"
  level1Identifier <- readRDS(paste0(gadmFolder, gadmFileName))

  if(!is.null(level1Names)){
    level1Identifier <- level1Identifier[which(level1Identifier$NAME_1 %in% level1Names), ]}

  valueRange <- c(0, 5, 10, 25, 50, 100, 250, 1000, 10000)
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
    addLegend(pal = colorBin(palette = colorRampPalette(ramp)(9)[-1],
                             bins = valueRange,
                             domain = valueRange),
              values = valueRange,
              opacity = 0.75,
              title = "Obs. persons",
              position = "topright")

  leafletPlot
}
