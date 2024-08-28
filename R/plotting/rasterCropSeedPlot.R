createCroppedSeedPlot <- function(selectedCountry, isCropped, level1Names = NULL, susceptibleLayer, uploadedSeedDataPath, seedRadius = 0) {
  inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c') # Converts country name to ISO Alpha

  Susceptible <- susceptibleLayer

  if (isCropped) {
    gadmSpatVector <- gadmSpatVector[which(gadmSpatVector$NAME_1 %in% level1Names), ]

    gadmSpatVector <- vect(gadmSpatVector)
    crs(gadmSpatVector) <- crs(Susceptible, proj = TRUE)

    Level1Raster <- crop(gadmSpatVector, Susceptible)

    Level1Raster <- rast(gadmSpatVector, resolution = res(Susceptible)[1])
    Level1Raster <- rasterize(gadmSpatVector, Level1Raster)
    Level1Raster <- resample(Level1Raster, Susceptible, method = "near")

    values(Level1Raster) <- ifelse(values(Level1Raster) > 0, values(Level1Raster), 0)
    Level1Raster <- replace(Level1Raster, is.na(Level1Raster), 0)
    values(Susceptible) <- ifelse(values(Level1Raster) > 0, values(Susceptible), 0)
    Inhabitable <- Vaccinated <- Exposed <- Infected <- Recovered <- Dead <- Susceptible
    values(Vaccinated) <- values(Exposed) <- values(Infected) <- values(Recovered) <- values(Dead) <- 0
    values(Inhabitable) <- ifelse(values(Susceptible) > 0, 1, 0)
    inhabitableTrim <- terra::trim(Inhabitable, value = 0)
    rasterStack <- c(Susceptible, Vaccinated, Exposed, Infected, Recovered, Dead, Inhabitable, Level1Raster)
    names(rasterStack) <- c("Susceptible", "Vaccinated", "Exposed", "Infected", "Recovered", "Dead", "Inhabitable", "Level1Raster")
    rasterStack <- crop(rasterStack, inhabitableTrim)
    croppedInfected <- rasterStack[["Infected"]]
    seedData <- read.csv(uploadedSeedDataPath, header = T)
    numLocations <- dim(seedData)[1]
    numCellsPerRegion <- (2*seedRadius + 1)^2

    for (ff in 1:numLocations)
    {
      row <- rowFromY(rasterStack, seedData[ff,2])
      col <- colFromX(rasterStack, seedData[ff,3])
      newInfPerCell <- seedData[ff,6]/numCellsPerRegion    #round(seedData[ff,4]/numCellsPerRegion)
      croppedInfected[(row-seedRadius):(row+seedRadius),(col-seedRadius):(col+seedRadius)] <- newInfPerCell
    }
  } else {
    print("Seed plot for the whole country")

    gadmSpatVector <- vect(gadmSpatVector)
    crs(gadmSpatVector) <- crs(Susceptible, proj = TRUE)

    Level1Raster <- crop(gadmSpatVector, Susceptible)
    Level1Raster <- rast(gadmSpatVector, resolution = res(Susceptible)[1])
    Level1Raster <- rasterize(gadmSpatVector, Level1Raster)
    Level1Raster <- resample(Level1Raster, Susceptible, method = "near")
    values(Level1Raster) <- ifelse(values(Level1Raster) > 0, values(Level1Raster), 0)
    Level1Raster <- replace(Level1Raster, is.na(Level1Raster), 0)
    values(Susceptible) <- ifelse(values(Level1Raster) > 0, values(Susceptible), 0)
    Inhabitable <- Vaccinated <- Exposed <- Infected <- Recovered <- Dead <- Susceptible
    values(Vaccinated) <- values(Exposed) <- values(Infected) <- values(Recovered) <- values(Dead) <- 0
    values(Inhabitable) <- ifelse(values(Susceptible) > 0, 1, 0)
    rasterStack <- c(Susceptible, Vaccinated, Exposed, Infected, Recovered, Dead, Inhabitable, Level1Raster)
    names(rasterStack) <- c("Susceptible", "Vaccinated", "Exposed", "Infected", "Recovered", "Dead", "Inhabitable", "Level1Raster")
    ULCornerLongitude <- xmax(rasterStack[["Infected"]])
    ULCornerLatitude <- ymax(rasterStack[["Infected"]])
    LLCornerLongitude <- xmin(rasterStack[["Infected"]])
    LLCornerLatitude <- ymin(rasterStack[["Infected"]])
    hcellSize <- res(rasterStack[["Infected"]])[1]
    vcellSize <- res(rasterStack[["Infected"]])[2]
    midLongitude <-(LLCornerLongitude + ULCornerLongitude)/2
    midCol <- trunc(abs((midLongitude - (ULCornerLongitude-hcellSize/2))/hcellSize)) + 1
    seedData <- read.csv(uploadedSeedDataPath, header = T)
    numLocations <- dim(seedData)[1]
    numLocations <- dim(seedData)[1] #nrow(data())
    numCellsPerRegion <- (2*seedRadius + 1)^2
    for (ff in 1:numLocations) {
      row <- trunc(abs((seedData[ff,2] - (ULCornerLatitude+vcellSize/2))/vcellSize)) + 1
      col <- trunc(abs((seedData[ff,3] - (ULCornerLongitude-hcellSize/2))/hcellSize)) + 1
      col <- midCol - (col - midCol)

      if (selectedCountry == "Nigeria") {
        row <- row - 1
        col <- col + 2 # Additional correction needed for Nigeria
      }

      newInfPerCell <- seedData[ff, 6] / numCellsPerRegion
      rasterStack[["Infected"]][(row-seedRadius):(row+seedRadius),(col-seedRadius):(col+seedRadius)] <- newInfPerCell
    }

    createLeafletPlot(selectedCountry, level1Names, infectedData)
  }
}

printCroppedBubbleSeedPlot <- function(selectedCountry, dataPath, level1Names = NULL, activeCol) {
  inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c')
  gadmFileName <- paste0("gadm36_", inputISO, "_1_sp.rds")  # name of the .rds file
  gadmFolder <- "gadm/"         # .rds files should be stored in local gadm/ folder
  gadmSpatVector <- readRDS(paste0(gadmFolder, gadmFileName))

  if(!is.null(level1Names)){
    gadmSpatVector <- gadmSpatVector[which(gadmSpatVector$NAME_1 %in% level1Names), ]}

  seedData <- read.csv(dataPath, header = T)

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
  valueRange <- c(0, 5, 10, 25, 50, 100, 250, 1000)
  colorPalette <- colorBin(pal(8)[-1], domain = valueRange, bins = valueRange)
  colnames(seedData)[activeCol] <- "Current"

  labelText <- paste0(
    "Location: ", seedData$Location, "<br/>",
    "Count: ", seedData$Current, "<br/>") %>%
    lapply(htmltools::HTML)

  leaflet(seedData,
          width = 1920,
          height = 1080,
          options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%
    addPolygons(data = gadmSpatVector,
                color = "#444444",
                weight = 1.5,
                smoothFactor = 1,
                opacity = 1.0,
                fillColor = "#F5F5F5",
                fillOpacity = 0.75,
                popup = paste(gadmSpatVector$NAME_1),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = FALSE)) %>%
    addLegend(pal = colorBin(palette = pal(8)[-1],
                             bins = valueRange,
                             domain = valueRange),
              values = valueRange,
              opacity = 0.75,
              title = "Obs. persons",
              position = "topright") %>%
    addCircles(lng = ~lon,
               lat = ~lat,
               radius = 5000,
               weight = 1,
               opacity = 1,
               color = ~ifelse(Current > 0, "black", "transparent"),
               fillColor = ~ifelse(Current > 0, colorPalette(Current), "transparent"),
               fillOpacity = 0.8,
               popup = labelText,
               label = labelText)
}
