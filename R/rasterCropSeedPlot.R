# rm(list = ls())
library(countrycode)
library(terra, warn.conflicts=FALSE)

source("R/rasterWorldPop.R")

# selectedCountry <- "Democratic Republic of Congo"
# rasterAgg <- 10
# isCropped <- T
# level1Names <- c("Ituri", "Nord-Kivu")
# seedData <- "seeddata/COD_InitialSeedData.csv"
# seedRadius <- 1

createCroppedSeedPlot <- function(selectedCountry, isCropped, level1Names = NULL, susceptibleLayer, seedData, seedRadius = 0) {
  
  inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c') # Converts country name to ISO Alpha
  
  Susceptible <- susceptibleLayer
  

    #---------------------------------------#
    # Source 2: From GADM: level1Identifier #
    #---------------------------------------#
    
    gadmFileName <- paste0("gadm36_", inputISO, "_1_sp.rds")  # name of the .rds file
    
    gadmFolder <- "gadm/"         # .rds files should be stored in local gadm/ folder
    
    #print(paste0(gadmFolder, gadmFileName))

    level1Identifier <- readRDS(paste0(gadmFolder, gadmFileName))
    
    # print(level1Identifier)
    # print(level1Identifier$NAME_1) # List of all states/provinces/regions
    
    if (isCropped)
    {
      print("Seed plot for a smaller area")
      # positions <- which(level1Identifier$NAME_1 %in% level1Names)  # Determines the position of which indices are TRUE.
      # print(positions)
      
      level1Identifier <- level1Identifier[which(level1Identifier$NAME_1 %in% level1Names), ]
      # print(level1Identifier) # It is a SpatialPolygonsDataFrame
    
      level1Identifier <- vect(level1Identifier)
      crs(level1Identifier) <- crs(Susceptible, proj = TRUE)
      # level1Identifier <- level1Identifier[level1Identifier$NAME_1 %in% level1Names, ]
      # print(level1Identifier)
    
      #print(which(level1Identifier$NAME_1 %in% level1Names)) # Assigns 1, 2, ...
      #print(table(level1Identifier$NAME_1 %in% level1Names))
    
      Level1Raster <- crop(level1Identifier, Susceptible)
      # print(Level1Raster) # It is still a SpatialPolygonsDataFrame
    
      Level1Raster <- rast(level1Identifier, resolution = res(Susceptible)[1])
      # print(Level1Raster) # It is now a RasterLayer
      # print(values(Level1Raster))
    
      Level1Raster <- rasterize(level1Identifier, Level1Raster)
      # print(Level1Raster) # It is now a RasterLayer
      # print(values(Level1Raster))
    
      # print(extent(Level1Raster))
      # Level1Raster <- replace(Level1Raster, is.na(Level1Raster), 0)
      
      # print(table(values(Level1Raster)))
      # print(freq(Level1Raster))
    
      # Resampling methods
      # "near": Nearest-neighbor; assigns the value of the nearest cell
      # "bilinear": Bilinear interpolation; assigns a weighted average of the four nearest cells (the default)
    
      # Level1Raster <- round(resample(Level1Raster, Susceptible, method = "bilinear"))
      # Level1Raster <-  round(resample(Level1Raster, Susceptible, method = "near", fun ='modal'))
    
      Level1Raster <- resample(Level1Raster, Susceptible, method = "near")
    
      values(Level1Raster) <- ifelse(values(Level1Raster) > 0, values(Level1Raster), 0) # Refill the rasterLayer with 0, 1, 2, 3, ....
      # print(table(values(Level1Raster)))
      # print(freq(Level1Raster))
      
      # Level1Raster <- replace(Level1Raster, values(Level1Raster) < 0, 0)
      # Unless you are using method = "near" the above line is needed for some countries.
      # The other method is called "bilinear"
    
      Level1Raster <- replace(Level1Raster, is.na(Level1Raster), 0)
      # print(table(values(Level1Raster)))
      # print(freq(Level1Raster))
    
      # Background: Aggregating typically an entire column or an entire row or both worth of NAs are added to the Level1Raster
      # NOTE: If rasterAgg = 0 or 1, no NAs are added.
    
      values(Susceptible) <- ifelse(values(Level1Raster) > 0, values(Susceptible), 0) # Fill the Susceptible Layer with either a 0 or it's actual susceptible.
    
      Inhabitable <- Vaccinated <- Exposed <- Infected <- Recovered <- Dead <- Susceptible
    
      values(Vaccinated) <- values(Exposed) <- values(Infected) <- values(Recovered) <- values(Dead) <- 0 # Fill the entire rasterLayer with zeroes
      values(Inhabitable) <- ifelse(values(Susceptible) > 0, 1, 0) # Fill the rasterLayer with either a 0 or 1.
    
      inhabitableTrim <- terra::trim(Inhabitable, value = 0)
      #print(inhabitableTrim)
    
      #print(table(as.matrix(Inhabitable)))
      #print(table(as.matrix(Vaccinated)))
      
      # print(freq(Level1Raster)) # Frequency table of the values of a RasterLayer.
      # print(freq(Inhabitable))
      
      # print(dim(Level1Raster)); print(dim(Susceptible))
      # print(res(Level1Raster)); print(res(Susceptible))
      # print(origin(Level1Raster)); print(origin(Susceptible))
      
      # print(extent(Level1Raster))
      # print(extent(Susceptible))
      
      rasterStack <- c(Susceptible, Vaccinated, Exposed, Infected, Recovered, Dead, Inhabitable, Level1Raster)
      
      names(rasterStack) <- c("Susceptible", "Vaccinated", "Exposed", "Infected", "Recovered", "Dead", "Inhabitable", "Level1Raster")
      
      print("rasterStack before cropping")
      print(rasterStack)
      
      rasterStack <- crop(rasterStack, inhabitableTrim)
      
      print("rasterStack after cropping")
      print(rasterStack)
      
      croppedInfected <- rasterStack[["Infected"]] 
    
      print(croppedInfected)
      print("Cropped Infected Unseeded")

      #------------------------#
      # Initial seed locations #
      #------------------------#
      
      # if (missing(seedData)){
      #   seedFolder <- "seeddata/"         # .csv or .xlsx files may be stored in local seeddata/ folder
      #   seedData <<- read_excel(paste0(seedFolder, inputISO, "_InitialSeedData.csv"), header = T)
      #   seedData <<- read_excel(paste0(seedFolder, inputISO, "_InitialSeedData.xlsx"), 1, header = T)
      # } else {
        seedData <- read.csv(seedData, header = T)
      # }
      
      print(seedData)

      numLocations <- dim(seedData)[1]
    
      print(numLocations)
    
      # When seedRadius = 1 we seed the initial infections equitably in a Moore Neighborhood of cells
      numCellsPerRegion <- (2*seedRadius + 1)^2

      for (ff in 1:numLocations)
      { 
        print(paste("Seed location = ", seedData[ff,1]))
        
        row <- rowFromY(rasterStack, seedData[ff,2])
        col <- colFromX(rasterStack, seedData[ff,3])
      
        print(paste("row = ", row, "col = ", col)) 

        newInfPerCell <- seedData[ff,6]/numCellsPerRegion    #round(seedData[ff,4]/numCellsPerRegion) 
  
        croppedInfected[(row-seedRadius):(row+seedRadius),(col-seedRadius):(col+seedRadius)] <- newInfPerCell
      }
      
      print(croppedInfected)
      print("Cropped Infected Seeded")
    
      printCroppedSeedPlot(croppedInfected, level1Identifier)
    }
    else
    {
      print("Seed plot for the whole country")
      
      level1Identifier <- vect(level1Identifier)
      crs(level1Identifier) <- crs(Susceptible, proj = TRUE)

      Level1Raster <- crop(level1Identifier, Susceptible)
      # print(Level1Raster) # It is still a SpatialPolygonsDataFrame

      Level1Raster <- rast(level1Identifier, resolution = res(Susceptible)[1])
      # print(Level1Raster) # It is now a RasterLayer
      # print(values(Level1Raster))

      Level1Raster <- rasterize(level1Identifier, Level1Raster)
      # print(Level1Raster) # It is now a RasterLayer
      # print(values(Level1Raster))

      # print(extent(Level1Raster))
      # Level1Raster <- replace(Level1Raster, is.na(Level1Raster), 0)

      # print(table(values(Level1Raster)))
      # print(freq(Level1Raster))

      # Resampling methods
      # "near": Nearest-neighbor; assigns the value of the nearest cell
      # "bilinear": Bilinear interpolation; assigns a weighted average of the four nearest cells (the default)

      # Level1Raster <- round(resample(Level1Raster, Susceptible, method = "bilinear"))
      # Level1Raster <-  round(resample(Level1Raster, Susceptible, method = "near", fun ='modal'))

      Level1Raster <- resample(Level1Raster, Susceptible, method = "near")

      values(Level1Raster) <- ifelse(values(Level1Raster) > 0, values(Level1Raster), 0) # Refill the rasterLayer with 0, 1, 2, 3, ....
      # print(table(values(Level1Raster)))
      # print(freq(Level1Raster))

      # Level1Raster <- replace(Level1Raster, values(Level1Raster) < 0, 0)
      # Unless you are using method = "near" the above line is needed for some countries.
      # The other method is called "bilinear"

      Level1Raster <- replace(Level1Raster, is.na(Level1Raster), 0)
      # print(table(values(Level1Raster)))
      # print(freq(Level1Raster))

      # Background: Aggregating typically an entire column or an entire row or both worth of NAs are added to the Level1Raster
      # NOTE: If rasterAgg = 0 or 1, no NAs are added.

      values(Susceptible) <- ifelse(values(Level1Raster) > 0, values(Susceptible), 0) # Fill the Susceptible Layer with either a 0 or it's actual susceptible.

      Inhabitable <- Vaccinated <- Exposed <- Infected <- Recovered <- Dead <- Susceptible

      values(Vaccinated) <- values(Exposed) <- values(Infected) <- values(Recovered) <- values(Dead) <- 0 # Fill the entire rasterLayer with zeroes
      values(Inhabitable) <- ifelse(values(Susceptible) > 0, 1, 0) # Fill the rasterLayer with either a 0 or 1.

      # print(table(as.matrix(Inhabitable)))
      # print(table(as.matrix(Vaccinated)))

      # print(freq(Level1Raster)) # Frequency table of the values of a RasterLayer.
      # print(freq(Inhabitable))

      # print(dim(Level1Raster)); print(dim(Susceptible))
      # print(res(Level1Raster)); print(res(Susceptible))
      # print(origin(Level1Raster)); print(origin(Susceptible))

      # print(extent(Level1Raster))
      # print(extent(Susceptible))

      rasterStack <- c(Susceptible, Vaccinated, Exposed, Infected, Recovered, Dead, Inhabitable, Level1Raster)

      names(rasterStack) <- c("Susceptible", "Vaccinated", "Exposed", "Infected", "Recovered", "Dead", "Inhabitable", "Level1Raster")

      print(rasterStack)

      print("InfectedUnseeded")
      print(rasterStack[["Infected"]])
      print("InfectedUnseeded")

      ULCornerLongitude <- xmax(rasterStack[["Infected"]])
      ULCornerLatitude <- ymax(rasterStack[["Infected"]])

      LLCornerLongitude <- xmin(rasterStack[["Infected"]])
      LLCornerLatitude <- ymin(rasterStack[["Infected"]])

      print(c(ULCornerLongitude, ULCornerLatitude, LLCornerLongitude, LLCornerLatitude))

      hcellSize <- res(rasterStack[["Infected"]])[1]
      vcellSize <- res(rasterStack[["Infected"]])[2]

      midLongitude <-(LLCornerLongitude + ULCornerLongitude)/2
      midCol <- trunc(abs((midLongitude - (ULCornerLongitude-hcellSize/2))/hcellSize)) + 1

      seedData <- read.csv(seedData, header = T)
      # }
      
      print(seedData)
      
      numLocations <- dim(seedData)[1]

      print(seedData)

      numLocations <- dim(seedData)[1] #nrow(data())
      
      print(numLocations)

      numCellsPerRegion <- (2*seedRadius + 1)^2

      for (ff in 1:numLocations)
      {
        #print(paste("Seed location = ", seedData[ff,9]))

        row <- trunc(abs((seedData[ff,2] - (ULCornerLatitude+vcellSize/2))/vcellSize)) + 1
        col <- trunc(abs((seedData[ff,3] - (ULCornerLongitude-hcellSize/2))/hcellSize)) + 1

        col <- midCol - (col - midCol)
        row <- row   # reflecting along middle vertical line of plot so that data is in the right place.

        if(selectedCountry == "Nigeria"){
          row <- row - 1
          col <- col + 2 # Additional correction needed for Nigeria
        }

        print(paste("row = ", row, "col = ", col))
        # print(Inhabitable[(row-seedRadius):(row+seedRadius),(col-seedRadius):(col+seedRadius)])
        # print(sum(Inhabitable[(row-seedRadius):(row+seedRadius),(col-seedRadius):(col+seedRadius)]))

        newInfPerCell <- seedData[ff,6]/numCellsPerRegion    #round(seedData[ff,4]/numCellsPerRegion)

        rasterStack[["Infected"]][(row-seedRadius):(row+seedRadius),(col-seedRadius):(col+seedRadius)] <- newInfPerCell
        #print(paste("Susceptible = ", sum(values(Susceptible))))
      }

      print(rasterStack[["Infected"]])
      print("InfectedSeededWholeCountry")

      printCroppedSeedPlot(rasterStack[["Infected"]], level1Identifier)
    }
}

printCroppedSeedPlot <- function(infectedData, level1Identifier) {
  
  #ramp <- c('#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
  ramp <- c('#FFFFFF', '#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
  pal <- colorRampPalette(ramp)
  
  # If you want a plot for the 28 reporting Health Zones set
  # seedRadius <- 0
  # seedData <- "seeddata/COD_InitialSeedData_28.csv"
  # main = "Location of Health Zones", 
  # legend = FALSE,
  
  terra::plot(infectedData, 
              col = pal(8)[-2], 
              axes = TRUE, 
              cex.main = 1, 
              main = "Location of initial infections", 
              xlab = expression(bold("Longitude")), 
              ylab = expression(bold("Latitude")), 
              plg = list(title = "Persons", 
                         loc = "bottom", 
                         horiz = TRUE,
                         yjust = 3.5, 
                         x.intersp = 0.6, 
                         inset = c(0, -0.2), 
                         cex = 1.15),
              mar = c(8.5, 3.5, 4, 2.5))
  
  #plot(rasterStack[["Infected"]], col = pal(8)[-2], axes = TRUE, cex.main = 1, main = "Location of Initial Infections", legend=TRUE, horizontal = TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
  # plg = list(title = expression(bold("Persons")), title.cex = 1, horiz=TRUE, x.intersp=0.6, inset=c(0, -0.2), cex=1.15), pax = list(cex.axis=1.15),
  plot(level1Identifier, 
       add = TRUE)#ramp <- c('#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
}

#------------------------#
# Example Function Calls #
#------------------------#

# Move the read.csv(paste0("seeddata/", seedData), header = T) in the example calls

# createCroppedSeedPlot(selectedCountry = "Democratic Republic of Congo", rasterAgg = 10, isCropped = T, level1Names = c("Ituri", "Nord-Kivu"), seedData = "seeddata/COD_InitialSeedData.csv", seedRadius = 1)

# createCroppedSeedPlot(selectedCountry = "Democratic Republic of Congo", rasterAgg = 10, isCropped = F, level1Names = NULL, seedData = "seeddata/COD_InitialSeedData.csv", seedRadius = 1)

# createCroppedSeedPlot(selectedCountry = "Democratic Republic of Congo", rasterAgg = 5, isCropped = T, level1Names = c("Ituri", "Nord-Kivu"), seedData = "seeddata/COD_InitialSeedData_28.csv", seedRadius = 0)

# createCroppedSeedPlot(selectedCountry = "Czech Republic", rasterAgg = 10, isCropped = F, level1Names = NULL, seedData = "seeddata/CZE_InitialSeedDataSep 1, 2020.csv", seedRadius = 1)

# createCroppedSeedPlot(selectedCountry = "Nigeria", rasterAgg = 5, isCropped = T, level1Names = c("Kwara", "Oyo"), seedData = "seeddata/NGA_InitialSeed_Oyo_Kwara.csv", seedRadius = 1)

# createCroppedSeedPlot(selectedCountry = "Korea", rasterAgg = 5, isCropped = F, level1Names = NULL, seedData = "seeddata/KOR_InitialSeedData2022-07-07.csv", seedRadius = 1)

# createCroppedSeedPlot(selectedCountry = "Nigeria", rasterAgg = 15, isCropped = F, level1Names = NULL, seedData = "seeddata/NGA_InitialSeedDataSep 1, 2020.csv", seedRadius = 1)