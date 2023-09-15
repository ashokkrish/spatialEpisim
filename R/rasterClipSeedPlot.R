# rm(list = ls())
library(countrycode)
library(raster, warn.conflicts=FALSE)
#library(terra, warn.conflicts=FALSE) 

source("R/rasterWorldPop.R")

# selectedCountry <- "Democratic Republic of Congo"
# rasterAgg <- 10
# isCropped <- T
# level1Names <- c("Ituri", "Nord-Kivu")
# seedData <- "seeddata/COD_InitialSeedData.csv"
# seedNeighbourhood <- 1

createClippedSeedPlot <- function(selectedCountry, rasterAgg, isCropped, level1Names = NULL, seedData, seedNeighbourhood = 0) {
  
  inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c') # Converts country name to ISO Alpha
  
  Susceptible <- createSusceptibleLayer(selectedCountry, rasterAgg, isCropped, level1Names = NULL)$Susceptible
  
  print(Susceptible)
  print("Susceptible")

    #---------------------------------------#
    # Source 2: From GADM: Level1Identifier #
    #---------------------------------------#
    
    gadmFileName <- paste0("gadm36_", inputISO, "_1_sp.rds")  # name of the .rds file
    
    gadmFolder <- "gadm/"         # .rds files should be stored in local gadm/ folder
    
    #print(paste0(gadmFolder, gadmFileName))

    Level1Identifier <- readRDS(paste0(gadmFolder, gadmFileName))
    
    # print(Level1Identifier)
    # print(Level1Identifier$NAME_1) # List of all states/provinces/regions
    
    if (isCropped)
    {
    print("Seed plot for a smaller area")
    # positions <- which(Level1Identifier$NAME_1 %in% level1Names)  # Determines the position of which indices are TRUE.
    # print(positions)
    
    Level1Identifier <- Level1Identifier[which(Level1Identifier$NAME_1 %in% level1Names), ]
    # print(Level1Identifier) # It is a SpatialPolygonsDataFrame
    
    # Level1Identifier <- Level1Identifier[Level1Identifier$NAME_1 %in% level1Names, ]
    # print(Level1Identifier)
    
    #print(which(Level1Identifier$NAME_1 %in% level1Names)) # Assigns 1, 2, ...
    #print(table(Level1Identifier$NAME_1 %in% level1Names))
    
    Level1Raster <- crop(Level1Identifier, Susceptible)
    # print(Level1Raster) # It is still a SpatialPolygonsDataFrame
    
    Level1Raster <- raster(Level1Identifier, resolution = res(Susceptible)[1])
    # print(Level1Raster) # It is now a RasterLayer
    # print(values(Level1Raster))
    
    Level1Raster <- rasterize(Level1Identifier, Level1Raster)
    # print(Level1Raster) # It is now a RasterLayer
    # print(values(Level1Raster))
    
    # print(extent(Level1Raster))
    # Level1Raster <- replace(Level1Raster, is.na(Level1Raster), 0)
    
    # print(table(values(Level1Raster)))
    # print(freq(Level1Raster))
    
    # Resampling methods
    # "ngb": Nearest-neighbor; assigns the value of the nearest cell
    # "bilinear": Bilinear interpolation; assigns a weighted average of the four nearest cells (the default)
    
    # Level1Raster <- round(resample(Level1Raster, Susceptible, method = "bilinear"))
    # Level1Raster <-  round(resample(Level1Raster, Susceptible, method = "ngb", fun ='modal'))
    
    Level1Raster <- resample(Level1Raster, Susceptible, method = "ngb")
    
    values(Level1Raster) <- ifelse(values(Level1Raster) > 0, values(Level1Raster), 0) # Refill the rasterLayer with 0, 1, 2, 3, ....
    # print(table(values(Level1Raster)))
    # print(freq(Level1Raster))
    
    # Level1Raster <- replace(Level1Raster, values(Level1Raster) < 0, 0)
    # Unless you are using method = "ngb" the above line is needed for some countries.
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
    
    inhabitableTrim <- trim(Inhabitable, values = 0, padding = 1)
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
    
    rasterStack <- stack(Susceptible, Vaccinated, Exposed, Infected, Recovered, Dead, Inhabitable, Level1Raster)
    
    names(rasterStack) <- c("Susceptible", "Vaccinated", "Exposed", "Infected", "Recovered", "Dead", "Inhabitable", "Level1Raster")
    
    print("rasterStack before cropping")
    print(rasterStack)
    
    rasterStack <- crop(rasterStack, inhabitableTrim)
    
    print("rasterStack after cropping")
    print(rasterStack)
    
    clippedInfected <- rasterStack[["Infected"]] 
    
    print(clippedInfected)
    print("Clipped Infected Unseeded")

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
    
    # When seedNeighbourhood = 1 we seed the initial infections equitably in a Moore Neighborhood of cells
    numCellsPerRegion <- (2*seedNeighbourhood + 1)^2

    for (ff in 1:numLocations)
    { 
      print(paste("Seed location = ", seedData[ff,1]))
      
      row <- rowFromY(rasterStack, seedData[ff,2])
      col <- colFromX(rasterStack, seedData[ff,3])
      
      print(paste("row = ", row, "col = ", col)) 

      newInfPerCell <- seedData[ff,6]/numCellsPerRegion    #round(seedData[ff,4]/numCellsPerRegion)

      clippedInfected[(row-seedNeighbourhood):(row+seedNeighbourhood),(col-seedNeighbourhood):(col+seedNeighbourhood)] <- newInfPerCell
    }
    
    print(clippedInfected)
    print("Clipped Infected Seeded")
    
    #ramp <- c('#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
    ramp <- c('#FFFFFF', '#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
    pal <- colorRampPalette(ramp)
    
    # If you want a plot for the 28 reporting Health Zones set
    # seedNeighbourhood <- 0
    # seedData <- "seeddata/COD_InitialSeedData_28.csv"
    # main = "Location of Health Zones", 
    # legend = FALSE,

    plot(clippedInfected, col = pal(8)[-2], axes = T, cex.main = 1, main = "Location of inital infections (Ituri and North Kivu)", 
         xlab = expression(bold("Longitude")), ylab = expression(bold("Latitude")), 
         legend = TRUE, horizontal = TRUE, legend.args = list(text='Persons', side = 1, line = 2),
         mar = c(8.5, 3.5, 2.5, 2.5))
    # plg = list(title = expression(bold("Persons")), title.cex = 1, horiz=TRUE, x.intersp=0.6, inset=c(0, -0.2), cex=1.15), pax = list(cex.axis=1.15), 
    plot(Level1Identifier, add = TRUE)
    }
    else
    {
      print("Seed plot for the whole country")

      Level1Raster <- crop(Level1Identifier, Susceptible)
      # print(Level1Raster) # It is still a SpatialPolygonsDataFrame

      Level1Raster <- raster(Level1Identifier, resolution = res(Susceptible)[1])
      # print(Level1Raster) # It is now a RasterLayer
      # print(values(Level1Raster))

      Level1Raster <- rasterize(Level1Identifier, Level1Raster)
      # print(Level1Raster) # It is now a RasterLayer
      # print(values(Level1Raster))

      # print(extent(Level1Raster))
      # Level1Raster <- replace(Level1Raster, is.na(Level1Raster), 0)

      # print(table(values(Level1Raster)))
      # print(freq(Level1Raster))

      # Resampling methods
      # "ngb": Nearest-neighbor; assigns the value of the nearest cell
      # "bilinear": Bilinear interpolation; assigns a weighted average of the four nearest cells (the default)

      # Level1Raster <- round(resample(Level1Raster, Susceptible, method = "bilinear"))
      # Level1Raster <-  round(resample(Level1Raster, Susceptible, method = "ngb", fun ='modal'))

      Level1Raster <- resample(Level1Raster, Susceptible, method = "ngb")

      values(Level1Raster) <- ifelse(values(Level1Raster) > 0, values(Level1Raster), 0) # Refill the rasterLayer with 0, 1, 2, 3, ....
      # print(table(values(Level1Raster)))
      # print(freq(Level1Raster))

      # Level1Raster <- replace(Level1Raster, values(Level1Raster) < 0, 0)
      # Unless you are using method = "ngb" the above line is needed for some countries.
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

      rasterStack <- stack(Susceptible, Vaccinated, Exposed, Infected, Recovered, Dead, Inhabitable, Level1Raster)

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

      numCellsPerRegion <- (2*seedNeighbourhood + 1)^2

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
        # print(Inhabitable[(row-seedNeighbourhood):(row+seedNeighbourhood),(col-seedNeighbourhood):(col+seedNeighbourhood)])
        # print(sum(Inhabitable[(row-seedNeighbourhood):(row+seedNeighbourhood),(col-seedNeighbourhood):(col+seedNeighbourhood)]))

        newInfPerCell <- seedData[ff,6]/numCellsPerRegion    #round(seedData[ff,4]/numCellsPerRegion)

        rasterStack[["Infected"]][(row-seedNeighbourhood):(row+seedNeighbourhood),(col-seedNeighbourhood):(col+seedNeighbourhood)] <- newInfPerCell
        #print(paste("Susceptible = ", sum(values(Susceptible))))
      }

      print(rasterStack[["Infected"]])
      print("InfectedSeededWholeCountry")

      #ramp <- c('#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
      ramp <- c('#FFFFFF', '#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
      pal <- colorRampPalette(ramp)
      
      
      plot(rasterStack[["Infected"]], col = pal(8)[-2], axes = T, cex.main = 1, main = "Location of inital infections (Czechia)", 
           xlab = expression(bold("Longitude")), ylab = expression(bold("Latitude")), 
           legend = TRUE, horizontal = TRUE, legend.args = list(text='Persons', side = 1, line = 2),
           mar = c(8.5, 3.5, 2.5, 2.5))

      #plot(rasterStack[["Infected"]], col = pal(8)[-2], axes = TRUE, cex.main = 1, main = "Location of Initial Infections", legend=TRUE, horizontal = TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
      # plg = list(title = expression(bold("Persons")), title.cex = 1, horiz=TRUE, x.intersp=0.6, inset=c(0, -0.2), cex=1.15), pax = list(cex.axis=1.15),
      plot(Level1Identifier, add = TRUE)
    }
}

#------------------------#
# Example Function Calls #
#------------------------#

# Move the read.csv(paste0("seeddata/", seedData), header = T) in the example calls

 createClippedSeedPlot(selectedCountry = "Democratic Republic of Congo", rasterAgg = 10, isCropped = T, level1Names = c("Ituri", "Nord-Kivu"), seedData = "seeddata/COD_InitialSeedData.csv", seedNeighbourhood = 1)

# createClippedSeedPlot(selectedCountry = "Democratic Republic of Congo", rasterAgg = 10, isCropped = F, level1Names = NULL, seedData = "seeddata/COD_InitialSeedData.csv", seedNeighbourhood = 1)

# createClippedSeedPlot(selectedCountry = "Democratic Republic of Congo", rasterAgg = 5, isCropped = T, level1Names = c("Ituri", "Nord-Kivu"), seedData = "seeddata/COD_InitialSeedData_28.csv", seedNeighbourhood = 0)

 createClippedSeedPlot(selectedCountry = "Czech Republic", rasterAgg = 10, isCropped = F, level1Names = NULL, seedData = "seeddata/CZE_InitialSeedDataSep 1, 2020.csv", seedNeighbourhood = 1)

# createClippedSeedPlot(selectedCountry = "Nigeria", rasterAgg = 5, isCropped = T, level1Names = c("Kwara", "Oyo"), seedData = "seeddata/NGA_InitialSeed_Oyo_Kwara.csv", seedNeighbourhood = 1)

# createClippedSeedPlot(selectedCountry = "Korea", rasterAgg = 5, isCropped = F, level1Names = NULL, seedData = "seeddata/KOR_InitialSeedData2022-07-07.csv", seedNeighbourhood = 1)

# createClippedSeedPlot(selectedCountry = "Nigeria", rasterAgg = 15, isCropped = F, level1Names = NULL, seedData = "seeddata/NGA_InitialSeedDataSep 1, 2020.csv", seedNeighbourhood = 1)