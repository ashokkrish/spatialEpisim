library(countrycode)
library(terra)

createRasterStack <- function(selectedCountry, rasterAgg, isCropped = F, level1Names = NULL) {

  inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c') # Converts country name to ISO Alpha

  source("R/rasterWorldPop.R")
  
  SusceptibleLayer <- createSusceptibleLayer(selectedCountry, rasterAgg, isCropped, level1Names)
  Susceptible <- SusceptibleLayer$Susceptible
  
  
  
  # print(Susceptible)
    
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
    # positions <- which(Level1Identifier$NAME_1 %in% level1Names)  # Determines the position of which indices are TRUE.
    # print(positions)

    Level1Identifier <- Level1Identifier[which(Level1Identifier$NAME_1 %in% level1Names), ]
    # print(Level1Identifier) # It is a SpatialPolygonsDataFrame
    
    # Level1Identifier <- Level1Identifier[Level1Identifier$NAME_1 %in% level1Names, ]
    # print(Level1Identifier)

    # print(which(Level1Identifier$NAME_1 %in% level1Names)) # Assigns 1, 2, ...
    # print(table(Level1Identifier$NAME_1 %in% level1Names))
    
    Level1Identifier <- vect(Level1Identifier)
    crs(Level1Identifier) <- crs(Susceptible, proj = TRUE)
    # convert Level1Identifier into a spatVector
    
    Level1Raster <- crop(Level1Identifier, Susceptible)
    # print(Level1Raster) # It is still a SpatialPolygonsDataFrame

    Level1Raster <- rast(Level1Identifier, resolution = res(Susceptible)[1])
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

    Level1Raster <- resample(Level1Raster, Susceptible, method = "near")

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
    
    inhabitableTrim <- trim(Inhabitable, value = 0)
    # print(extent(inhabitableTrim))
    
    # inhabitableRows <- inhabitableCols <-
    # 
    # # print(Inhabitable[1][1])
    # #   
    # # for (i in seq_len(nrow(Inhabitable))) {
    # #   for (j in seq_len(ncol(Inhabitable))){
    # #     if (Inhabitable[i][j] == 1) {
    # #       append(inhabitableRows, i)
    # #       append(inhabitableCols, j)
    # #     }
    # #   }
    # # }
    # # 
    # # minRow = min(inhabitableRows)
    # # maxRow = max(inhabitableRows)
    # # minCol = min(inhabitableCols)
    # # maxCol = max(inhabitableCols)
    # # 
    # print(minRow)
    # print(maxRow)
    # print(minCol)
    # print(maxCol)
    
    # print("isCropped selected")
    
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

    rasterStack <- crop(rasterStack, inhabitableTrim)

    # croppedSusceptible <- crop(Susceptible, inhabitableTrim)
    # 
    # croppedIdentifier <- crop(Level1Raster, inhabitableTrim)
    #
    # writeRaster(croppedSusceptible, "croppedSusceptible", format = "GTiff", overwrite = TRUE) # the tif file may not be at 1km resolution
    # 
    # writeRaster(croppedIdentifier, "croppedIdentifier", format = "GTiff", overwrite = TRUE) # the tif file may not be at 1km resolution
  }
  else
  {
    Level1Identifier <- vect(Level1Identifier)
    crs(Level1Identifier) <- crs(Susceptible, proj = TRUE)
    # convert Level1Identifier into a spatVector
    Level1Raster <- crop(Level1Identifier, Susceptible)
    # print(Level1Raster) # It is still a SpatialPolygonsDataFrame
    
    Level1Raster <- rast(Level1Identifier, resolution = res(Susceptible)[1])
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
    
    Level1Raster <- resample(Level1Raster, Susceptible, method = "near")
    
    values(Level1Raster) <- ifelse(values(Level1Raster) > 0, values(Level1Raster), 0) # Refill the rasterLayer with 0, 1, 2, 3, ....
    # print(table(values(Level1Raster)))
    # print(freq(Level1Raster))
    
    # Level1Raster <- replace(Level1Raster, values(Level1Raster) < 0, 0)
    # Unless you are using method = "ngb" the above line is needed for some countries.
    # The other method is called "bilinear"
    
    Level1Raster <- replace(Level1Raster, is.na(Level1Raster), 0)
    # print(table(values(Level1Raster)))
    # print(freq(Level1Raster))
    # print(values(Level1Raster))
    
    # Background: Aggregating typically an entire column or an entire row or both worth of NAs are added to the Level1Raster
    # NOTE: If rasterAgg = 0 or 1, no NAs are added.
    
    values(Susceptible) <- ifelse(values(Level1Raster) > 0, values(Susceptible), 0) # Fill the Susceptible Layer with either a 0 or it's actual susceptible.
    
    Inhabitable <- Vaccinated <- Exposed <- Infected <- Recovered <- Dead <- Susceptible
    
    values(Vaccinated) <- values(Exposed) <- values(Infected) <- values(Recovered) <- values(Dead) <- 0 # Fill the entire rasterLayer with zeroes
    values(Inhabitable) <- ifelse(values(Susceptible) > 0, 1, 0) # Fill the rasterLayer with either a 0 or 1.

    # print("isCropped not selected")
    
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
  }

  names(rasterStack) <- c("Susceptible", "Vaccinated", "Exposed", "Infected", "Recovered", "Dead", "Inhabitable", "Level1Raster")
  
  #print(rasterStack)

  returnList <- list("rasterStack" = rasterStack, "Level1Identifier" = Level1Identifier, "selectedCountry" = selectedCountry, "rasterAgg" = rasterAgg, "nRows" = SusceptibleLayer$nRows, "nCols" = SusceptibleLayer$nCols, "nCells" = SusceptibleLayer$nCells)
  
  return(returnList)
}

#------------------------#
# Example Function Calls #
#------------------------#
# To test this function set working directory to the root folder
# 
# createRasterStack("Democratic Republic of Congo", rasterAgg = 10, isCropped = T, level1Names = c("Ituri", "Nord-Kivu"))
# createRasterStack("Nigeria", rasterAgg = 0, isCropped = T, level1Names = "Oyo")
# createRasterStack("Czech Republic", rasterAgg = 0, isCropped = T, level1Names = c("Prague","Zlínský"))
# 
# createRasterStack("Nigeria", rasterAgg = 25, isCropped = F, level1Names = NULL)
# createRasterStack("Italy", rasterAgg = 30, isCropped = F, level1Names = NULL)
# createRasterStack("Latvia", rasterAgg = 0, isCropped = F, level1Names = NULL)
# createRasterStack("Uganda", rasterAgg = 0, isCropped = F, level1Names = NULL)
#
# rs <- createRasterStack("Czech Republic", 10, isCropped = F, level1Names = NULL)
# rs
# names(rs)
# rs$rasterStack$Susceptible