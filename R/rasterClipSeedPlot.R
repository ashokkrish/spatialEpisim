#rm(list = ls())
library(countrycode)
library(raster, warn.conflicts=FALSE)
#library(terra) 

createClippedSeedPlot<- function(selectedCountry, rasterAgg, level1Names = NULL, seedData, radius = 0) {
  
  #----------------------------------------------------------------#
  # Source 1: WorldPop UN-Adjusted Population Count GeoTIFF raster #
  #----------------------------------------------------------------#
  
  inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c') #Converts country name to ISO Alpha
  inputISOLower <- tolower(inputISO)
  
  url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/2020/", inputISO, "/", inputISOLower, "_ppp_2020_1km_Aggregated_UNadj.tif")
  
  tifFileName <- basename(url)    # name of the .tif file
  tifFolder <- "tif/"             # .tif files should be stored in local tif/ folder
  
  if (!file.exists(paste0(tifFolder, tifFileName)))
  {
    download.file(url, paste0(tifFolder, tifFileName), mode = "wb")
  }
  
  #print(paste0(tifFolder, tifFileName))
  
  WorldPop <- raster(paste0(tifFolder, tifFileName))
  
  WorldPop <- replace(WorldPop, is.na(WorldPop), 0) # Delete this line for clear plot. Check!!!
  
  # print(WorldPop)
  # print(nrow(WorldPop))
  # print(ncol(WorldPop))
  # print(ncell(WorldPop))
  
  if (rasterAgg == 0 || rasterAgg == 1) {
    Susceptible <- WorldPop
  } else {
    Susceptible <- aggregate(WorldPop, fact = c(rasterAgg, rasterAgg), fun = sum, na.rm = TRUE)
  }
  
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
    
    #print(extent(Level1Raster))
    #Level1Raster <- replace(Level1Raster, is.na(Level1Raster), 0)
    
    #print(table(values(Level1Raster)))
    #(freq(Level1Raster))
    
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
    
    print(rasterStack)
    
    rasterStack <- crop(rasterStack, inhabitableTrim)
    
    print(rasterStack)
    
    clippedInfected <- rasterStack[["Infected"]] 
    
    print(clippedInfected)
    
    my_df <- read.csv(paste0("seeddata/", seedData), header = T)
    
    #print(my_df)
    
    ULCornerLongitude <- xmax(clippedInfected)
    ULCornerLatitude <- ymax(clippedInfected)
    
    LLCornerLongitude <- xmin(clippedInfected)
    LLCornerLatitude <- ymin(clippedInfected)
    
    #print(c(ULCornerLongitude, ULCornerLatitude, LLCornerLongitude, LLCornerLatitude))
    
    hcellSize <- res(clippedInfected)[1]
    vcellSize <- res(clippedInfected)[2]
    
    numLocations <- dim(my_df)[1] #nrow(data())
    
    #print(numLocations)

    for (ff in 1:numLocations)
    {
      #print(paste("Region Identifier = ", seedData[ff,9]))
      
      row <- trunc(abs((my_df[ff,2] - (ULCornerLatitude+vcellSize/2))/vcellSize)) + 1
      col <- trunc(abs((my_df[ff,3] - (ULCornerLongitude-hcellSize/2))/hcellSize)) + 1
      
      # print(paste("row = ", row, "col = ", col))
      # print(Inhabitable[(row-radius):(row+radius),(col-radius):(col+radius)])
      # print(sum(Inhabitable[(row-radius):(row+radius),(col-radius):(col+radius)]))
      
      numCellsPerRegion    <- (2*radius + 1)^2
      newInfPerCell        <- my_df[ff,6]/numCellsPerRegion    #round(seedData[ff,4]/numCellsPerRegion)

      clippedInfected[(row-radius):(row+radius),(col-radius):(col+radius)] <- Infected[(row-radius):(row+radius),(col-radius):(col+radius)] + newInfPerCell

      #print(paste("Susceptible = ", sum(values(Susceptible))))
    }
    
    print(clippedInfected)
    
    #ramp <- c('#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
    ramp <- c('#FFFFFF', '#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
    pal <- colorRampPalette(ramp)
    
    plot(clippedInfected, col = pal(8)[-2], axes = TRUE, cex.main = 1, main = "Initial Infections", plg = list(title = expression(bold("Persons")), title.cex = 1, horiz=TRUE, x.intersp=0.6, inset=c(0, -0.2), cex=1.15), pax = list(cex.axis=1.15), legend=TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
    
    gadmFileName <- paste0("gadm36_", toupper(inputISO), "_1_sp.rds")   # name of the .rds file
    gadmFolder <- "gadm/"                                               # .rds files should be stored in local gadm/ folder
    
    #print(paste0(gadmFolder, gadmFileName))
    GADMdata <- readRDS(paste0(gadmFolder, gadmFileName))
    GADMdata <- GADMdata[GADMdata$NAME_1 %in% c(level1Names), ]
    
    plot(GADMdata, add = TRUE)
    
    # clippedSusceptible <- crop(Susceptible, inhabitableTrim)
    # writeRaster(clippedSusceptible, "DRCSusceptible", format = "GTiff", overwrite = TRUE) # the tif file may not be at 1km resolution

    #returnList <- list("rasterStack" = rasterStack, "Level1Identifier" = Level1Identifier, "selectedCountry" = selectedCountry, "rasterAgg" = rasterAgg, "WorldPopRows" = nrow(WorldPop), "WorldPopCols" = ncol(WorldPop), "WorldPopCells" = ncell(WorldPop))

    #return(returnList)
}

#------------------------#
# Example Function Calls #
#------------------------#
 createClippedSeedPlot("Democratic Republic of Congo", 15, level1Names = c("Ituri", "Nord-Kivu"), seedData = "COD_InitialSeedData.csv", radius = 0)

# createClippedSeedPlot("Democratic Republic of Congo", 15, level1Names = c("Ituri", "Nord-Kivu"), seedData = "COD_InitialSeedData.csv", radius = 1)

createClippedSeedPlot("Nigeria", 5, level1Names = c("Kwara", "Oyo"), seedData = "NGA_InitialSeedDataSep 1, 2020_Oyo_Kwara.csv", radius = 0)

