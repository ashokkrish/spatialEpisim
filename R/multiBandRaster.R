library(countrycode)
library(raster)

createMultiBandRaster <- function(selectedCountry) {
  
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
  
  WorldPop <- raster(paste0(tifFolder, tifFileName))
  
  #WorldPop <- replace(WorldPop, is.na(WorldPop), 0)
  
  print(WorldPop)
  
  # print(cellStats(WorldPop == 0, sum))    # Number of grid cells that are have a population count = 0
  # print(cellStats(!is.na(WorldPop), sum)) # Number of grid cells that are have a population count
  # print(cellStats(is.na(WorldPop), sum))  # Number of grid cells that are NA
  # print(cellStats(WorldPop, sum))         # Total estimated population count

  Inhabitable <- WorldPop
  
  values(Inhabitable) <- ifelse(values(WorldPop) > 0, 1, 0) # Fill the Inhabitable rasterLayer with either a 0 or 1.

  Inhabitable <- replace(Inhabitable, is.na(WorldPop), 0)
  
  print("Frequency table for Inhabitable")
  print(freq(Inhabitable))
  
  #---------------------------------------#
  # Source 2: From GADM: Level1Identifier #
  #---------------------------------------#
  
  gadmFileName <- paste0("gadm36_", inputISO, "_1_sp.rds")  # name of the .rds file

  gadmFolder <- "gadm/"         # .rds files should be stored in local gadm/ folder
  
  if (file.exists(paste0(gadmFolder, gadmFileName)))
  {
  Level1Identifier <- readRDS(paste0(gadmFolder, gadmFileName))
  }
  else
  {
      Level1Identifier <- getData("GADM", level = 1, country = inputISOLower)
  }
  
  cat("\n")
  cat("\n")
  
  # print("Details about the SpatialPolygonsDataFrame")
  # print(Level1Identifier)

  # print("List of all states/provinces/regions")
  # print(Level1Identifier$NAME_1)

  crs(Level1Identifier) <- crs(WorldPop)
  #print(coordinates(Level1Identifier)) # Prints the Lat-Lon of Level 1 locations
  
  #-----------------------------#
  # PLOTTING A COUNTRY BOUNDARY #
  #-----------------------------#
  #plot(Level1Identifier, main = "Level 1 Administrative Boundaries")
  
  Level1Raster <- raster(Level1Identifier, resolution = res(WorldPop)[1])
  Level1Raster <- rasterize(Level1Identifier, Level1Raster) # May take a few seconds to run
  #Level1Raster <- replace(Level1Raster, is.na(Level1Raster), 0)

  # print("Frequency table for Level1Raster before resampling")
  # print(freq(Level1Raster))
  
  before <- freq(Level1Raster)
  
  # Resampling methods
  # "ngb": Nearest-neighbor; assigns the value of the nearest cell
  # "bilinear": Bilinear interpolation; assigns a weighted average of the four nearest cells (the default)

  # cat("\n")
  # cat("\n")
  # 
  # print("LevelRaster RasterLayer before resampling")
  # print(Level1Raster)
  
  Level1Raster <-  resample(Level1Raster, WorldPop, method = "ngb")

  # Level1Raster <- replace(Level1Raster, is.na(WorldPop), NA)
  # 
  Level1Raster <- replace(Level1Raster, is.na(Level1Raster), -1) 
  
  print(Level1Raster)
  
  print(freq(Level1Raster))
  
  values(Level1Raster) <- ifelse(values(Inhabitable) > 0, values(Level1Raster), NA)
  
  Level1Raster <- replace(Level1Raster, is.na(Level1Raster), 0) 
  
  values(Level1Raster) <- ifelse(values(Level1Raster) < 0, NA, values(Level1Raster))
  
  cat("\n")
  cat("\n")

  print("LevelRaster RasterLayer after resampling")
  print(Level1Raster)
  
  # print("Frequency table for Level1Raster after resampling")
  # print(freq(Level1Raster))

  after <- freq(Level1Raster)
  
  cat("\n")
  cat("\n")
  
  print("Frequency table for Level1Raster before and after resampling")
  #print(cbind(before, after))
  #print(before)
  print(after)
  
  # print(dim(WorldPop)); print(dim(Inhabitable)); print(dim(Level1Raster))
  # print(res(WorldPop)); print(res(Inhabitable)); print(res(Level1Raster))
  # print(origin(WorldPop)); print(origin(Inhabitable)); print(origin(Level1Raster))
  # print(extent(WorldPop));  print(extent(Inhabitable)); print(extent(Level1Raster))
  # print(crs(WorldPop));  print(crs(Level1Raster))
 
  # extent(Level1Identifier) <- extent(WorldPop)
  # print(extent(Level1Identifier))
  
   rasterStack <- stack(WorldPop, Inhabitable, Level1Raster)
   
   names(rasterStack) <- c("WorldPop", "Inhabitable", "Level1Raster")
   
   returnList <- list("rasterStack" = rasterStack, "Level1Identifier" = Level1Identifier, "WorldPopRows" = nrow(WorldPop), "WorldPopCols" = ncol(WorldPop), "WorldPopCells" = ncell(WorldPop))
   
  return(returnList)
}

#------------------------#
# Example Function Calls #
#------------------------#

#createMultiBandRaster("Czech Republic")
#createMultiBandRaster("Qatar")
#createMultiBandRaster("Latvia")

# rs <- createMultiBandRaster("Czech Republic")
# sus <- rs$rasterStack$WorldPop
# lvOne <- rs$rasterStack$Level1Raster
# names <- rs$Level1Identifier$NAME_1
# #print(rs$rasterStack)
# #print(sus)
# #print(lvOne)
# #print(names)
# 
# popCount <- crosstab(sus,lvOne)
# #print(popCount)
# 
# lvMatrix <- as.matrix(lvOne)
# susMatrix <- as.matrix(sus)
# nMatrix <- as.matrix(names)
# 
# sumMatrix <- round(aggregate(c(susMatrix) ~ c(lvMatrix), FUN = sum))
# nameFrame <- data.frame(nMatrix)
# tableFrame <- data.frame(sumMatrix)
# testFrame <- data.frame(sumMatrix)
# #tableFrame <- tail(tableFrame, -1)
# tableFrame <- tableFrame %>% slice(-1)
# colnames(tableFrame) <- c("Nums", "Values") #renaming Columns to make it easier to reference them
# colnames(nameFrame) <- c("Names")
# tableFrame$Nums <- nameFrame$Names
# colnames(tableFrame) <- c("State/Province", "Population Count") #Changing Names So it matches to Spec
# #print(nameFrame)
# #print(rsMatrix)
# print(testFrame)

