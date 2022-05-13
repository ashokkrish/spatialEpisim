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

  Inhabitable <- WorldPop
  
  values(Inhabitable) <- ifelse(values(WorldPop) > 0, 1, 0) # Fill the Inhabitable rasterLayer with either a 0 or 1.

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
  
  print("Details about the SpatialPolygonsDataFrame")
  print(Level1Identifier)
  
  print("List of all states/provinces/regions")
  print(Level1Identifier$NAME_1)
  
  Level1Raster <- raster(Level1Identifier, resolution = res(WorldPop)[1])
  Level1Raster <- rasterize(Level1Identifier, Level1Raster) # May take a few seconds to run
  #Level1Raster <- replace(Level1Raster, is.na(Level1Raster), 0)

  print("Frequency table for Level1Raster before resampling")
  print(freq(Level1Raster))
  
  # Resampling methods
  # "ngb": Nearest-neighbor; assigns the value of the nearest cell
  # "bilinear": Bilinear interpolation; assigns a weighted average of the four nearest cells (the default)

  Level1Raster <-  resample(Level1Raster, WorldPop, method = "ngb")
  
  #Level1Raster <- replace(Level1Raster, is.na(Level1Raster), 0) 

  print(Level1Raster)
  
  print("Frequency table for Level1Raster after resampling")
  
  print(freq(Level1Raster))
  print(freq(Inhabitable))
  
  print(dim(Level1Raster)); print(dim(WorldPop))
  print(res(Level1Raster)); print(res(WorldPop))
  print(origin(Level1Raster)); print(origin(WorldPop))
  
  # print(extent(Level1Raster))
  # print(extent(WorldPop))
  
  #extent(Level1Identifier) <- extent(WorldPop)
  
  #print(extent(Level1Identifier))
  
  rasterStack <- stack(WorldPop, Inhabitable, Level1Raster)
  
  names(rasterStack) <- c("WorldPop", "Inhabitable", "Level1Raster")

  returnList <- list("rasterStack" = rasterStack, "Level1Identifier" = Level1Identifier, "WorldPopRows" = nrow(WorldPop), "WorldPopCols" = ncol(WorldPop), "WorldPopCells" = ncell(WorldPop))
  
  #return(returnList)
}

#------------------------#
# Example Function Calls #
#------------------------#

createMultiBandRaster("Qatar")
#createMultiBandRaster("Czech Republic")
#createMultiBandRaster("Latvia")

