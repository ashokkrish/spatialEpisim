library(sf)
library(fasterize)
library(sp)
library(rgdal)
library(ncdf4)
library(rstudioapi)
library(raster)
library(countrycode)

createClippedRaster <- function(selectedCountry, level1Region, rasterAgg)
{
  
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
  
  WorldPop <- replace(WorldPop, is.na(WorldPop), 0) 
  
  if (!(rasterAgg == 0 || rasterAgg == 1)) 
  {
    WorldPop <- aggregate(WorldPop, fact = c(rasterAgg, rasterAgg), fun = sum, na.rm = TRUE)
  }
  
  print(WorldPop)
  
  gadmFileName <- paste0("gadm36_", inputISO, "_1_sp.rds")        # name of the .rds file
  gadmFolder <- "gadm/"                                           # .rds files should be stored in local gadm/ folder
  GADMdata <- readRDS(paste0(gadmFolder, gadmFileName))
  GADMdata <- GADMdata[GADMdata$NAME_1 %in% c(level1Region), ]
  
  lvl1Raster <- crop(WorldPop, GADMdata)
  lvl1Raster <- mask(lvl1Raster, GADMdata)
  
  newProj <- CRS("+proj=longlat +datum=WGS84 +no_defs")           # Warning message, look into later...
  countryProj <- spTransform(GADMdata, newProj)
  
  print(lvl1Raster)
  plot(lvl1Raster)
  plot(log(lvl1Raster))
  
  level1Region <- tolower(gsub(" ", "", gsub(",", "_", toString(level1Region)))) # for single string and list depending on parameter
  writeRaster(lvl1Raster,paste(level1Region, inputISOLower,"ppp_2020_1km_Aggregated_UNadj.tif",sep='_'),format="GTiff", overwrite=TRUE)
}

#------------------------#
# Example Function Calls #
#------------------------#

#createClippedRaster(selectedCountry = "Czech Republic", level1Region = "Prague", rasterAgg = 0)

#createClippedRaster(selectedCountry = "Nigeria", level1Region = "Lagos", rasterAgg = 0)

#createClippedRaster(selectedCountry = "Nigeria", level1Region = "Rivers", rasterAgg = 0)

#createClippedRaster(selectedCountry = "Canada", level1Region = "Alberta", rasterAgg = 0)

#createClippedRaster(selectedCountry = "Democratic Republic of Congo", level1Region = "Ituri", rasterAgg = 0)

#createClippedRaster(selectedCountry = "Democratic Republic of Congo", level1Region = c("Nord-Kivu", "Ituri"), rasterAgg = 10)
