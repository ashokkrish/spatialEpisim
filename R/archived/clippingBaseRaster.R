options(conflicts.policy = list(warn = FALSE))
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(av))
shhh(library(countrycode))
shhh(library(cptcity))
shhh(library(lattice))
shhh(library(magick))
shhh(library(sp))
shhh(library(sf))     # classes and functions for vector data
options("rgdal_show_exportToProj4_warnings"="none")
shhh(library(rgdal, warn.conflicts=FALSE))
shhh(library(raster, warn.conflicts=FALSE))
shhh(library(rasterVis))
shhh(library(terra, warn.conflicts=FALSE))
shhh(library(rstudioapi))
shhh(library(fasterize))

source("R/rasterWorldPop.R")

createClippedRaster <- function(selectedCountry, level1Region, rasterAgg)
{
  inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c') # Converts country name to ISO Alpha
  inputISOLower <- tolower(inputISO)
  
  Susceptible <- createSusceptibleLayer(selectedCountry, rasterAgg, isCropped, level1Names = NULL)$Susceptible
  print(Susceptible)
  
  #Susceptible <- terra::rast(Susceptible)

  gadmFileName <- paste0("gadm36_", toupper(inputISO), "_1_sp.rds")   # name of the .rds file
  gadmFolder <- "gadm/"                                               # .rds files should be stored in local gadm/ folder
  
  #print(paste0(gadmFolder, gadmFileName))
  
  GADMdata <- readRDS(paste0(gadmFolder, gadmFileName))
  GADMdata <- GADMdata[GADMdata$NAME_1 %in% c(level1Region), ]
  
  lvl1Raster <- crop(Susceptible, GADMdata)
  lvl1Raster <- mask(lvl1Raster, GADMdata)
  
  newProj <- CRS("+proj=longlat +datum=WGS84 +no_defs")     # Warning message, look into later...
  countryProj <- spTransform(GADMdata, newProj)             # This is not used anywhere
  
  print(lvl1Raster)
  #plot(lvl1Raster)
  #plot(log(lvl1Raster))
  
  # @Tom Currently the clipped raster tif file is written on to the root directory. Write this to /tif directory.
  
  level1Region <- tolower(gsub(" ", "", gsub(",", "_", toString(level1Region)))) # for single string and list depending on parameter
  writeRaster(lvl1Raster, paste(level1Region, inputISOLower,"ppp_2020_1km_Aggregated_UNadj.tif", sep='_'), format = "GTiff", overwrite = TRUE)
}

#------------------------#
# Example Function Calls #
#------------------------#

# createClippedRaster(selectedCountry = "Czech Republic", level1Region = "Prague", rasterAgg = 0)
# 
# createClippedRaster(selectedCountry = "Nigeria", level1Region = "Lagos", rasterAgg = 0)
# 
# createClippedRaster(selectedCountry = "Nigeria", level1Region = "Rivers", rasterAgg = 0)
# 
# createClippedRaster(selectedCountry = "Democratic Republic of Congo", level1Region = "Ituri", rasterAgg = 0)
# 
# createClippedRaster(selectedCountry = "Democratic Republic of Congo", level1Region = c("Nord-Kivu", "Ituri"), rasterAgg = 10)