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

createClippedRaster <- function(selectedCountry, level1Region, rasterAgg)
{
  # setwd(dirname(getActiveDocumentContext()$path))
  
  #setwd('..') 
  palettePng <- './misc/seminf_haxby.png'  # default colour palette found in misc folder
  raster <- rast(palettePng)
  u <- unique(values(raster))
  hex <- rgb(u[,1], u[,2], u[,3], maxColorValue = 255)
  pal <- colorRampPalette(hex)

  inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c') #Converts country name to ISO Alpha
  inputISOLower <- tolower(inputISO)
  
  url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/2020/", toupper(inputISO), "/", inputISOLower, "_ppp_2020_1km_Aggregated_UNadj.tif")
  
  tifFileName <- basename(url)    # name of the .tif file
  tifFolder <- "tif/"             # .tif files should be stored in local tif/ folder
  
                                  # navigate to parent folder first
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
  
  gadmFileName <- paste0("gadm36_", toupper(inputISO), "_1_sp.rds")   # name of the .rds file
  gadmFolder <- "gadm/"                                               # .rds files should be stored in local gadm/ folder
  
  #print(paste0(gadmFolder, gadmFileName))
  GADMdata <- readRDS(paste0(gadmFolder, gadmFileName))
  GADMdata <- GADMdata[GADMdata$NAME_1 %in% c(level1Region), ]
  
  lvl1Raster <- crop(WorldPop, GADMdata)
  lvl1Raster <- mask(lvl1Raster, GADMdata)

  # newProj <- CRS("+proj=longlat +datum=WGS84 +no_defs")     # Warning message, look into later...
  # countryProj <- spTransform(GADMdata, newProj)             # This is not used anywhere
  
  print(lvl1Raster)
  # print(ext(lvl1Raster))
  # print(crs(lvl1Raster))
  
  # if (selectedCountry == "Czech Republic"){
  #   aggrPlotTitle <- paste0("2020 UN-Adjusted Population Count \n for the ", 
  #                           selectedCountry, 
  #                           " (1 sq. km resolution)")
  # }
  # else
  # {
  #   aggrPlotTitle <- paste0("2020 UN-Adjusted Population Count \n for ", 
  #                           selectedCountry, 
  #                           " (1 sq. km resolution)")  
  # }
  
  aggrPlotTitle <- "2020 UN-Adjusted Population Count"
  
  terra::plot(lvl1Raster, col=pal(20)[-1], axes = TRUE, cex.main = 1, main = aggrPlotTitle)
  terra::north(type = 2, xy = "bottomleft", cex = 2)
  
  title(xlab = expression(bold(Longitude)), ylab = expression(bold(Latitude)), line = 2, cex.lab=1.20)
  
  dir.create(file.path("tif/cropped"), showWarnings = FALSE)
  level1Region <- tolower(gsub(" ", "", gsub(",", "_", toString(level1Region)))) # for single string and list depending on parameter
  writeRaster(lvl1Raster, paste("tif/cropped/", level1Region, inputISOLower,"ppp_2020_1km_Aggregated_UNadj.tif", sep='_'), format = "GTiff", overwrite = TRUE) # the tif file may not be at 1km resolution
  print(getwd())
  #setwd('./R')
  #print(getwd())
}

#------------------------#
# Example Function Calls #
#------------------------#

# setwd('..')
# 
# createClippedRaster(selectedCountry = "Czech Republic", level1Region = "Prague", rasterAgg = 0)
#
# setwd('..')
#  
# createClippedRaster(selectedCountry = "Nigeria", level1Region = "Lagos", rasterAgg = 0)
#  
# setwd('..')
# 
# createClippedRaster(selectedCountry = "Nigeria", level1Region = "Rivers", rasterAgg = 0)
# 
# setwd('..')
#  
# createClippedRaster(selectedCountry = "Democratic Republic of Congo", level1Region = "Ituri", rasterAgg = 0)
#  
# setwd('..')
#  
# createClippedRaster(selectedCountry = "Democratic Republic of Congo", level1Region = c("Nord-Kivu", "Ituri"), rasterAgg = 0)
# 
# setwd('..')
#  
# createClippedRaster(selectedCountry = "Democratic Republic of Congo", level1Region = c("Nord-Kivu", "Ituri"), rasterAgg = 15)
# 
# setwd('..')
# createClippedRaster(selectedCountry = "Canada", level1Region = "Alberta", rasterAgg = 0)
#