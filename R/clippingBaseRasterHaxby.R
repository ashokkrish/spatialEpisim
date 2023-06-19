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

createClippedRaster <- function(selectedCountry, level1Region, rasterAgg, directOutput)
{
  # setwd(dirname(getActiveDocumentContext()$path))
  
  #setwd('..') 

  # palettePng <- './misc/seminf_haxby.png'  # default colour palette found in misc folder
  # raster <- rast(palettePng)
  # u <- unique(values(raster))

  #----------------------------------------------------------------#
  # Source 1: WorldPop UN-Adjusted Population Count GeoTIFF raster #
  #----------------------------------------------------------------#
  
  inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c') #Converts country name to ISO Alpha
  inputISOLower <- tolower(inputISO)
  
  url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/2020/", toupper(inputISO), "/", inputISOLower, "_ppp_2020_1km_Aggregated_UNadj.tif")
  
  tifFileName <- basename(url)    # name of the .tif file
  tifFolder <- "tif/"             # .tif files should be stored in local tif/ folder

  if (!file.exists(paste0(tifFolder, tifFileName)))
  {
    download.file(url, paste0(tifFolder, tifFileName), mode = "wb")
  }
  
  fname <- paste0("clipped_", inputISO, "_PopulationCount.png")
  PNGFileName <<- paste0("www/", fname)
  
  if(!directOutput){png(PNGFileName, width = 1024, height = 768)} # output the plot to the www image folder
  
  WorldPop <- raster(paste0(tifFolder, tifFileName))

  WorldPop <- replace(WorldPop, is.na(WorldPop), 0) 
  
  if (!(rasterAgg == 0 || rasterAgg == 1)) 
  {
    WorldPop <- aggregate(WorldPop, fact = c(rasterAgg, rasterAgg), fun = sum, na.rm = TRUE)
  }

  #---------------------------------------#
  # Source 2: From GADM: Level1Identifier #
  #---------------------------------------#
  
  gadmFileName <- paste0("gadm36_", toupper(inputISO), "_1_sp.rds")   # name of the .rds file
  gadmFolder <- "gadm/"                                               # .rds files should be stored in local gadm/ folder
  
  #print(paste0(gadmFolder, gadmFileName))
  GADMdata <- readRDS(paste0(gadmFolder, gadmFileName))
  GADMdata <- GADMdata[GADMdata$NAME_1 %in% c(level1Region), ]
  
  lvl1Raster <- crop(WorldPop, GADMdata)
  # print(WorldPop)
  
  dlong = abs(xmax(lvl1Raster) - xmin(lvl1Raster))
  dlat = abs(ymax(lvl1Raster) - xmax(lvl1Raster))
   
 
  lvl1Raster <- mask(lvl1Raster, GADMdata)
  
  lvl1Rasterrast <- lvl1Raster
  
  lvl1Raster <- terra::rast(lvl1Raster)
 
  x <- classify(lvl1Raster, c(0, 10, 25, 50, 100, 250, 1000, 10000))
  
  #plot(x, col=pal(8)[-1], xlab = "Longitude", ylab = "Latitude")
  
   levs <- levels(x)[[1]]
  # #levs[7] <- "> 1000"
   levels(x) <- levs
  
  #ramp <- c('#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
  ramp <- c('#FFFFFF', '#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
  pal <- colorRampPalette(ramp)
  

  # newProj <- CRS("+proj=longlat +datum=WGS84 +no_defs")     # Warning message, look into later...
  # countryProj <- spTransform(GADMdata, newProj)             # This is not used anywhere
  
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
  
  aggrPlotTitle <- paste0("2020 UN-Adjusted Population Count \n for ", 
                          level1Region, " in ",
                          selectedCountry,
                          " (1 sq. km resolution)")
  
  terra::plot(x, col=pal(8)[-1], axes = TRUE, cex.main = 1, main = aggrPlotTitle, plg = list(x = xmin(lvl1Raster)-0.24*dlong, y = ymin(lvl1Raster)-0.015*dlat, title ="Persons", horiz=TRUE, x.intersp=0.6, inset=c(0, -0.2), cex=1.15), pax = list(cex.axis=1.15), legend = TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
  terra::north(type = 2, xy = "bottomleft", cex = 1)
  
  title(xlab = expression(bold(Longitude)), ylab = expression(bold(Latitude)), line = 2, cex.lab=1.20)
  # 
  # Level1Identifier <- readRDS(paste0(gadmFolder, gadmFileName))
  # 
  # Level1Identifier<- Level1Identifier[Level1Identifier$NAME_1 %in% c(level1Region), ]
  # 
  plot(GADMdata, add = TRUE)
  
  # if(!directOutput){dev.off()} 
  if(!directOutput){dev.off()}     # closes the file opened with png(PNGFileName)
  # title(xlab = expression(bold(Longitude)), ylab = expression(bold(Latitude)), line = 2, cex.lab=1.20)
  
  dir.create(file.path("tif/cropped"), showWarnings = FALSE)
  level1Region <- tolower(gsub(" ", "", gsub(",", "_", toString(level1Region)))) # for single string and list depending on parameter
  writeRaster(lvl1Rasterrast, paste("tif/cropped/", level1Region, inputISOLower,"ppp_2020_1km_Aggregated_UNadj.tif", sep='_'), format = "GTiff", overwrite = TRUE) # the tif file may not be at 1km resolution
  #print(getwd())
  #setwd('./R')
  #print(getwd())
}

#------------------------#
# Example Function Calls #
#------------------------#

#setwd('..')
#
#createClippedRaster(selectedCountry = "Czech Republic", level1Region = "Prague", rasterAgg = 0)
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
 createClippedRaster(selectedCountry = "Democratic Republic of Congo", level1Region = "Ituri", rasterAgg = 0, directOutput=FALSE)
#  
# setwd('..')
#  
#createClippedRaster(selectedCountry = "Democratic Republic of Congo", level1Region = c("Nord-Kivu", "Ituri"), rasterAgg = 0, directOutput = FALSE)
# 
# setwd('..')
#  
# createClippedRaster(selectedCountry = "Democratic Republic of Congo", level1Region = c("Nord-Kivu", "Ituri"), rasterAgg = 15)
# 
# setwd('..')
#
# createClippedRaster(selectedCountry = "Canada", level1Region = "Alberta", rasterAgg = 0)