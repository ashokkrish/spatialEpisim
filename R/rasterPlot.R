library(terra)
library(raster)
library(cptcity)
library(rasterVis)
library(sp)
library(sf)
library(raster)
library(rgdal)
library(countrycode)
library(av)
library(magick)

#------------------------------------------------------------------------------#
# R Script Variables                                                           #
#------------------------------------------------------------------------------#

modelSelect <- "SEIRD"                            # select model from SEIR, SEIRD, SVEIRD
isoCode <- "CZE"                                  # user should pass ISO code as parameter
year <- 2020                                      # default year (2020)
resKm <- 1                                        # default resolution (1Km)
#aggrFactor <- 10                                 # default aggregation factor (10x10)
#rasterAgg <- 0
PNGFileName <- "susceptible.png"                  # default output file name
MP4FileName <- "susceptible_MP4.mp4"              # default MP4 file name
stackLayerFileName <- "susceptible.png"           # default name for arbitrary raster stack PNG TODO: change format to ISR_Susceptible_0001.png
palettePng <- "misc/seminf_haxby.png"             # default colour palette found in misc folder
layerName <- "Susceptible"                        # used to title raster layer plot
basePopBreaks <- c(0, 10, 25, 50, 100, 250, 1000, 100000) # TODO: dynamically set for each individual country
aggrPopBreaks <- c(0, 5000, 10000, 25000, 50000, 75000, 100000, 250000, 500000) # TODO: dynamically set for each individual country and aggr level

#------------------------------------------------------------------------------#
# R Script Functions (for External Use)                                        #
#------------------------------------------------------------------------------#

printStackLayer <- function(rasterStack, rasterLayer, directOutput, Level1Identifier, selectedCountry, rasterAgg, fname, maxVal, includeLabels) {
  # if(missing(fname)) {fname = stackLayerFileName} 
  setUp (isoCode, year, resKm, rasterAgg, fname)               # called to set www/ folder, stack should already be aggregated
  isoCode <<- countrycode(selectedCountry, origin = "country.name", destination = "iso3c")
  rasterAgg <<- rasterAgg
  layerName <<- toString(rasterLayer)                          # alters the plot title

  createPlotPNG(rasterStack[[layerName]], Level1Identifier, directOutput, maxVal, includeLabels)
}

# createMP4 <- function(isoCode, year, resKm, aggrFactor, fname, iterations) {
#   n <- 1
#   while (n <= iterations) { # create the .png files
#     setUp(isoCode, year, resKm, aggrFactor, paste0("MP4/", isoCode, "_", layerName, "_", sprintf("%04d", n), ".png"))
#     WorldPop <<- WorldPop*(n/iterations)
#     setPopulationBreaks()
#     createPlotPNG(toPlot) 
#     n <- n + 1 
#   } 
#   
#    setwd("www/MP4")         # create the .mp4 file
#    av::av_encode_video(list.files(), framerate = 8, output = paste0("../", fname))
#    setwd("./../..")
# }

# createPNG <- function(isoCode, year, resKm, aggrFactor, fname) {
#   setUp(isoCode, year, resKm, aggrFactor, fname)
#   if (aggrFactor != 0){
#     aggregateWorldPop()
#   }
#   setPopulationBreaks()
#   createPlotPNG(toPlot)
# }

setUp <- function(isoCode, year, resKm, rasterAgg, fname) {
  isoCode <<- isoCode
  year <<- year
  resKm <<- resKm
  rasterAgg <<- rasterAgg
  PNGFileName <<- paste0("www/", fname)
  
  #print(PNGFileName)
  
  #setDirectory()
  #importGeoTiff()
}

#------------------------------------------------------------------------------#
# R Script Functions (for Internal Use)                                        #
#------------------------------------------------------------------------------#

#------------------------------------------------------#
# Creates the base plot .png and outputs to www folder #
#------------------------------------------------------#
createPlotPNG <- function(rasterToPrint, Level1Identifier, directOutput, maxVal, includeLabels) {
  is.na(rasterToPrint) <- !rasterToPrint  # used to clear raster values of 0
  pal <- getPalette(palettePng)
  
  if (!directOutput){
    png(PNGFileName) # output the plot to the www/ image folder
  }
  
  if (rasterAgg == 0 || rasterAgg == 1){
    basePlotTitle <- ""
    if(includeLabels){
      basePlotTitle <- paste0(year, 
                             " UN-Adjusted ", layerName, " Count \n for ", 
                             countrycode(isoCode, origin = "iso3c", destination = "country.name"), 
                             " (", resKm, " sq. km resolution)")
    } else {
      par(bty = 'n')
    }
    plot(rasterToPrint, col=pal(8)[-1], main = basePlotTitle, interpolate = FALSE, axes = includeLabels, zlim=c(0,maxVal))
  } else {
    aggrPlotTitle <- ""
    if (includeLabels){
      aggrPlotTitle <- paste0(year, 
                             " UN-Adjusted Aggregated ", layerName, " Count \n for ", 
                             countrycode(isoCode, origin = "iso3c", destination = "country.name"), 
                             " (", rasterAgg^2 * resKm, " sq. km resolution)")
    } else {
      par(bty = 'n')
    }
    plot(rasterToPrint, col=pal(9)[-1], main = aggrPlotTitle, interpolate = FALSE, axes = includeLabels, zlim=c(0,maxVal))
  }
  plot(Level1Identifier, add = TRUE, axes = includeLabels)
  
  if(includeLabels){
    title(xlab = expression(bold(Longitude)), ylab = expression(bold(Latitude)), line = 2)
  }
  
  if (!directOutput){
    dev.off()     # closes the file opened with png(PNGFileName)
    if (!includeLabels){
      img <- image_read(PNGFileName)
      img <- image_trim(img)
      image_write(img, path=PNGFileName)
    }
  }
}

#---------------------------------------#
# Sets the working directory for R file #
#---------------------------------------#
setDirectory <- function() {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # RStudio IDE preferred
  getwd() # Path to your working directory
}

#-----------------------------------------------#
# Extracts a palette from a formatted .png file #
#-----------------------------------------------#
getPalette <- function(png) {
  raster <- rast(png)
  u <- unique(values(raster))
  hex <- rgb(u[,1], u[,2], u[,3], maxColorValue = 255)
  colorRampPalette(hex)
}

#----------------------------------------------------#
# Imports the GeoTIFF file into tif folder using FTP #
#----------------------------------------------------#
importGeoTiff <- function() {
  # URL format: "https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/2020/NGA/nga_ppp_2020_1km_Aggregated_UNadj.tif"
  url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_", resKm, "km_UNadj/", 
               year, "/", isoCode, "/", tolower(isoCode), "_ppp_", year, "_", resKm,
               "km_Aggregated_UNadj.tif")
  
  tifFileName <- basename(url)  # name of the .tif file
  foldName <- "tif/"         # .tif files should be stored in local tif/ folder
  if (!file.exists(paste0(foldName, tifFileName))){
    download.file(url, paste0(foldName, tifFileName), mode="wb")
  } 
   WorldPop <<- raster(paste0(foldName, tifFileName))
}

#---------------------------------------------------#
# Sets custom population breaks for a given country #
#---------------------------------------------------#
setPopulationBreaks <- function() {
  if (rasterAgg == 0 || rasterAgg == 1){
    toPlot <<- classify(as(WorldPop, "SpatRaster"), basePopBreaks) #raster must be cast to terra spatRaster
    levs <- levels(toPlot)[[1]]
    levs[7] <- "> 1000"
  } else {
    toPlot <<- classify(as(WorldPop_aggr_count, "SpatRaster"), aggrPopBreaks) #raster must be cast to terra spatRaster
    levs <- levels(toPlot)[[1]]
    levs[8] <- "> 250000"
  }
  levels(toPlot) <- levs
}


# #---------------------------------------------------------------#
# # Aggregate the WorldPop raster by specified aggregation factor #
# #---------------------------------------------------------------#
# aggregateWorldPop <- function() {
#   WorldPop_aggr_count <<- terra :: aggregate(WorldPop, fact = c(aggrFactor, aggrFactor), fun = sum, na.rm = TRUE)
#   is.na(WorldPop_aggr_count) <<- !WorldPop_aggr_count # sets cells which are 0 as NA, allows for 0 lower bound in plot
# }


# END OF CODE #

# createMP4(isoCode = "ISR", year = 2020, resKm = 1, aggrFactor = 1, fname = "susceptible_MP4.mp4", iterations = 30)    # example function calls, comment out before calling script
# createPNG(isoCode = "ISR", year = 2020, resKm = 1, aggrFactor = 1, fname = "susceptible.png") # example function call
# printStackLayer(rasterStack = ?, rasterLayer = "Susceptible", fname = "susceptible.png", includeLabels = T)
