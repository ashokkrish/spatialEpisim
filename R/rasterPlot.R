options(conflicts.policy = list(warn = FALSE))
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(av))
shhh(library(countrycode))
shhh(library(cptcity))
shhh(library(fasterize))
shhh(library(lattice))
shhh(library(magick))
options("rgdal_show_exportToProj4_warnings"="none")
shhh(library(rgdal, warn.conflicts=FALSE))
# shhh(library(raster, warn.conflicts=FALSE))
shhh(library(rasterVis))
shhh(library(rstudioapi))
shhh(library(sp))
shhh(library(sf))     # classes and functions for vector data
shhh(library(terra, warn.conflicts=FALSE))

#------------------------------------------------------------------------------#
# R Script Variables                                                           #
#------------------------------------------------------------------------------#

# modelSelect <- "SVEIRD"                         # select model from SEIR, SEIRD, SVEIRD
isoCode <- "COD"                                  # user should pass ISO code as parameter
year <- 2020                                      # default year (2020)
resKm <- 1                                        # default resolution (1Km)
# rasterAgg <- 10                                 # default aggregation factor (10x10)
PNGFileName <- "susceptible.png"                  # default output file name
MP4FileName <- "susceptible_MP4.mp4"              # default MP4 file name
stackLayerFileName <- "susceptible.png"           # default name for arbitrary raster stack PNG TODO: change format to ISO3_Susceptible_0001.png
# palettePng <- "misc/seminf_haxby.png"             # default colour palette found in misc folder
colPalette <- c('#FFFFFF', 
                '#D0D8FB', 
                '#BAC5F7', 
                '#8FA1F1', 
                '#617AEC', 
                '#0027E0', 
                '#1965F0', 
                '#0C81F8', 
                '#18AFFF', 
                '#31BEFF', 
                '#43CAFF', 
                '#60E1F0', 
                '#69EBE1', 
                '#7BEBC8', 
                '#8AECAE', 
                '#ACF5A8', 
                '#CDFFA2', 
                '#DFF58D', 
                '#F0EC78', 
                '#F7D767', 
                '#FFBD56', 
                '#FFA044', 
                '#EE4F4D')
layerName <- "Susceptible"                        # used to title raster layer plot
basePopBreaks <- c(0, 10, 25, 50, 100, 250, 1000, 100000) # TODO: dynamically set for each individual country
aggrPopBreaks <- c(0, 5000, 10000, 25000, 50000, 75000, 100000, 250000, 500000) # TODO: dynamically set for each individual country and aggr level

#------------------------------------------------------------------------------#
# R Script Functions (for External Use)                                        #
#------------------------------------------------------------------------------#

printStackLayer <- function(rasterStack, rasterLayer, directOutput, Level1Identifier, selectedCountry, rasterAgg, fname, maxVal, includeLabels) {
  # if(missing(fname)) {fname = stackLayerFileName} 
  setUp(isoCode, year, resKm, rasterAgg, fname)               # called to set www/ folder, stack should already be aggregated
  isoCode <<- countrycode(selectedCountry, origin = "country.name", destination = "iso3c")
  rasterAgg <<- rasterAgg
  layerName <<- toString(rasterLayer)                          # alters the plot title
  createPlotPNG(rasterStack[[layerName]], Level1Identifier, directOutput, maxVal, includeLabels)
}

# createMP4 <- function(isoCode, year, resKm, rasterAgg, fname, iterations) {
#   n <- 1
#   while (n <= iterations) { # create the .png files
#     setUp(isoCode, year, resKm, rasterAgg, paste0("MP4/", isoCode, "_", layerName, "_", sprintf("%04d", n), ".png"))
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

# createPNG <- function(isoCode, year, resKm, rasterAgg, fname) {
#   setUp(isoCode, year, resKm, rasterAgg, fname)
#   if (rasterAgg != 0){
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

  x <- classify(rasterToPrint, c(0, 10, 25, 50, 100, 250, 1000, 100000))
  levs <- levels(x)[[1]]
  levels(x) <- levs
  
  # pal <- getPalette(palettePng)
  pal <- colorRampPalette(colPalette)

  if (!directOutput){
    png(PNGFileName, width = 1024, height = 768) # output the plot to the www/ image folder
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
    terra::plot(x, 
                col=pal(8)[-1], 
                main = basePlotTitle, 
                axes = includeLabels,
                buffer = TRUE,
                box = TRUE,
                cex.main = 1.5,
                line.main = 1.25,
                all_levels = TRUE,
                zlim=c(0,maxVal),
                plg = list(title = expression(bold("Persons")),
                           title.cex = 1.25,
                           horiz = FALSE, 
                           loc = "topright",
                           xjust = -1,
                           cex = 1.25),
                mar = c(8.5, 3.5, 4, 2.5))
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
    terra::plot(x, 
                col=pal(9)[-1], 
                main = aggrPlotTitle, 
                axes = includeLabels,
                buffer = TRUE,
                box = TRUE,
                cex.main = 1.5,
                line.main = 1.25,
                all_levels = TRUE,
                zlim=c(0,maxVal),
                plg = list(title = expression(bold("Persons")),
                           title.cex = 1.25,
                           horiz = FALSE, 
                           loc = "topright",
                           xjust = -1,
                           cex = 1.25),
                mar = c(8, 3.5, 4, 5))
  }
  terra::plot(Level1Identifier, 
              add = TRUE)
  terra::north(type = 2, xy = "bottomleft", cex = 1)
  
  if(includeLabels){
    title(xlab = expression(bold(Longitude)), 
          ylab = expression(bold(Latitude)),
          line = 1,
          cex.lab = 1.5)
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
# setDirectory <- function() {
#   setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # RStudio IDE required
#   getwd() # Path to the working directory
# }

#-----------------------------------------------#
# Extracts a palette from a formatted .png file #
#-----------------------------------------------#
# getPalette <- function(png) {
#   raster <- terra::rast(png)
#   u <- unique(values(raster))
#   hex <- rgb(u[,1], u[,2], u[,3], maxColorValue = 255)
#   colorRampPalette(hex)
# }

#----------------------------------------------------#
# Imports the GeoTIFF file into tif folder using FTP #
#----------------------------------------------------#
importGeoTiff <- function() {
  url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_", resKm, "km_UNadj/", 
               year, "/", toupper(isoCode), "/", tolower(isoCode), "_ppp_", year, "_", resKm,
               "km_Aggregated_UNadj.tif")
  
  tifFileName <- basename(url)  # name of the .tif file
  foldName <- "tif/"            # .tif files should be stored in local tif/ folder
  
  if (!file.exists(paste0(foldName, tifFileName))){
    download.file(url, paste0(foldName, tifFileName), mode = "wb")
  } 
   WorldPop <<- rast(paste0(foldName, tifFileName))
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
#   WorldPop_aggr_count <<- terra::aggregate(WorldPop, fact = c(rasterAgg, rasterAgg), fun = sum, na.rm = TRUE)
#   is.na(WorldPop_aggr_count) <<- !WorldPop_aggr_count # sets cells which are 0 as NA, allows for 0 lower bound in plot
# }

# END OF CODE #

# createMP4(isoCode = "ISR", year = 2020, resKm = 1, rasterAgg = 1, fname = "susceptible_MP4.mp4", iterations = 30)    # example function calls, comment out before calling script
# createPNG(isoCode = "ISR", year = 2020, resKm = 1, rasterAgg = 1, fname = "susceptible.png") # example function call
# printStackLayer(rasterStack = ?, rasterLayer = "Susceptible", fname = "susceptible.png", includeLabels = T)