library(terra)
library(raster)
library(countrycode)

getPal <- function(f) {
  x <- rast(f)
  u <- unique(values(x))
  hex <- rgb(u[,1], u[,2], u[,3], maxColorValue = 255)
  colorRampPalette(hex)
}

palettePng <- "misc/seminf_haxby.png"  # default colour palette found in misc folder

createBasePlot <- function(selectedCountry, rasterAgg, directOutput) {

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

  fname <- paste(inputISO, "_PopulationCount.png")
  PNGFileName <<- paste0("www/", fname)
  if(!directOutput){png(PNGFileName, width = 1024, height = 768)} # output the plot to the www image folder
  
  WorldPop <- terra::rast(paste0(tifFolder, tifFileName))

  # WorldPop <- replace(WorldPop, is.na(WorldPop), 0) # TODO: delete this line for clear plot @Gurs why???

  x <- classify(WorldPop, c(0, 10, 25, 50, 100, 250, 1000, 100000))
  
  #plot(x, col=pal(8)[-1], xlab = "Longitude", ylab = "Latitude")
  
  levs <- levels(x)[[1]]
  levels(x) <- levs

  #ramp <- c('#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
  ramp <- c('#FFFFFF', '#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
  pal <- colorRampPalette(ramp)
  
  aggrPlotTitle <- paste0("2020 UN-Adjusted Population Count \n for ", 
                         selectedCountry, 
                         " (1 sq. km resolution)")
  
  plot(x, col = pal(8)[-1], axes = TRUE, main = aggrPlotTitle)
  
  title( xlab = expression(bold(Longitude)), ylab = expression(bold(Latitude)), line = 2)

  if (rasterAgg == 0 || rasterAgg == 1) {
    Susceptible <- WorldPop
  } else {
    Susceptible <- aggregate(WorldPop, fact = c(rasterAgg, rasterAgg), fun = sum, na.rm = TRUE)
  }
  
  #---------------------------------------#
  # Source 2: From GADM: Level1Identifier #
  #---------------------------------------#
  
  gadmFileName <- paste0("gadm36_", inputISOLower, "_1_sp.rds")  # name of the .rds file
  gadmFolder <- "gadm/"         # .rds files should be stored in local gadm/ folder
  
  Level1Identifier <- readRDS(paste0(gadmFolder, gadmFileName))

  plot(Level1Identifier, add=TRUE)
  if(!directOutput){dev.off()}     # closes the file opened with png(PNGFileName)
}

# NOTE:
# During the first run an error message appears which can be safely ignored.
# See details here: https://github.com/rspatial/terra/issues/30
# In fact, in the second call of the above function the error disappears.

#------------------------#
# Example Function Calls #
#------------------------#

# createBasePlot(selectedCountry = "Nigeria", rasterAgg = 0, directOutput = T)
# createBasePlot(selectedCountry = "Nigeria", rasterAgg = 0, directOutput = F)
# 
# createBasePlot(selectedCountry = "Czech Republic", rasterAgg = 0, directOutput = T)
# createBasePlot(selectedCountry = "Czech Republic", rasterAgg = 0, directOutput = F)
# 
# createBasePlot(selectedCountry = "Israel", rasterAgg = 0, directOutput = T)
# createBasePlot(selectedCountry = "Israel", rasterAgg = 0, directOutput = F)
# 
# createBasePlot(selectedCountry = "Latvia", rasterAgg = 0, directOutput = T)
# createBasePlot(selectedCountry = "Latvia", rasterAgg = 0, directOutput = F)
# 
# createBasePlot(selectedCountry = "Belgium", rasterAgg = 0, directOutput = T)
# createBasePlot(selectedCountry = "Belgium", rasterAgg = 0, directOutput = F)
# 
# createBasePlot(selectedCountry = "Japan", rasterAgg = 0, directOutput = T)
# createBasePlot(selectedCountry = "Japan", rasterAgg = 0, directOutput = F)
# 
# createBasePlot(selectedCountry = "Korea", rasterAgg = 0, directOutput = T)
# createBasePlot(selectedCountry = "Korea", rasterAgg = 0, directOutput = F)
# 
# createBasePlot(selectedCountry = "Austria", rasterAgg = 0, directOutput = T)
# createBasePlot(selectedCountry = "Austria", rasterAgg = 0, directOutput = F)