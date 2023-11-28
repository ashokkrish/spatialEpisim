library(countrycode)
library(raster, warn.conflicts = FALSE)
#library(terra, warn.conflicts = FALSE)

createSusceptibleLayer <- function(selectedCountry, rasterAgg, isCropped = F, level1Names = NULL) {

#----------------------------------------------------------------#
# Source 1: WorldPop UN-Adjusted Population Count GeoTIFF raster #
#----------------------------------------------------------------#

inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c') # Converts country name to ISO Alpha
inputISOLower <- tolower(inputISO)

url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/2020/", inputISO, "/", inputISOLower, "_ppp_2020_1km_Aggregated_UNadj.tif")

tifFileName <- basename(url)    # name of the .tif file
tifFolder <- "tif/"             # .tif files should be stored in local tif/ folder

if (!file.exists(paste0(tifFolder, tifFileName)))
{
  download.file(url, paste0(tifFolder, tifFileName), mode = "wb")
}

WorldPop <- raster(paste0(tifFolder, tifFileName))

WorldPop <- replace(WorldPop, is.na(WorldPop), 0) # Delete this line for clear plot. Check!!!

# WorldPop <- terra::rast(paste0(tifFolder, tifFileName))
# Use the above line if fully switching over to terra R package completely
# Note: rasterBasePlot.R was developed with the terra::rast()
# Error in (function (classes, fdef, mtable) :
#             unable to find an inherited method for function ‘classify’ for signature ‘"RasterLayer"’

# print(WorldPop)
# print(nrow(WorldPop))
# print(ncol(WorldPop))
# print(ncell(WorldPop))
# print(res(WorldPop))
# print(ext(WorldPop))

if (rasterAgg == 0 || rasterAgg == 1) {
  Susceptible <- WorldPop
} else {
  Susceptible <- aggregate(WorldPop, fact = c(rasterAgg, rasterAgg), fun = sum, na.rm = TRUE)
}

# print(Susceptible)

returnList <- list("Susceptible" =  Susceptible)

return(returnList)
}

#------------------------#
# Example Function Calls #
#------------------------#

# NOTE: The isCropped argument is unused

# createSusceptibleLayer("Nigeria", 0, isCropped = F, level1Names = NULL)