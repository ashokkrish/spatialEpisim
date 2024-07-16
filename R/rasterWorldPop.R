library(countrycode)
library(terra, warn.conflicts = FALSE)

createSusceptibleLayer <- function(selectedCountry, rasterAgg = 0) {

  ##----------------------------------------------------------------#
  ## Source 1: WorldPop UN-Adjusted Population Count GeoTIFF raster #
  ##----------------------------------------------------------------#
  inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c') # Converts country name to ISO Alpha
  inputISOLower <- tolower(inputISO)

  ## Construct the path to the data on data.worldpop.org
  urlPath <-
    c("GIS",
      "Population",
      "Global_2000_2020_1km_UNadj",
      "2020",
      iso3c,
      basename = sprintf("%s_ppp_2020_1km_Aggregated_UNadj.tif",
                         tolower(iso3c))) %>%
    httr2:::dots_to_path()
  url_build(structure(list(scheme = "https",
                           hostname = "data.worldpop.org",
                           path = urlPath),
                      class = "httr2_url"))

  tifFileName <- basename(url)    # name of the .tif file
  tifFolder <- "tif/"             # .tif files should be stored in local tif/ folder
  
  if (!file.exists(paste0(tifFolder, tifFileName)))
  {
    download.file(url, paste0(tifFolder, tifFileName), mode = "wb")
  }

  WorldPop <- rast(paste0(tifFolder, tifFileName))

                                        #Gives the five number summary
  print(summary(values(WorldPop)))

                                        #Number of cells that have an NA value
  print(sum(is.na(values(WorldPop))))

                                        # print(as.raster(WorldPop))

  WorldPop <- replace(WorldPop, is.na(WorldPop), 0) # Delete this line for clear plot. Check!!!

  ## WorldPop <- terra::rast(paste0(tifFolder, tifFileName))
  ## Use the above line if fully switching over to terra R package completely
  ## Note: rasterBasePlot.R was developed with the terra::rast()
  ## Error in (function (classes, fdef, mtable) :
  ##             unable to find an inherited method for function ‘classify’ for signature ‘"RasterLayer"’

  ## print(WorldPop)
  ## print(nrow(WorldPop))
  ## print(ncol(WorldPop))
  ## print(ncell(WorldPop))
  ## print(res(WorldPop))
  ## print(ext(WorldPop))

  if (rasterAgg == 0 || rasterAgg == 1) {
    Aggregated <- WorldPop
  } else {
    Aggregated <- aggregate(WorldPop, fact = c(rasterAgg, rasterAgg), fun = sum, na.rm = TRUE)
  }

  ## print(Susceptible)

  returnList <- list("Susceptible" =  WorldPop, "Aggregated" = Aggregated, "nRows" = nrow(WorldPop), "nCols" = ncol(WorldPop), "nCells" = ncell(WorldPop))

  return(returnList)
}
