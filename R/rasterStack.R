library(countrycode)
library(raster)

createRasterStack <- function(selectedCountry, rasterAgg) {
  
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
  
  #print(paste0(tifFolder, tifFileName))
  
  WorldPop <- raster(paste0(tifFolder, tifFileName))
  
  WorldPop <- replace(WorldPop, is.na(WorldPop), 0) # TODO: delete this line for clear plot @Gurs why???
  
  # print(WorldPop)
  # print(nrow(WorldPop))
  # print(ncol(WorldPop))
  # print(ncell(WorldPop))
  
  if (rasterAgg == 0 || rasterAgg == 1) {
    Susceptible <- WorldPop
  } else {
    Susceptible <- aggregate(WorldPop, fact = c(rasterAgg, rasterAgg), fun = sum, na.rm = TRUE)
  }
  
  #print(Susceptible)
  
  Inhabitable <- Vaccinated <- Exposed <- Infected <- Recovered <- Dead <- Susceptible
  values(Inhabitable) <- ifelse(values(Susceptible) > 0, 1, 0)
  
  #print(table(as.matrix(Inhabitable)))
  
  values(Vaccinated) <- values(Exposed) <- values(Infected) <- values(Recovered) <- values(Dead) <- 0
  
  #print(table(as.matrix(Vaccinated)))
  
  #---------------------------------------#
  # Source 2: From GADM: Level1Identifier #
  #---------------------------------------#
  
  gadmFileName <- paste0("gadm36_", inputISO, "_1_sp.rds")  # name of the .rds file
  
  #print(gadmFileName)
  
  gadmFolder <- "gadm/"         # .rds files should be stored in local gadm/ folder
  
  #print(paste0(gadmFolder, gadmFileName))
  
  # if (file.exists(paste0(gadmFolder, gadmFileName)))
  # {
  Level1Identifier <- readRDS(paste0(gadmFolder, gadmFileName))
  # }
  # else
  # {
  #     Level1Identifier <- getData("GADM", level = 1, country = inputISOLower)
  # }
  
  #print(Level1Identifier$NAME_1) # List of all states/provinces/regions
  
  Level1Raster <- raster(Level1Identifier, resolution = res(Susceptible)[1])
  Level1Raster <- rasterize(Level1Identifier, Level1Raster) # May take a few seconds to run
  Level1Raster <- replace(Level1Raster, is.na(Level1Raster), 0)
  
  #print(table(values(Level1Raster)))
  
  # Level1Raster <- round(resample(Level1Raster, Susceptible, method = "bilinear"))
  # print(table(values(Level1Raster)))
  
  Level1Raster <-  round(resample(Level1Raster, Susceptible, method = "ngb"))
  
  #print(table(values(Level1Raster)))
  
  Level1Raster <- replace(Level1Raster, values(Level1Raster) < 0, 0)
  # Unless you are using method = "ngb" the above line is needed for some countries.
  # The other method is called "bilinear"
  
  # print(table(values(Level1Raster)))
  
  # print(dim(Level1Raster)); print(dim(Susceptible))
  # 
  # print(res(Level1Raster)); print(res(Susceptible))
  # 
  # print(origin(Level1Raster)); print(origin(Susceptible))
  
  # print(extent(Level1Raster))
  # print(extent(Susceptible))
  
  #extent(Level1Identifier) <- extent(Susceptible)
  
  #print(extent(Level1Identifier))
  
  rasterStack <- stack(Susceptible, Vaccinated, Exposed, Infected, Recovered, Dead, Inhabitable, Level1Raster)
  
  names(rasterStack) <- c("Susceptible", "Vaccinated", "Exposed", "Infected", "Recovered", "Dead", "Inhabitable", "Level1Raster")
  
  #print(Level1Identifier)
  
  #print(rasterStack)

  returnList <- list("rasterStack" = rasterStack, "Level1Identifier" = Level1Identifier, "selectedCountry" = selectedCountry, "rasterAgg" = rasterAgg, "WorldPopRows" = nrow(WorldPop), "WorldPopCols" = ncol(WorldPop), "WorldPopCells" = ncell(WorldPop))
  
  return(returnList)
}

#------------------------#
# Example Function Calls #
#------------------------#
# #set working directory to source file location if this function is to be tested standalone
#
# rs <- createRasterStack("Czech Republic", 10)
# rs
# names(rs)
#
# rs$rasterStack$Susceptible
#
# createRasterStack("Nigeria", 25)
# 
# createRasterStack("Czech Republic", 10)
# 
# createRasterStack("Latvia", 10)
