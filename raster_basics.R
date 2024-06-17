setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # RStudio IDE preferred
getwd() # Path to your working directory

source("#install_packages.R")
source("R/rasterBasePlot.R")
source("R/rasterWorldPop.R")
#source("#rasterBasePlot.R")

selectedCountry <- "Greece" # "Italy" # "Nigeria" # Korea"

rasterAgg <- 10 # 15 # 5 # 

inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c') #Converts country name to ISO Alpha
inputISOLower <- tolower(inputISO)

url <- paste("https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/2020/", inputISO, "/", inputISOLower, "_ppp_2020_1km_Aggregated_UNadj.tif", sep = "")

tifFileName <- basename(url)    # name of the .tif file
tifFolder <- "tif/"             # .tif files should be stored in local tif/ folder

if (!file.exists(paste(tifFolder, tifFileName, sep = "")))
{
  download.file(url, paste(tifFolder, tifFileName, sep = ""), mode = "wb")
}

WorldPop <- raster(paste(tifFolder, tifFileName, sep = "")) #raster(file.choose()) #

WorldPop

res(WorldPop)

extent(WorldPop)

origin(WorldPop)

crs(WorldPop)

nrow(WorldPop)

ncol(WorldPop)

ncell(WorldPop)

dim(WorldPop)

cellStats(WorldPop == 0, sum) # Number of grid cells that have a population count = 0

cellStats(!is.na(WorldPop), sum) # Number of grid cells that have a population count

cellStats(is.na(WorldPop), sum) # Number of grid cells that are NA

cellStats(WorldPop, sum) # Total estimated 2020 population count

#---------------------------------------#
# Source 2: From GADM: Level1Identifier #
#---------------------------------------#

gadmFileName <- paste("gadm36_", inputISOLower, "_1_sp.rds", sep = "")  # name of the .rds file

#print(gadmFileName)
#?readRDS

gadmFolder <- "gadm/"         # .rds files should be stored in local gadm/ folder

if (file.exists(paste(gadmFolder, gadmFileName, sep = ""))) {
     Level1Identifier <- readRDS(paste(gadmFolder, gadmFileName, sep = ""))
} else {
     Level1Identifier <- getData("GADM", level = 1, country = inputISOLower)
}

print(Level1Identifier$NAME_1) # List of all States/Provinces/Regions

#-----------------------------#
# PLOTTING A COUNTRY BOUNDARY #
#-----------------------------#

plot(Level1Identifier, main = "Level 1 Administrative Boundaries")

#-------------------#
# PLOTTING A RASTER #
#-------------------#

# plot(WorldPop)
#
# plot(WorldPop, col = terrain.colors(255))
#
# par(mfrow = c(1, 2))
# image(log(WorldPop), col = heat.colors(10), main = "heat: 2020 UN-Adjusted Population Count (log-scale) \n (each grid cell is 1 km x 1 km) \n")
# image(log(WorldPop), col = topo.colors(10), main = "topo: 2020 UN-Adjusted Population Count (log-scale) \n (each grid cell is 1 km x 1 km) \n")
#
# plot(log(WorldPop), xlab = "Longitude", ylab = "Latitude", main = "2020 UN-Adjusted Population Count (log-scale) \n (each grid cell is 1 km x 1 km) \n")
# 
# # createBasePlot(selectedCountry = selectedCountry, rasterAgg = 0, directOutput = TRUE)
# 

suscLayer <- createSusceptibleLayer(selectedCountry, 0)

#createBasePlot(selectedCountry = selectedCountry, susceptible = suscLayer$Susceptible, directOutput = TRUE) 

createBasePlot(selectedCountry = selectedCountry, susceptible = suscLayer$Susceptible, directOutput = FALSE) 

#----------------------------------------#
# Switch back to PowerPoint Presentation #
#----------------------------------------#

#----------------------#
# AGGREGATING A RASTER #
#----------------------#

WorldPop <- replace(WorldPop, is.na(WorldPop), 0)

WorldPop_aggr <- aggregate(WorldPop, fact = c(rasterAgg, rasterAgg), fun = sum, na.rm = TRUE)

names(WorldPop_aggr) <- "Susceptible"

WorldPop_aggr

summary(getValues(WorldPop_aggr))

# Check that the population count before and after aggregation are the same

cellStats(WorldPop, sum)

cellStats(WorldPop_aggr, sum)

#----------------------------------------#
# Switch back to PowerPoint Presentation #
#----------------------------------------#

#--------------------#
# EXPORTING A RASTER #
#--------------------#

writeRaster(WorldPop, filename = paste(inputISO, "_unaggregated.nc", sep = ""), format = "CDF",  varname = "Susceptible", varunit = "Persons", longname = "Susceptible", overwrite = TRUE)
 
writeRaster(WorldPop_aggr, filename = paste(inputISO, "_aggr_0000.nc", sep = ""), format = "CDF",  varname = "Susceptible", varunit = "Persons", longname = "Susceptible", overwrite = TRUE)
 
writeRaster(WorldPop_aggr, filename = paste(inputISO, "_aggr_0000.tif", sep = ""), format = "GTiff",  varname = "Susceptible", varunit = "Persons", longname = "Susceptible", overwrite = TRUE)

#----------------------------------------#
# Switch back to PowerPoint Presentation #
#----------------------------------------#
