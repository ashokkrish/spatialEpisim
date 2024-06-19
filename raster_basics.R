here::i_am("install_packages.R")

# To run the code presented in this short-course you will need to have installed
# the following packages: Be sure to update Java to its newest version
packages = c("rsconnect",     "av",           "bslib",
             "countrycode",   "cptcity",      "deSolve",
             "dplyr",         "DT",           "ggplot2",
             "htmltools",     "latex2exp",    "lattice",
             "latticeExtra",  "leaflet",      "maps",
             "markdown",      "plotly",       "purrr",
             "rasterVis",     "readr",        "readxl",
             "sf",            "shiny",        "shinyalert",
             "shinybusy",     "shinyhelper",  "shinyjs",
             "shinyvalidate", "shinyWidgets", "sp",
             "stringr",       "terra",        "tidyverse",
             "tinytex",       "tools",        "writexl",
             "fasterize",     "magick",       "raster",
             "rstudioapi",    "Matrix",       "lubridate",
             "plot3D",        "colourpicker", "ggspatial")

packagesNotInstalled <-
  packages[!(packages %in% installed.packages()[, "Package"])]

if (length(packagesNotInstalled))
  install.packages(packagesNotInstalled,
                   dependencies = TRUE,
                   repos = "https://cloud.r-project.org")

suppressMessages(
  suppressPackageStartupMessages(
    sapply(packages, require, character.only = TRUE, simplify = TRUE)
  )
)

# It is worth keeping your packages and R version up-to-date. To ensure the
# former, run:
suppressWarnings(update.packages(oldPkgs = packages,
                                 repos = "https://cloud.r-project.org"))

source("R/rasterBasePlot.R")
source("R/rasterWorldPop.R")

##############################
## ┓ ┏┓┏┳┓  ┳┳┏┓  ┳┓┏┓┏┓┳┳┓╻ #
## ┃ ┣    ┃      ┣┫┣ ┃ ┓┃┃┃┃ #
## ┗┛┗┛ ┻   ┗┛┗┛  ┻┛┗┛┗┛┻┛┗• #
##############################

selectedCountry <- "Greece"
selectedCountry <- "Italy"
## selectedCountry <- "Korea"
## selectedCountry <- "Nigeria"

## rasterAgg <- 5
rasterAgg <- 10
## rasterAgg <- 15

## Converts country name to ISO Alpha
inputISO <- countrycode(selectedCountry,
                        origin = 'country.name',
                        destination = 'iso3c')
inputISOLower <- tolower(inputISO)

## Construct a URL from which to download a TIF file.
paste("https://data.worldpop.org",
      "GIS",
      "Population",
      "Global_2000_2020_1km_UNadj",
      "2020",
      inputISO,
      "%s_ppp_2020_1km_Aggregated_UNadj.tif",
      sep = "/") %>%
  sprintf(tolower(inputISO)) ->
  url

tifPath <- here("tif", basename(url))
if (!file.exists(tifPath))
  download.file(url, tifPath, mode = "wb")

WorldPop <- raster(tifPath)
## WorldPop <- raster(file.choose())

WorldPop

res(WorldPop)

extent(WorldPop)

origin(WorldPop)

crs(WorldPop)

nrow(WorldPop)

ncol(WorldPop)

ncell(WorldPop)

dim(WorldPop)

## Number of grid cells that have a population count = 0
cellStats(WorldPop == 0, sum)

## Number of grid cells that have a population count
cellStats(!is.na(WorldPop), sum)

## Number of grid cells that are NA
cellStats(is.na(WorldPop), sum)

## Total estimated 2020 population count
cellStats(WorldPop, sum)

#---------------------------------------#
# Source 2: From GADM: Level1Identifier #
#---------------------------------------#
## ?readRDS

# Level1Identifier <- gadm(country = inputISO,
#                          level = 1,
#                          version = 3.6,
#                          path = here())

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
## plot(WorldPop)

## plot(WorldPop, col = terrain.colors(255))

## par(mfrow = c(1, 2))
## image(log(WorldPop),
##       col = heat.colors(10),
##       main = sprintf("%s\n%s",
##                      "heat: 2020 UN-Adjusted Population Count (log-scale)",
##                      "(each grid cell is 1 km x 1 km)"))
## image(log(WorldPop),
##       col = topo.colors(10),
##       main = sprintf("%s\n%s",
##                      "topo: 2020 UN-Adjusted Population Count (log-scale)",
##                      "(each grid cell is 1 km x 1 km)"))

## plot(log(WorldPop),
##      xlab = "Longitude",
##      ylab = "Latitude",
##      main = sprintf("%s\n%s",
##                     "2020 UN-Adjusted Population Count (log-scale)",
##                     "(each grid cell is 1 km x 1 km)"))

# createBasePlot(selectedCountry = selectedCountry,
#                rasterAgg = 0,
#                directOutput = TRUE)

suscLayer <- createSusceptibleLayer(selectedCountry, 0)

createBasePlot(
  selectedCountry = selectedCountry,
  susceptible = suscLayer$Susceptible,
  directOutput = TRUE
)

#----------------------------------------#
# Switch back to PowerPoint Presentation #
#----------------------------------------#

#----------------------#
# AGGREGATING A RASTER #
#----------------------#

is.na(WorldPop) <- 0
WorldPop_aggr <- aggregate(WorldPop,
                           fact = c(rasterAgg, rasterAgg),
                           fun = sum,
                           na.rm = TRUE)
is.na(WorldPop_aggr) <- 0
names(WorldPop_aggr) <- "Susceptible"
WorldPop_aggr

summary(getValues(WorldPop_aggr))

print(sprintf("Population count before and after aggregation is equal? %s",
              cellStats(WorldPop, sum) == cellStats(WorldPop_aggr, sum)))

#----------------------------------------#
# Switch back to PowerPoint Presentation #
#----------------------------------------#

#--------------------#
# EXPORTING A RASTER #
#--------------------#
## writeRaster(WorldPop,
##             filename = sprintf("%s_unaggregated.nc", inputISO),
##             format = "CDF",
##             varname = "Susceptible",
##             varunit = "Persons",
##             longname = "Susceptible",
##             overwrite = TRUE)

## writeRaster(WorldPop_aggr,
##             filename = sprintf("%s_aggr_0000.nc", inputISO),
##             format = "CDF",
##             varname = "Susceptible",
##             varunit = "Persons",
##             longname = "Susceptible",
##             overwrite = TRUE)

# writeRaster(WorldPop_aggr,
#             filename = sprintf("%s_aggr_0000.tif", inputISO),
#             format = "GTiff",
#             varname = "Susceptible",
#             varunit = "Persons",
#             longname = "Susceptible",
#             overwrite = TRUE)

#----------------------------------------#
# Switch back to PowerPoint Presentation #
#----------------------------------------#
