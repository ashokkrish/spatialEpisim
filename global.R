options(conflicts.policy = list(warn = FALSE))
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(av))
shhh(library(bslib))
shhh(library(cptcity))
shhh(library(countrycode))
shhh(library(deSolve))
shhh(library(dplyr))
shhh(library(ggplot2))
shhh(library(latex2exp))
shhh(library(lattice))
shhh(library(latticeExtra))
shhh(library(maps))
shhh(library(markdown))
shhh(library(purrr))
options("rgdal_show_exportToProj4_warnings"="none")
# shhh(library(raster, warn.conflicts=FALSE)) # classes and functions for raster data
shhh(library(rasterVis))
shhh(library(readxl))
shhh(library(writexl))
shhh(library(rgdal, warn.conflicts=FALSE))
shhh(library(sf))     # classes and functions for vector data
shhh(library(shiny))
shhh(library(shinyalert))
shhh(library(shinyhelper))
shhh(library(shinyjs))
shhh(library(shinyvalidate))
shhh(library(shinyWidgets))
shhh(library(sp))
shhh(library(stringr))
shhh(library(terra, warn.conflicts=FALSE))  # suppressWarnings(suppressMessages(library(terra)))
shhh(library(tidyverse))
shhh(library(tinytex))

population <- read_excel("misc/population.xlsx", 1)
epiparms <- read_excel("misc/epiparms.xlsx", 1)
#print(epiparms)

fieldsMandatory <- c("selectedCountry", "seedData")

#hoverDrop <- "selectedCountry"

# labelMandatory <- function(label) {
#   tagList(
#     label,
#     span("*", class = "mandatory_star")
#   )
# }

highlightDrop <- function(menu) {
  tagList(
    menu, 
    span(class = "dropDown")
  )
}

appCSS <- ".mandatory_star {color: red;}"
appCSS <- ".invisible {display:none;}"
appCSS <- ".dropDown:hover {color:ADD8E6;background-color: #000000}"
