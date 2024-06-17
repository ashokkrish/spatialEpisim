options(conflicts.policy = list(warn = FALSE),
        shiny.reactlog = TRUE)
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(av))
shhh(library(bslib))
shhh(library(cptcity))
shhh(library(countrycode))
shhh(library(deSolve))
shhh(library(dplyr))
shhh(library(DT))
shhh(library(ggplot2))
shhh(library(htmltools))
shhh(library(latex2exp))
shhh(library(lattice))
shhh(library(latticeExtra))
shhh(library(leaflet))
shhh(library(maps))
shhh(library(markdown))
shhh(library(plotly))
shhh(library(purrr))

## MAYBE TODO: https://rgdal.r-forge.r-project.org/reference/showWKT.html.
## Investigate the consequences of this option and document why it is set. Why
## are warnings on exportToProj4 disabled? Why were warnings encountered
## (presumably) when this option was originally set?
options("rgdal_show_exportToProj4_warnings"="none")

shhh(library(rasterVis))
shhh(library(readr))
shhh(library(readxl))
shhh(library(writexl))
shhh(library(sf))     # classes and functions for vector data
shhh(library(shiny))
shhh(library(shinyalert))
shhh(library(shinybusy))
shhh(library(shinyhelper))
shhh(library(shinyjs))
shhh(library(shinyvalidate))
shhh(library(shinyWidgets))
shhh(library(sp))
shhh(library(stringr))
shhh(library(terra, warn.conflicts=FALSE))  # suppressWarnings(suppressMessages(library(terra)))
shhh(library(tidyverse))
shhh(library(tinytex))
shhh(library(here))

here::i_am("global.R")

source("R/cropBaseRasterHaxby.R")
source("R/makePlots.R")
source("R/plotOptionsMenu.R")
source("R/rasterBasePlot.R")
source("R/rasterCropSeedPlot.R")
source("R/rasterCumulativePlot.R")
source("R/rasterLeafletPlots.R")
#source("R/rasterSimulation.R")
source("R/rasterSimulation_DA.R")
source("R/rasterStack.R")
source("R/WorldPopPlots.R")

options(scipen = 999)

epiparms <- read_excel("misc/epiparms.xlsx", 1)

## List of countries that need "the" prepended to their name
prependList <- c("Czech Republic",
                 "Democratic Republic of Congo",
                 "Gambia",
                 "Netherlands")

valueRange <- c(0, 5, 10, 25, 50, 100, 250, 1000, 10000)
ramp <- c("#FFFFFF",
          "#D0D8FB",
          "#BAC5F7",
          "#8FA1F1",
          "#617AEC",
          "#0027E0",
          "#1965F0",
          "#0C81F8",
          "#18AFFF",
          "#31BEFF",
          "#43CAFF",
          "#60E1F0",
          "#69EBE1",
          "#7BEBC8",
          "#8AECAE",
          "#ACF5A8",
          "#CDFFA2",
          "#DFF58D",
          "#F0EC78",
          "#F7D767",
          "#FFBD56",
          "#FFA044",
          "#EE4F4D")
pal <- colorRampPalette(ramp)
colorPalette <- colorBin(pal(9)[-1], domain = valueRange, bins = valueRange)

highlightDrop <- \(menu) tagList(menu, span(class = "dropDown"))

## MAYBE TODO: replace with read_table?
openDataFile <- function(datafile) {
  ext <- tools::file_ext(datafile$name)
  ext <- tolower(ext)

  switch(ext,
         csv = read_csv(datafile$datapath, show_col_types = FALSE),
         xls = read_xls(datafile$datapath),
         xlsx = read_xlsx(datafile$datapath),
         txt = read_tsv(datafile$datapath, show_col_types = FALSE),

         validate("Improper file format."))
}

## TODO: move to the CSS file in www/.
appCSS <-
  list(".mandatory_star { color: red; }",
       ".invisible { display:none; }",
       ".dropDown:hover { color:ADD8E6; background-color: #000000; }") |>
  reduce(\(...) paste(..., sep = ""))

acceptedFileTypes <- c("text/csv",
                       "text/comma-separated-values",
                       "text/plain",
                       ".csv",
                       ".xls",
                       ".xlsx",
                       ".txt")

updateNumericInputs <- function(defaults, session) {
  if (any(is.null(dim(defaults)), dim(defaults)[1] != 1)) {
    warning("The `defaults` dataframe is not a single row!")
    warning(defaults) # DONT remove this. It's intentional, not for development.
  }
  iwalk(defaults, \(value, inputId) {
    updateNumericInput(session, inputId, value = value)
  })
}

population <- read_xlsx("misc/population.xlsx")
