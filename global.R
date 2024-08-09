## MAYBE TODO: https://rgdal.r-forge.r-project.org/reference/showWKT.html.
## Investigate the consequences of this option and document why it is set. Why
## are warnings on exportToProj4 disabled? Why were warnings encountered
## (presumably) when this option was originally set?
options("rgdal_show_exportToProj4_warnings" = "none",
        conflicts.policy = list(warn = FALSE),
        scipen = 999)

## MAYBE FIXME: I have a strong feeling not all of these libraries are actually
## used in spatialEpisim.
suppressPackageStartupMessages({
  library(av)
  library(bslib)
  library(cptcity)
  library(countrycode)
  library(deSolve)
  library(DT)
  library(htmltools)

  library(lattice)
  library(latticeExtra)

  library(leaflet)
  library(maps)

  library(markdown)

  library(plotly)
  library(rasterVis)

  library(readxl)
  library(writexl)

  library(sf)
  library(sp)
  library(terra)

  library(shiny)
  library(shinyalert)
  library(shinybusy)
  library(shinyhelper)
  library(shinyjs)
  library(shinyvalidate)
  library(shinyWidgets)

  library(tidyverse)
  library(here)

  library(urltools)
  library(tools)
})

## TODO: include instructions in the README on installing the following package;
## it's quite easy.
library(spatialEpisim.foundation)

here::i_am("global.R")

## NOTE: this is alike the parameter data in (non-spatial) Episim. It provides
## useful defaults for various combinations of model parameters and options.
epiparms <- read_excel(here("data", "misc", "epiparms.xlsx"))

## List of countries that need "the" prepended to their name
prependList <- c("Czech Republic",
                 "Democratic Republic of Congo",
                 "Gambia",
                 "Netherlands")

## TODO: Why are we creating our own palette when many others already exist? Use
## an existing one which is loaded from a package we're already using. Don't
## include a new package though, unless the palette is really pretty and useful.
## ## MAYBE FIXME: if this is that palette that Ashok likes, we can probably get
## it from a package and not need to hard-code it.
valueRange <- c(0, 5, 10, 25, 50, 100, 250, 1000, 10000)
colourPalette <-
  colorBin((colorRampPalette(c("#FFFFFF", "#D0D8FB", "#BAC5F7", "#8FA1F1",
                               "#617AEC", "#0027E0", "#1965F0", "#0C81F8",
                               "#18AFFF", "#31BEFF", "#43CAFF", "#60E1F0",
                               "#69EBE1", "#7BEBC8", "#8AECAE", "#ACF5A8",
                               "#CDFFA2", "#DFF58D", "#F0EC78", "#F7D767",
                               "#FFBD56", "#FFA044", "#EE4F4D")))(9)[-1],
           domain = valueRange,
           bins = length(valueRange))

acceptedFileTypes <- c("text/csv",
                       "text/comma-separated-values",
                       "text/plain",
                       ".csv",
                       ".xls",
                       ".xlsx",
                       ".txt")

## MAYBE TODO: replace with read_table?
openDataFile <- function(datafile) {
  switch(tolower(tools::file_ext(datafile$name)),
         csv = read_csv(datafile$datapath, show_col_types = FALSE),
         xls = read_xls(datafile$datapath),
         xlsx = read_xlsx(datafile$datapath),
         txt = read_tsv(datafile$datapath, show_col_types = FALSE),

         validate("Improper file format."))
}

## NOTE: borrowed from Episim; the features I use—validated inputs, inputs
## updated with rows of a dataframe filtered on some input, et cetera—should be
## made into a package that both use and which others can use.
updateNumericInputs <- function(defaults, session) {
  if (any(is.null(dim(defaults)), dim(defaults)[1] != 1)) {
    warning("The `defaults` dataframe is not a single row!")
    warning(defaults) # DONT remove this. It's intentional, not for development.
  }
  iwalk(defaults, \(value, inputId) {
    updateNumericInput(session, inputId, value = value)
  })
}

## TODO: population is filtered by shortlist == TRUE in both ui.R and
## WorldPopViz.R; that work should only be done once. FIXME: it would be better
## to change this to internal data in a package than ship this as a file and
## depend on read_xlsx.
population <- read_xlsx(here("data", "misc", "recommendedRasterAggregationFactors.xlsx"))

lineThickness <- 1.5

shinyAppDir(here::here()) # and hurrah!
