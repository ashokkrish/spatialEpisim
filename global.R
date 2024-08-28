## FIXME: I have a strong feeling not all of these libraries are actually used
## in spatialEpisim. TODO: remove all packages which are dependencies of
## spatialEpisim.foundation; there's no need to load them twice.
suppressPackageStartupMessages({
  library(cptcity) # used just for seminf haxby colour palette
  library(countrycode)
  library(DT)

  library(htmltools)
  library(bslib)

  library(leaflet)
  library(maps)

  library(plotly)

  library(readxl)
  library(writexl)

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

  library(waiter)
})

## DONT suppress messages from our own back-end and front-end dependencies.
library(spatialEpisim.foundation)
library(spatialEpisim.data)
library(reactcharteditor)

here::i_am("global.R")

## These provide useful defaults for various combinations of model parameters
## and options, and a set of countries which the application is usable with.
epiparms <- load(here("data", "epiparms.RData"))
shortList <- load(here("data", "shortlist.RData"))

## FIXME: it is debatable whether "the" should be prepended to any country name!
prependList <- c("Czech Republic",
                 "Democratic Republic of Congo",
                 "Gambia",
                 "Netherlands")

grey6 <- "#0f0f0f" # the common name for this hexadecimal RGB colour

valueRange <- c(0, 5, 10, 25, 50, 100, 250, 1000, 10000)
colourPalette <-
  colorBin(cptcity::cpt("jjg_misc_seminf_haxby",
                        colorRampPalette = TRUE)(9)[-1],
           domain = valueRange,
           bins = length(valueRange))

mimetypes <- c("text/csv",
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

lineThickness <- 1.5

shinyAppDir(here::here()) # and hurrah!
