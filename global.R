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

  library(readods)
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

shinyAppDir(here::here()) # and hurrah!
