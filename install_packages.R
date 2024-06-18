# To run the code presented in this short-course you will need to have installed the following packages:
# Be sure to update Java to its newest version
packages = c("rsconnect",  "av",          "bslib",     "countrycode",   "cptcity",
             "deSolve",    "dplyr",       "DT",        "ggplot2",       "here",
             "htmltools",  "latex2exp",   "lattice",   "latticeExtra",  "leaflet",
             "maps",       "markdown",    "plotly",    "purrr",         "rasterVis",
             "readr",      "readxl",      "sf",        "shiny",         "shinyalert",
             "shinybusy",  "shinyhelper", "shinyjs",   "shinyvalidate", "shinyWidgets",
             "sp",         "stringr",     "terra",     "tidyverse",     "tinytex",
             "tools",      "writexl",     "fasterize", "magick",        "raster",
             "rstudioapi", "Matrix",      "lubridate", "plot3D",        "colourpicker",
             "ggspatial")

packagesNotInstalled <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(packagesNotInstalled))
  install.packages(packagesNotInstalled, dependencies = TRUE, repos = "https://cloud.r-project.org")

suppressMessages(
  suppressPackageStartupMessages(
    sapply(packages, require, character.only = TRUE, simplify = TRUE)
  )
)

# It is worth keeping your packages and R version up-to-date. To ensure the former, run:
suppressWarnings(update.packages(oldPkgs = packages, repos = "https://cloud.r-project.org"))
