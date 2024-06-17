# To run the code presented in this short-course you will need to have installed the following packages:

# ipak function: install and load multiple R packages.
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
# Be sure to update Java to its newest version

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("av", "bslib", "colorRamps", "countrycode", "cptcity", "deSolve", "dplyr", "DT", "fasterize", "ggplot2", "ggmap", "ggspatial", "htmltools", "latex2exp", "lattice", "latticeExtra", "leaflet", "lubridate", "magick", "maps", "markdown", "ncdf4",
              "plotly", "purrr", "raster", "rasterVis", "Rcpp", "readr", "readxl", "rgdal", "rjson", "rosm", "rsconnect", "rstudioapi", "sp", "sf", "shiny", "shinyalert", "shinybusy", "shinyjs", "shinyhelper", "shinyWidgets", "stringr", "shinyFeedback",
              "shinyvalidate", "terra", "tidyverse", "tinytex", "viridis", "writexl")
ipak(packages)

# To see if the needed packages are installed correctly
lapply(packages, require, character.only = TRUE)

#It is worth keeping your packages and R version up-to-date. To ensure the former, run:
update.packages(oldPkgs = packages)