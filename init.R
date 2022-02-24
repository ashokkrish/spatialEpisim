my_packages = c("av", "countrycode", "cptcity", "dplyr", "fasterize", "ggplot2",
                "lattice" , "latticeExtra" , "lubridate", "magick", "maps", "markdown",
                "purrr", "raster", "rasterVis", "Rcpp", "readxl", "rgdal", "rstudioapi",
                "shiny", "shinyalert", "shinyFeedback", "shinyhelper", "shinyjs",
                "shinyvalidate", "shinyWidgets", "sf", "sp", "stringr", "terra", "xlsx")

install_if_missing = function(p) {
     if (p %in% rownames(installed.packages()) == FALSE) {
          install.packages(p)
     }
}

invisible(sapply(my_packages, install_if_missing))