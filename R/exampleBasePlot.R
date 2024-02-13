# source("R/rasterWorldPop.R")
# source("R/rasterBasePlot.R")
# source("R/rasterLeafletPlot.R")
# 
# selectedCountry <- "Belgium"
# 
# susceptible <- createSusceptibleLayer(selectedCountry, rasterAgg = 0, isCropped = F, level1Names = NULL)$Susceptible
# 
# createBasePlot(selectedCountry, susceptible, directOutput = F)
# 
# createLeafletPlot(selectedCountry, susceptible)

# Notes on running an example
#-----------------------------
# Make sure before running this file to set your working directory to the root 
# spatialEpisim folder ( /spatialEpisim ) or to a folder that contains an R folder with a copy of the 
# sourced scripts and a www folder for the baseplot output:
#     This can be done by opening the 'Files' window in RStudio, changing to the 
#     desired location, then selecting settings -> Set As Working Directory.
#
# Alternatively, you can physically relocate the exampleBasePlot.R file to a 
# location as described above.
#
# example directory structure if not using /spatialEpisim:
#
#   /rootFolder
#     -> exampleBasePlot.R
#     -> /R
#         -> rasterWorldPop.R
#         -> rasteBasePlot.R
#         -> rasterLeafletPlot.R
#     -> /www
#