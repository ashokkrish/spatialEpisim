# rm(list = ls())

library(ggplot2)
library(dplyr)
library(sf)
library(ggspatial)
library(maps)
library(countrycode)
library(raster)

#------------------------------------------------------------------------------#
# Set your working directory                                                   #
#------------------------------------------------------------------------------#
createSeedPlot <- function(countryName, seedData, startDate, source) {
  # setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # RStudio IDE preferred
  # getwd()                                                     # Path to your working directory
  
  worldmap <- map_data("world")
  level1Identifier <- readRDS(paste0("gadm/", "gadm36_", countrycode(countryName, "country.name", "iso3c"), "_1_sp.rds"))

  worldmap2 <- dplyr::filter(worldmap, region == countryName) %>% 
          st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
          st_combine() %>% 
          st_cast("POLYGON")
  
  base_map <- ggplot(worldmap2) +
          geom_polygon(data = level1Identifier, aes(x = long, y = lat, group = group), color = "black", fill = "white") +
          annotation_north_arrow() +
          annotation_scale(location = "tl") +
          labs(
                  title = paste0("COVID-19 in the ", countryName), subtitle = paste0("As of ", startDate), x = expression(bold("Longitude")), y = expression(bold("Latitude")),
                   caption = "(Source: Ministerstvo zdravotnictví Ceské republiky)" # TODO: include dynamic source?
          )
  
  #print(base_map)
  
  my_df <- read.csv(paste0("seeddata/", countrycode(countryName, "country.name", "iso3c"), "_InitialSeedDataSep 1, 2021.csv"), header = T) # TODO: use parameter directly? potential errors
  
  # In the above line the seed data file name is hard coded it should dynamically change from 2020 to 2021
  # for any month and any date
  
  #print(class(my_df))
  #print(my_df)
  #names(my_df)
  #dim(my_df)
  
  my_df <- my_df %>% 
          st_as_sf(coords = c("lon", "lat"), crs = 4326)
  #print(class(my_df))
  base_map <- base_map +
          geom_sf(data = my_df, aes(color = InitialInfections, size = InitialInfections)) +
          #geom_point(data = my_df, aes(x = long, y = lat), size = 1) +
          guides(colour = guide_legend(override.aes = list(size = 8))) +
          scale_color_continuous(name = "Active Cases", low = "blue", high = "red", na.value = "grey50") +
          scale_size_continuous(name = "Active Cases") +
          theme(legend.position = "bottom", legend.text.align = 1, legend.title.align = 0.5) + 
          theme(panel.border = element_blank(), panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))  # Removes background grid lines
          
  base_map
}

#------------------------------------------------------------------------------#
# Example Function Calls                                                       #
#------------------------------------------------------------------------------#
 createSeedPlot(countryName = "Czech Republic", seedData = "seeddata/CZE_InitialSeedDataSep 1, 2020.csv", startDate = "2020-09-01", source = "testSource")
# createSeedPlot(countryName = "Nigeria", seedData = "seeddata/NGA_InitialSeedData.csv", startDate = "2021-04-01", source = "testSource")
 
# # createLevel1Plot(countryName = "Czech Republic")
# createLevel1Plot <- function(countryName) {
#   worldmap <- raster(paste0("tif/", tolower(countrycode(countryName, "country.name", "iso3c")), "_ppp_2020_1km_Aggregated_UNadj.tif")) # name of the country .tif file
#   Level1Identifier <- readRDS(paste0("gadm/", "gadm36_", countrycode(countryName, "country.name", "iso3c"), "_1_sp.rds"))  # name of the Level 1 Identifier .rds file
#   
#   Level1Plot <- raster(Level1Identifier, resolution = res(worldmap)[1])
#   Level1Plot <- rasterize(Level1Identifier, Level1Plot)
#   
#   origin(Level1Plot) <- origin(worldmap)
#   Level1Plot <- crop(Level1Plot, worldmap)
#   plot(Level1Plot, main = paste0("COVID-19 in the ", countryName))
# }