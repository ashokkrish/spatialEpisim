library(ggplot2)
library(dplyr)
library(sf)
library(ggspatial)
library(maps)
library(countrycode)
library(raster)
#library(ggforce)

createSeedPlot <- function(countryName, displayCountryName, seedData, startDate, source) {

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
                  title = paste0("COVID-19 in ", displayCountryName), x = expression(bold("Longitude")), y = expression(bold("Latitude")),
                   caption = source
          ) + theme(plot.title = element_text(hjust = 0.5))
  
  #print(base_map)
  
  my_df <- read.csv(paste0("seeddata/", countrycode(countryName, "country.name", "iso3c"), "_InitialSeedDataSep 1, 2020.csv"), header = T)
  
  #print(my_df) # This should print the CSV file rows and columns
  #names(my_df)
  #dim(my_df)
  
  my_df <- my_df %>% 
          st_as_sf(coords = c("lon", "lat"), crs = 4326)
  #print(class(my_df))
  base_map <- base_map +
       geom_sf(data = my_df, aes(color = InitialInfections, size = InitialInfections), alpha = 0.5) +
       #geom_point(data = my_df, aes(x = long, y = lat), size = 1) +
       guides(colour = guide_legend(override.aes = list(size = 8))) +
       scale_color_continuous(name = "Active Cases", low = "blue", high = "red", na.value = "grey50") +
       scale_size_continuous(name = "Active Cases", range = c(0, 20)) +
       theme(legend.position = "bottom", legend.text.align = 1, legend.title.align = 0.5) + 
       theme(panel.border = element_blank(), panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))  # Removes background grid lines
       #geom_circle() + coord_equal() + theme_classic()
  base_map
}

#------------------------#
# Example Function Calls #
#------------------------#

#createSeedPlot(countryName = "Czech Republic", displayCountryName = "the Czech Republic", seedData = "seeddata/CZE_InitialSeedDataSep 1, 2020.csv", startDate = "2020-09-01", source = "(Source: Ministerstvo zdravotnictví Ceské republiky)")

#createSeedPlot(countryName = "Nigeria", displayCountryName = "Nigeria", seedData = "seeddata/NGA_InitialSeedDataSep 1, 2020.csv", startDate = "2020-09-01", source = "(Source: Nigerian Centre for Disease Control)")
