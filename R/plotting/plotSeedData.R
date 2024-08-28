library(ggplot2)
library(dplyr)
library(sf)
library(ggspatial)
library(maps)
library(countrycode)
library(raster)
#library(ggforce)

createBubblePlot <- function(countryName, level1Names = NULL, displayCountryName, uploadedSeedData, startDate, source) {

  worldmap <- map_data("world")
  
  level1Identifier <- readRDS(paste0("gadm/", "gadm36_", countrycode(countryName, "country.name", "iso3c"), "_1_sp.rds"))
  
  if(!is.null(level1Names)){
    level1Identifier <- level1Identifier[which(level1Identifier$NAME_1 %in% level1Names), ]}
  
  worldmap2 <- dplyr::filter(worldmap, region == countryName) %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    st_combine() %>%
    st_cast("POLYGON")
  
  base_map <- ggplot(worldmap2) +
    geom_polygon(data = level1Identifier, aes(x = long, y = lat, group = group), color = "black", fill = "white") +
    annotation_north_arrow() +
    annotation_scale(location = "tl") +
    labs(
      title = paste0("Inital infections in ", displayCountryName), x = expression(bold("Longitude")), y = expression(bold("Latitude")),
      caption = source
    ) + theme(plot.title = element_text(hjust = 0.5))
  
  #print(base_map)
  
  my_df <- read.csv(paste0("seeddata/", uploadedSeedData), header = T)
  
  # print(my_df) # This should print the CSV file rows and columns
  # names(my_df)
  # dim(my_df)
  
  my_df <- my_df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  #print(class(my_df))
  base_map <- base_map +
    geom_sf(data = my_df, aes(color = InitialInfections, size = InitialInfections), alpha = 0.5) +
    #geom_point(data = my_df, aes(x = long, y = lat), size = 1) +
    guides(colour = guide_legend(override.aes = list(size = 8))) +
    scale_color_continuous(name = "Active Cases", low = "blue", high = "red", na.value = "grey50") +
    scale_size_continuous(name = "Active Cases", range = c(5, 10)) +
    theme(legend.position = "bottom", legend.text.align = 1, legend.title.align = 0.5) +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), axis.line = element_line(colour = "black"))  # Removes background grid lines
  #geom_circle() + coord_equal() + theme_classic()
  base_map
}

#------------------------#
# Example Function Calls #
#------------------------#

# createBubblePlot(countryName = "Democratic Republic of Congo", level1Names = c("Ituri", "Nord-Kivu"), displayCountryName = "the DRC", uploadedSeedData = "COD_InitialSeedData.csv", startDate = "2018-08-01", source = "(Source: WHO)")
# 
# createBubblePlot(countryName = "Democratic Republic of Congo", level1Names = NULL, displayCountryName = "the DRC", uploadedSeedData = "COD_InitialSeedData.csv", startDate = "2018-08-01", source = "(Source: WHO)")
# 
# createBubblePlot(countryName = "Nigeria", level1Names=NULL, displayCountryName = "Nigeria", uploadedSeedData = "NGA_InitialSeedDataSep 1, 2020.csv", startDate = "2020-09-01", source = "(Source: Nigerian Centre for Disease Control)")
