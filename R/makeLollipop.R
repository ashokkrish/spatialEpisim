library(ggplot2)
library(readr)
library(readxl)

plotLolliChart <- function(filename) {
  
  data <- data.frame(
    x = colnames(data),
    y = colSums(data)
  )
  
  # plot <- ggplot(newData, 
  #                aes(Count, HealthZone, color = "Red")) +
  #         geom_segment(aes(x = Count, y = HealthZone, xend = Count, yend = HealthZone), color = "grey50") +
  #         geom_point()
  
  plot <- ggplot(data, aes(x=x, y=y)) +
    geom_segment( aes(x=x, xend=x, y=0, yend=y), color="black") +
    geom_point( color="#18536F", size=3) +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank()
    )

  plot
}

openDataFile <- function(filename) {
  ext <- tools::file_ext(filename)
  ext <- tolower(ext)
  
  switch(ext, 
         csv = read_csv(filename, show_col_types = FALSE),
         xls = read_xls(filename),
         xlsx = read_xlsx(filename),
         txt = read_tsv(filename, show_col_types = FALSE),
  )
}

plotLolliChart("C:/Users/Cupcake/Documents/MRU/Year 4/Winter/Senior Project/spatialEpisim/observeddata/Ebola_Death_Data.xlsx")