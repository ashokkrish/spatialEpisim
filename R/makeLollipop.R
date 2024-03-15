library(ggplot2)
library(readr)
library(readxl)

plotLolliChart <- function(filename) {
  
  file <- as.data.frame(openDataFile(filename))
  data <- file[,3:ncol(file)]
  chartData <- data.frame(x = colnames(data),
                          y = colSums(data))
  
  plotTitle <- paste("Total Observed Persons per Health Zone from", file[1,"Date"], "to", file[nrow(file), "Date"])
  
  plot <- ggplot(chartData, aes(x=x, y=y)) +
    geom_segment( aes(x=x, xend=x, y=0, yend=y), color="black") +
    geom_point( color="#18536F", size=3, position = position_dodge2(0.5)) +
    labs(title = plotTitle,
         x = "Health Zone", # x and y labels will be flipped
         y = "Total Observed Persons") +
    theme_light() +
    coord_flip() +
    theme(
      plot.title = element_text(size = 28, 
                                face = "bold", 
                                hjust = 0.5,
                                margin = margin(0, 0, 50, 0)),
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1),"cm"),
      axis.title.x = element_text(size = 20, 
                                  face = "bold", 
                                  vjust = -1.5,
                                  margin = margin(25, 0, 0, 0)),
      axis.title.y = element_text(size = 20, 
                                  face = "bold",
                                  margin = margin(0, 25, 0, 0)),
      axis.text.x.bottom = element_text(size = 14),
      axis.text.y.left = element_text(size = 14),
      axis.ticks.y = element_blank()
    ) +
    scale_y_continuous(n.breaks = 8)

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

# plotLolliChart("observeddata/Ebola_Incidence_Data.xlsx")
