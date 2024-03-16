library(ggplot2)
library(readr)
library(readxl)

plotLolliChart <- function(filename) {
  
  file <- as.data.frame(openDataFile(filename))
  data <- file[,3:ncol(file)]
  chartData <- data.frame(x = colnames(data),
                          y = colSums(data))
  
  plotTitle <- paste("Cumulative Cases per Location from", file[1,"Date"], "to", file[nrow(file), "Date"])
  
  plot <- ggplot(chartData, aes(x=x, y=y)) +
    # geom_hline(yintercept = 0) +
    # geom_vline(xintercept = colnames(data)[1]) +
    geom_segment( aes(x=reorder(x, y), xend=reorder(x, y), y=0, yend=y), color="black") +
    geom_point( color="#18536F", size=5) +
    labs(title = plotTitle,
         x = "Location", # x and y labels will be flipped
         y = "Cumulative Cases") +
    theme_light() +
    coord_flip() +
    theme(
      plot.title = element_text(size = 28, 
                                face = "bold", 
                                hjust = 0.5,
                                margin = margin(0, 0, 50, 0)),
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(1, 1, 1, 0),"cm"),
      axis.title.x = element_text(size = 20, 
                                  face = "bold",
                                  margin = margin(25, 0, 0, 0)),
      axis.title.y = element_text(size = 20, 
                                  face = "bold",
                                  margin = margin(0, 25, 0, 0)),
      axis.text.x.bottom = element_text(size = 14),
      axis.text.y.left = element_text(size = 14),
      axis.ticks.y = element_blank(),
      axis.line = element_line(linewidth = 0.5)
    ) +
    scale_y_continuous(n.breaks = 8, expand = c(0, 0), limits = c(0,(max(chartData[,"y"] * 1.1))))

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
