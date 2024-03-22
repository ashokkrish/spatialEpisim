library(ggplot2)
library(readr)
library(readxl)

prependList <- c("Czech Republic", 
                 "Democratic Republic of Congo",
                 "Gambia",
                 "Netherlands")

plotLolliChart <- function(selectedCountry, filename) {
  
  file <- as.data.frame(openDataFile(filename))
  data <- file[,3:ncol(file)]
  chartData <- data.frame(x = colnames(data),
                          y = colSums(data))
  
  plotTitle <- paste0("Cumulative Cases in ")
  if(selectedCountry %in% prependList) {
    plotTitle <- paste0(plotTitle, "the ")
  }
  plotTitle <- paste0(plotTitle, selectedCountry, "\nfrom ", file[1,"Date"], " to ", file[nrow(file), "Date"])
  
  
  plot <- ggplot(chartData, aes(x=x, y=y)) +
    # geom_hline(yintercept = 0) +
    # geom_vline(xintercept = colnames(data)[1]) +
    geom_segment(aes(x=reorder(x, y), xend=reorder(x, y), y=0, yend=y), color="black") +
    geom_point(color="#18536F", size=2.5) +
    labs(title = plotTitle,
         x = "Location", # x and y labels will be flipped
         y = "Cumulative Cases") +
    theme_light() +
    coord_flip() +
    theme(
      plot.title = element_text(size = 24, 
                                face = "bold", 
                                hjust = 0.5,
                                margin = margin(0, 0, 25, 0)),
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(3, 2, 2, 2),"cm"),
      axis.title.x = element_text(size = 16, 
                                  face = "bold",
                                  margin = margin(25, 0, 0, 0)),
      axis.title.y = element_text(size = 16, 
                                  face = "bold",
                                  margin = margin(0, 10, 0, 0)),
      axis.text.x.bottom = element_text(size = 14),
      axis.text.y.left = element_text(size = 14),
      axis.ticks.y = element_blank(),
      axis.line = element_line(linewidth = 0.5)
    ) +
    scale_y_continuous(n.breaks = 8, expand = c(0, 0), limits = c(0,(max(chartData[,"y"] * 1.1))))

  plot
}

plotTimeSeries <- function(filename, selectedCountry) {
  
  file <- as.data.frame(openDataFile(filename))
  plotData <- data.frame(x = ymd(file[,"Date"]),
                          y = rowSums(file[,3:ncol(file)]))

  plotTitle <- paste0("Time-Series Graph of Daily Incidence/Death Rates \nin ")
  if(selectedCountry %in% prependList) {
    plotTitle <- paste0(plotTitle, "the ")
  }
  plotTitle <- paste0(plotTitle, selectedCountry)
  
  p = ggplot(plotData,
             aes(x = x, y = y)) +
    geom_line(color="#22031F", linewidth = 2) +
    geom_point(color="#22031F") +
    # geom_area(fill = "#22031F", alpha = 0.3) +
    labs(title = plotTitle, 
         x = "Date", 
         y = "Number of Persons") +
    scale_x_date(date_breaks = "2 months", date_labels = "%d %b %Y") +
    scale_y_continuous(minor_breaks = waiver()) +
    theme(
      plot.title = element_text(size = 18, 
                                face = "bold",
                                hjust = 0.5,
                                margin = margin(0, 0, 25, 0)),
      axis.title.x = element_text(size = 16, 
                                  face = "bold",
                                  margin = margin(25, 0, 0, 0)),
      axis.title.y = element_text(size = 16, 
                                  face = "bold",
                                  margin = margin(0, 25, 0, 0)),
      axis.text.x.bottom = element_text(size = 14),
      axis.text.y.left = element_text(size = 14),
      axis.line = element_line(linewidth = 0.5),
      plot.margin = unit(c(3, 2, 2, 2),"cm")
    ) 
    # scale_color_gradient(low="#9E929D", high="#22031F") 
  
  return(p)
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
