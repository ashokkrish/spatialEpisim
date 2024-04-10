library(ggplot2)
library(readr)
library(readxl)

PrintCumulativePlot <- function(filename, column, plotTitle, xTitle) {
    file <- as.data.frame(openFile(filename))
    plotData <- data.frame(x = ymd(file[,"Date"]),
                             y = file[,column])

    p = ggplot(plotData,
               aes(x = x, y = y)) +
      geom_line(color = "red", linewidth = 2) +
      geom_point(color = "red") +
      # geom_area(fill = "#22031F", alpha = 0.3) +
      labs(title = plotTitle, 
           x = xTitle, 
           y = "Cumulative Cases") +
      scale_x_date(date_labels = "%d %b %Y") +
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
        plot.margin = unit(c(3, 2, 2, 2),"cm")) 
    
    return(p)
}

openFile <- function(filename) {
  ext <- tools::file_ext(filename)
  ext <- tolower(ext)
  
  switch(ext, 
         csv =  read_csv(filename, show_col_types = FALSE),
         xls =  read_xls(filename),
         xlsx = read_xlsx(filename),
         txt =  read_tsv(filename, show_col_types = FALSE),
  )
}
