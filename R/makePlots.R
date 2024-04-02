makePlot <- function (compartments, selectedCountry, plotTitle, xTitle, yTitle, lineThickness) {
  
  compColors <- data.frame(row.names = c("S", "V", "E", "I", "R", "D"), val=c("yellow", "blue", "orange", "red", "green", "black"))
  
    outfile <- tempfile(fileext = '.png')
    png(outfile, width = 800, height = 600)
    df <- as.data.frame(read_xlsx(paste0("www/MP4/", countrycode(selectedCountry, "country.name", "iso3c"), "_summary.xlsx")))
    
    plotData = data.frame(Date = ymd(df[,"Date"]))
    
    for (comp in compartments){
      plotData[comp] <- df[,comp]
    }
    
    plotData <- as.data.frame(pivot_longer(plotData, all_of(compartments), names_to = "Compartment", values_to = "Count"))    
    plotData$Compartment <- factor(plotData$Compartment, levels = compartments)
    
    # , mapping = aes_string("Date", compartments[1], group = 1)
    
    p = ggplot(plotData,
               aes(x = Date, y = Count, color = Compartment)) +
      geom_line(linewidth = lineThickness) +
      labs(title = plotTitle, 
           x = xTitle, 
           y = yTitle,
           color = "") +
      scale_x_date(date_labels = "%d %b %Y") +
      theme(
        plot.title = element_text(size = 18, 
                                  face = "bold",
                                  margin = margin(0, 0, 25, 0),
                                  hjust = 0.5),
        axis.title.x = element_text(size = 14, 
                                    face = "bold",
                                    margin = margin(25, 0, 0, 0)),
        axis.title.y = element_text(size = 14, 
                                    face = "bold",
                                    margin = margin(0, 25, 0, 0)),
        axis.text.x.bottom = element_text(size = 14),
        axis.text.y.left = element_text(size = 14),
        axis.line = element_line(linewidth = 0.5),
        plot.margin = unit(c(1, 1, 1, 0),"cm"),
        legend.title = element_text(size = 10,
                                    face = "bold"),
        legend.box = "horizontal"
      ) +
      scale_color_manual(
        values = c(
          "S" = "yellow",
          "V" = "blue",
          "E" = "orange",
          "I" = "red",
          "R" = "green",
          "D" = "black"
        )
      ) +
      coord_cartesian(clip="off")
      
    if(length(compartments) == 1) {
      p <- p + theme( legend.position = 'none')
    }
    
    plot(p)
    dev.off()

    return(p)
}