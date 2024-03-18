makePlot <- function (compartments, selectedCountry, plotTitle, xTitle, yTitle, lineThickness) {
  
  compColors <- data.frame(row.names = c("S", "V", "E", "I", "R", "D"), val=c("yellow", "blue", "orange", "red", "green", "black"))
  
  return(renderImage({
    outfile <- tempfile(fileext = '.png')
    png(outfile, width = 800, height = 600)
    df <- as.data.frame(read_xlsx(paste0("www/MP4/", countrycode(selectedCountry, "country.name", "iso3c"), "_summary.xlsx")))
    
    plotData = data.frame(Date = ymd(df[,"Date"]))
    
    for (comp in compartments){
      plotData[comp] <- df[,comp]
    }
    
    # , mapping = aes_string("Date", compartments[1], group = 1)
    
    p = ggplot(plotData) +
      labs(title = plotTitle, x = xTitle, y = yTitle) +
      scale_x_date(date_labels = "%d %b %Y") 
      # ylim(0, NA)
    
    for (comp in compartments) {
      p <- p + geom_line(aes(.data[["Date"]], .data[[comp]]), linewidth=lineThickness, color=compColors[comp,])
    }
    
    plot(p)
    dev.off()
    
    list(src = outfile, contentType = 'image/png', width = 800, height = 600, alt = "Image not found")
  }, deleteFile = TRUE))
}