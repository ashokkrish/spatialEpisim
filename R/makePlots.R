makePlot <- function (compartments, input, plotTitle, xTitle, yTitle, lineThickness) {
  compColors <- data.frame(row.names = c("S", "V", "E", "I", "R", "D"), val=c("yellow", "blue", "orange", "red", "green", "black"))
  return(renderImage({
    outfile <- tempfile(fileext = '.png')
    
    png(outfile, width = 800, height = 600)
    df <- read_xlsx(paste0("www/MP4/", countrycode(input$selectedCountry, "country.name", "iso3c"), "_summary.xlsx"))
    plotData = data.frame(Date = ymd(df[,"Date"]))
    for (comp in compartments){
      plotData[comp] <- df[,comp]
    }
    p = ggplot(plotData, mapping = aes_string("Date", compartments[1], group = 1)) +
      labs(title = plotTitle, x = xTitle, y = yTitle) +
      scale_x_date(date_labels = "%d %b %Y")
    for (comp in compartments) {
      p <- p + geom_line(aes_string("Date", comp), size=lineThickness, color=compColors[comp,])
    }
    plot(p)
    dev.off()
    
    list(src = outfile, contentType = 'image/png', width = 800, height = 600, alt = "Image not found")
  }, deleteFile = TRUE))
}