##' @title Population or susceptible persons choropleth mapping
##' @param selectedCountry A country, as listed in [countrycode::codelist$country.name.en]
##' @param susceptibleSpatRaster A SpatRaster containing the population or susceptible persons data
##' @param administrativeBordersSpatVector A SpatVector providing the administrative boundaries of provinces/states
##' @returns a plot object
##' @author Bryce Carson
##' @author Ashok Krishnamurthy
populationChoropleth <- function(selectedCountry, susceptibleSpatRaster, administrativeBordersSpatVector) {
  ramp <- c('#FFFFFF',
            '#D0D8FB',
            '#BAC5F7',
            '#8FA1F1',
            '#617AEC',
            '#0027E0',
            '#1965F0',
            '#0C81F8',
            '#18AFFF',
            '#31BEFF',
            '#43CAFF',
            '#60E1F0',
            '#69EBE1',
            '#7BEBC8',
            '#8AECAE',
            '#ACF5A8',
            '#CDFFA2',
            '#DFF58D',
            '#F0EC78',
            '#F7D767',
            '#FFBD56',
            '#FFA044',
            '#EE4F4D')

  theCountries <- c("Czech Republic",
                    "Democratic Republic of Congo",
                    "Gambia",
                    "Netherlands")
  plotTitling <- paste("2020 UN-Adjusted Population Count \n for",
                       if (selectedCountry %in% theCountries) "the",
                       selectedCountry,
                       "(1 sq. km resolution)")

  x <- classify(susceptible, c(0, 10, 25, 50, 100, 250, 1000, 100000))
  levels(x) <- levels(x)[[1]]
  terra::plot(x,
              col = colorRampPalette(ramp)(8)[-1], # no white
              axes = TRUE,
              buffer = TRUE,
              box = TRUE,
              cex.main = 1.5,
              line.main = 1.25,
              main = plotTitling,
              xlab = expression(bold(Longitude)),
              ylab = expression(bold(Latitude)),
              line.lab = 2.25,
              cex.lab = 1.5,
              plg = list(title = expression(bold("Persons")),
                         title.cex = 1.25,
                         horiz = TRUE,
                         loc = "bottom",
                         yjust = 3.5,
                         x.intersp = 0.6,
                         inset = c(0, -0.2),
                         cex = 1.25),
              pax = list(cex.axis = 1.7),
              mar = c(8.5, 3.5, 4, 2.5))
  terra::north(type = 2, xy = "bottomleft", cex = 1)

  plot(administrativeBordersSpatVector, add = TRUE)
}
