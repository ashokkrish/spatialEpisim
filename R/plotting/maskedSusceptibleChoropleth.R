##' @description Plot a map of the susceptible persons in the level one
##'   administrative regions of a masked susceptible RasterLayer
##' @title Plot a map of Susceptible persons
##' @param maskedSpatRaster a masked susceptibile layer created with
##'   maskSusceptibleSpatRaster
##' @param subregionRaster TODO
##' @param countryISO3C TODO
##' @author Bryce Carson
##' @author Michael Myer
##' @author Ashok Krishnmaurthy
##' @examples
##' ## Get the subregion raster, ensure its CRS is the same as that for
##' ## Susceptible, and then mask it.
##' countryISO3C <- "COD"
##' suceptible <- createSusceptibleLayer(countryISO3C)
##' subregionRaster <- getSubregionRaster(countryISO3C)
##' crs(subregionRaster) <- crs(susceptible, proj = TRUE)
##' plotMaskedRaster(maskSusceptibleSpatRaster(subregionRaster, susceptible),
##'                  subregionRaster,
##'                  countryISO3C)
plotMaskedRaster <- function(maskedSpatRaster, subregionRaster, countryISO3C) {
  ## The Haxby colour palette
  palette <- c('#FFFFFF', '#D0D8FB', '#BAC5F7',
               '#8FA1F1', '#617AEC', '#0027E0',
               '#1965F0', '#0C81F8', '#18AFFF',
               '#31BEFF', '#43CAFF', '#60E1F0',
               '#69EBE1', '#7BEBC8', '#8AECAE',
               '#ACF5A8', '#CDFFA2', '#DFF58D',
               '#F0EC78', '#F7D767', '#FFBD56',
               '#FFA044', '#EE4F4D') %>%
    colorRampPalette()

  countryNameEnglish <- countrycode(countryISO3C, "iso3c", "country.name.en")

  ## Plot the classified raster, add a compass (north) mark, and add the GADM
  ## data to the plot.
  terra::plot(maskedSpatRaster,
              col = palette(8)[-1],
              axes = TRUE,
              buffer = TRUE,
              box = TRUE,
              cex.main = 1.5,
              line.main = 1.25,
              main = sprintf(r"(2020 UN-Adjusted Population Count
for cropped selection(s) in %s (1 sq. km resolution))", countryNameEnglish),
              xlab = expression(bold(Longitude)),
              ylab = expression(bold(Latitude)),
              line.lab = 2.25,
              cex.lab = 1.4,
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
  terra::plot(subregionRaster, add = TRUE)
  terra::north(type = 2, xy = "bottomleft", cex = 1)
}
