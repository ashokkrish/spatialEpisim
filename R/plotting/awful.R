## NOTE: these two functions were used to plot values from the SVERID simulation.

awfulPlottingCode <- function() {
  ## TODO: refactor everything below this line.
  unlink("www/MP4", recursive = TRUE, force = TRUE) # Delete the MP4
  dir.create("www/MP4")               # Create empty MP4 folder before running new simulation
  dir.create("www/MP4/paper")         # Create paper folder before for plots without labels

  save(stack$raster.list[["Infected"]], file = "infectedRaster.RData")
  ## plot(layers.timeseries[[t]]$raster.list[["Infected"]],
  ##      col = pal(8)[-2],
  ##      axes = T,
  ##      cex.main = 1,
  ##      main = "Location of Initial Infections",
  ##      plg = list(title = expression(bold("Persons")),
  ##                 title.cex = 1,
  ##                 horiz = TRUE,
  ##                 x.intersp = 0.6,
  ##                 inset = c(0, -0.2),
  ##                 cex = 1.15),
  ##      pax = list(cex.axis = 1.15),
  ##      legend = TRUE,
  ##      mar = c(8.5, 3.5, 2.5, 2.5),
  ##      add = FALSE)
  ## plot(Level1Identifier, add = TRUE)

  ## Print a PNG for the infected variable
  rasterLayer <- "Infected"
  ## print(layers.timeseries[[1]]$raster.list[[rasterLayer]])
  maxRasterLayerVal <- 0

  for (t in 1:n.days) {
    tempMax <- minmax(layers.timeseries[[t]]$raster.list[[rasterLayer]])
    maxRasterLayerVal <- max(maxRasterLayerVal, tempMax)
  }

  ramp <- c('#FFFFFF', '#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC',
            '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF',
            '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE',
            '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767',
            '#FFBD56', '#FFA044', '#EE4F4D')
  pal <- colorRampPalette(ramp)

  for (t in 1:n.days) {
    ## OLD
    ## fname = paste0("MP4/", countryISO3C, "_", rasterLayer, "_", sprintf("%04d", t), ".png")

    ## From /home/bryce/Documents/src/r/spatialEpisim/R/plotting/rasterPlot.R
    printStackLayer(raster.list = layers.timeseries[[t]]$raster.list,
                    rasterLayer = rasterLayer,
                    directOutput = directOutput,
                    Level1Identifier = stack$Level1Identifier,
                    countryISO3C,
                    rasterAgg = rasterAgg,
                    fname = here("www", "MP4", "paper", sprintf("%s_%s_%04d.png",
                                                                countryISO3C,
                                                                rasterLayer,
                                                                t)),
                    maxVal = maxRasterLayerVal,
                    includeLabels = F,
                    isCropped)
  }

  ## MERGE THE PNGs TO A GET AN MP4 VIDEO
  setwd("www/MP4")
  videoDuration <- 15 # in seconds
  av::av_encode_video(list.files(pattern = ".png"),
                      framerate = n.days/videoDuration,
                      output = paste0(rasterLayer, "_MP4.mp4"))
  setwd("./../..")
}

awfulPlottingCode_two <- function() {
    ## ramp <- c('#FFFFFF', '#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC',
    ##           '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF',
    ##           '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE',
    ##           '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767',
    ##           '#FFBD56', '#FFA044', '#EE4F4D')
    ## pal <- colorRampPalette(ramp)
    ## par(mfrow = c(1, 2))
    ## plot(Infected, col = pal(8)[-2], axes = T, cex.main = 1,
    ##      main = "Location of Initial Infections",
    ##      xlab = expression(bold("Longitude")), ylab = expression(bold("Latitude")),
    ##      legend = TRUE, horizontal = TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
    ##
    ## plot(Level1Identifier, add = TRUE)
    ##
    ## plot(Dead, col = pal(8)[-2], axes = T, cex.main = 1,
    ##      main = "Location of Initial Deaths",
    ##      xlab = expression(bold("Longitude")), ylab = expression(bold("Latitude")),
    ##      legend = TRUE, horizontal = TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
    ##
    ## plot(Level1Identifier, add = TRUE)
    ##
    ## plot(log10(Susceptible), col = pal(8)[-2], axes = T, cex.main = 1, main = "Susceptible", legend=TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
    ## plot(Level1Identifier, add = TRUE)
    ##
    ## plot(Inhabited, col = pal(8)[-2], axes = T, cex.main = 1, main = "Inhabited Cells", legend=TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
    ## plot(Level1Identifier, add = TRUE)
    ## writeRaster(Infected, "seed.tif", overwrite = TRUE)
}
