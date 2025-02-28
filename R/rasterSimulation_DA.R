options(conflicts.policy = list(warn = FALSE))
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(av))
shhh(library(countrycode))
shhh(library(cptcity))
shhh(library(lattice))
shhh(library(lubridate))
shhh(library(magick))
shhh(library(Matrix))
shhh(library(rasterVis))
shhh(library(rstudioapi))
shhh(library(readxl))
shhh(library(sp))
shhh(library(sf))     # classes and functions for vector data
shhh(library(terra, warn.conflicts=FALSE))
shhh(library(writexl))

# source("R/rasterStack.R")  # This code generates the base RasterStack/RasterBrick
source("R/rasterPlot.R")   # This code generates the .png and .mp4 files for RasterStack
source("R/distwtRaster.R") # This code sets the Euclidean distance and the weight matrix

#-------------------------------------------------------------------------------------#
# Compartmental model simulation with an option to include Bayesian Data Assimilation #
#-------------------------------------------------------------------------------------#

SpatialCompartmentalModelWithDA <- function(model, stack, startDate, selectedCountry, directOutput,
                                            rasterAgg, alpha, beta, gamma, sigma, delta, radius, lambda,
                                            timestep, seedFile, seedRadius, deterministic, isCropped,
                                            level1Names, DA = F, sitRepData, dataI, dataD, varCovarFunc,
                                            QVar, QCorrLength, nbhd, psiDiag)
{
  unlink("www/MP4", recursive = TRUE, force = TRUE) # Delete the MP4
  dir.create("www/MP4")               # Create empty MP4 folder before running new simulation
  dir.create("www/MP4/paper")         # Create paper folder before for plots without labels

  inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c') #Converts country name to ISO Alpha

  Susceptible <- stack$rasterStack$Susceptible
  Vaccinated <- stack$rasterStack$Vaccinated
  Exposed <- stack$rasterStack$Exposed
  Infected <- stack$rasterStack$Infected
  Recovered <- stack$rasterStack$Recovered
  Dead <- stack$rasterStack$Dead

  Inhabitable <- stack$rasterStack$Inhabitable
  Level1Raster <- stack$rasterStack$Level1Raster

  Level1Identifier <- stack$Level1Identifier

  # print(Level1Identifier$NAME_1)  # List of states/provinces/regions

  # plot(Level1Raster)
  # plot(Level1Identifier, add = TRUE)
  #
  # print(Susceptible);  print(Vaccinated); print(Exposed); print(Infected); print(Recovered); print(Dead)
  #
  # dim(Susceptible); dim(Vaccinated); dim(Exposed); dim(Infected); dim(Recovered); dim(Dead); dim(Inhabitable); dim(Level1Raster)
  #
  # print(table(values(Inhabitable)))

  names <- c("Date", "N", "S", "V", "E", "I", "R", "D",
             "newV", "newE", "newI", "newR","newD", "cumE", "cumI", "Alpha", "Beta", "Gamma", "Sigma", "Delta",
             "Radius", "Lambda", "Model", "DA")

  summary <- data.frame(matrix(data = 0, ncol = length(names), nrow = timestep))

  colnames(summary) <- names

  nrows <- nrow(stack$rasterStack) #
  ncols <- ncol(stack$rasterStack) #

  p <- nrows * ncols

  #------------------------#
  # Initial seed locations #
  #------------------------#

  if (missing(seedFile)){
    seedFolder <- "seeddata/"         # .csv or .xlsx files may be stored in local seeddata/ folder
    seedData <<- read_excel(paste0(seedFolder, inputISO, "_InitialSeedData.csv"), header = T)
    seedData <<- read_excel(paste0(seedFolder, inputISO, "_InitialSeedData.xlsx"), 1, header = T)
  } else {
    seedData <<- read.csv(seedFile)
  }

  numLocations <- dim(seedData)[1]
  # print(numLocations)

  # Seed the initial infections equitably in a Moore Neighborhood of cells
  #seedRadius <- 0
  numCellsPerRegion <- (2*seedRadius + 1)^2
  for (ff in 1:numLocations)
  {
    # print(seedData)
    # print(paste("Seed location = ", seedData[ff,1]))
    row <- terra::rowFromY(stack$rasterStack, seedData[ff,2])
    col <- terra::colFromX(stack$rasterStack, seedData[ff,3])
    # print("row = ", row, "col = ", col)
    # print(Inhabitable[(row-seedRadius):(row+seedRadius),(col-seedRadius):(col+seedRadius)])
    # print(sum(Inhabitable[(row-seedRadius):(row+seedRadius),(col-seedRadius):(col+seedRadius)]))
    newVaccinatedPerCell <- seedData[ff,4]#/numCellsPerRegion    #round(seedData[ff,8]/numCellsPerRegion)
    newExpPerCell        <- seedData[ff,5]/numCellsPerRegion     #round(seedData[ff,5]/numCellsPerRegion)
    newInfPerCell        <- seedData[ff,6]/numCellsPerRegion     #round(seedData[ff,4]/numCellsPerRegion)
    newRecoveredPerCell  <- seedData[ff,7]#/numCellsPerRegion    #round(seedData[ff,6]/numCellsPerRegion)
    newDeadPerCell       <- seedData[ff,8]#/numCellsPerRegion    #round(seedData[ff,7]/numCellsPerRegion)
    # print(newVaccinatedPerCell)
    # print(newExpPerCell)
    # print(newInfPerCell)
    # print(newRecoveredPerCell)
    # print(newDeadPerCell)
    #Vaccinated[(row-seedRadius):(row+seedRadius),(col-seedRadius):(col+seedRadius)] <- Vaccinated[(row-seedRadius):(row+seedRadius),(col-seedRadius):(col+seedRadius)] + newVaccinatedPerCell
    Vaccinated[row,col] <- Vaccinated[row,col] + newVaccinatedPerCell
    Exposed[(row-seedRadius):(row+seedRadius),(col-seedRadius):(col+seedRadius)] <- Exposed[(row-seedRadius):(row+seedRadius),(col-seedRadius):(col+seedRadius)] + newExpPerCell
    Infected[(row-seedRadius):(row+seedRadius),(col-seedRadius):(col+seedRadius)] <- Infected[(row-seedRadius):(row+seedRadius),(col-seedRadius):(col+seedRadius)] + newInfPerCell
    #Recovered[(row-seedRadius):(row+seedRadius),(col-seedRadius):(col+seedRadius)] <- Recovered[(row-seedRadius):(row+seedRadius),(col-seedRadius):(col+seedRadius)] + newRecoveredPerCell
    Recovered[row, col] <- Recovered[row,col] + newRecoveredPerCell
    #Dead[(row-seedRadius):(row+seedRadius),(col-seedRadius):(col+seedRadius)] <- Dead[(row-seedRadius):(row+seedRadius),(col-seedRadius):(col+seedRadius)] + newDeadPerCell
    Dead[row, col] <- Dead[row,col] + newDeadPerCell
    #print(Exposed)
    #print(paste("Susceptible = ", sum(values(Susceptible))))
  }

  ramp <- c('#FFFFFF', '#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
  pal <- colorRampPalette(ramp)

  valSusceptible <- terra::as.matrix(Susceptible, wide = TRUE)
  valVaccinated <- terra::as.matrix(Vaccinated, wide = TRUE)
  valExposed <- terra::as.matrix(Exposed, wide = TRUE)
  valInfected <- terra::as.matrix(Infected, wide = TRUE)
  valRecovered <- terra::as.matrix(Recovered, wide = TRUE)
  valDead <- terra::as.matrix(Dead, wide = TRUE)
  # print("new----------------")
  # print(valVaccinated)
  # print("-------------------")

  #par(mfrow = c(1, 2))

  # plot(Infected, col = pal(8)[-2], axes = T, cex.main = 1,
  #      main = "Location of Initial Infections",
  #      xlab = expression(bold("Longitude")), ylab = expression(bold("Latitude")),
  #      legend = TRUE, horizontal = TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
  #
  # plot(Level1Identifier, add = TRUE)
  #
  # plot(Dead, col = pal(8)[-2], axes = T, cex.main = 1,
  #      main = "Location of Initial Deaths",
  #      xlab = expression(bold("Longitude")), ylab = expression(bold("Latitude")),
  #      legend = TRUE, horizontal = TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
  #
  # plot(Level1Identifier, add = TRUE)
  #
  # plot(log10(Susceptible), col = pal(8)[-2], axes = T, cex.main = 1, main = "Susceptible", legend=TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
  # plot(Level1Identifier, add = TRUE)
  #
  # plot(Inhabitable, col = pal(8)[-2], axes = T, cex.main = 1, main = "Inhabitable Cells", legend=TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
  # plot(Level1Identifier, add = TRUE)

  # writeRaster(Infected, "seed.tif", overwrite = TRUE)

  sumS <- sum(values(Susceptible))
  sumV <- sum(values(Vaccinated))
  sumE <- sum(values(Exposed))
  sumI <- sum(values(Infected))
  sumR <- sum(values(Recovered))
  sumD <- sum(values(Dead))

  # print(sumS); print(sumV); print(sumE); print(sumI); print(sumR); print(sumD)

  propVaccinated <- sumV/sumS
  propExposed <- sumE/sumS
  propInfected <- sumI/sumS
  propRecovered <- sumR/sumS
  propDead <- sumD/sumS

  # print(propVaccinated)
  # print(propExposed)
  # print(propInfected)
  # print(propRecovered)
  # print(propDead)

  print(paste("Susceptible Count before removing initial seed values: ", sum(values(Susceptible))))
  # print(values(Susceptible))

  Susceptible <- Susceptible - (Susceptible*propVaccinated) - (Susceptible*propExposed) - (Susceptible*propInfected) - (Susceptible*propRecovered) - (Susceptible*propDead)

  print(paste("Susceptible Count after removing initial seed values: ", sum(values(Susceptible))))

  datarow <- 1 # pre-allocating the row from which we read the data to assimilate each week
  cumVaccinated <- round(sumV)
  cumExposed <- round(sumE)
  cumInfected <- round(sumI)
  cumRecovered <- round(sumR)
  cumDead <- round(sumD)

  cumIncidence <- round(sumI)

  ################# DA Begins ##################

  if (DA == T)
  {
    #-------------------------------------------#
    # Import the Ebola Incidence and Death Data #
    #-------------------------------------------#

    incidence_data <- read_excel(dataI)
    #death_data <- read_excel(dataD)

    print(paste("Dimension of Incidence Matrix: ", dim(incidence_data)[1], dim(incidence_data)[2]))

    #print(paste("Dimension of Death Matrix: ", dim(death_data)[1], dim(death_data)[2]))

    ## DONT change this here, change it in spatialEpisim.foundation is needed,
    ## then copy over the function defintion here.
    linearInterpolationOperator <- function(layers,
                                            healthZoneCoordinates,
                                            neighbourhood.order = 2,
                                            compartmentsReported = 1) {
      stopifnot(neighbourhood.order %in% c(0, 1, 2))
      if (neighbourhood.order == 2)
        stopifnot(terra::ncell(layers) >= 5 && terra::nrow(layers) >= 5) # required for 2nd order
      stopifnot(compartmentsReported %in% 1:2)

      queensNeighbours <- function(order, cell, ncols) {
        stopifnot(order %in% 1:2)

        if (order == 1) {
          neighbouringCells <-
            c((cell - ncols - 1) : (cell - ncols + 1),
              cell - 1 , cell + 1,
              (cell + ncols - 1) : (cell + ncols + 1))
          stopifnot(length(neighbouringCells) == 8)
        } else if (order == 2) {
          neighbouringCells <-
            c((cell - ncols * 2 - 2) : (cell - ncols * 2 + 2),
              cell - ncols - 2 , cell - ncols + 2,
              cell - 2 , cell + 2,
              cell + ncols - 2 , cell + ncols + 2,
              (cell + ncols * 2 - 2) : (cell + ncols * 2 + 2))
          stopifnot(length(neighbouringCells) == 16)
        }

        neighbouringCells
      }

      extend.length <- 5
      layers <- terra::extend(layers, extend.length)

      ## NOTE: cells contains the index into the rasters in layers (when converted
      ## to a matrix). Extract coordinates as cbind(lon, lat); it's stored as
      ## cbind(lat, lon).
      cells <- terra::cellFromXY(layers, terra::as.matrix(healthZoneCoordinates[, 3:2]))

      if (any(duplicated(cells))) {
        overaggregatedHealthZones <- tibble::tibble("Health Zone" = healthZoneCoordinates[, 1], Cell = cells) %>%
          dplyr::group_by(Cell) %>%
          dplyr::filter(dplyr::n() != 1)

        warning(sprintf("[Linear interpolation operator] Raster aggregation factor is too high to differentiate between two (or more) health zones (they correspond to the same grid cell).\n%s",
                        ## Based on the helpful answer by Richie Cotton on SO:
                        ## https://stackoverflow.com/a/26083626/14211497, which the following is
                        ## based on.
                        paste(capture.output(print(overaggregatedHealthZones)), collapse = "\n")))
      }
      if (any(is.na(cells)))
        warning("Ignoring NAs in [cells] object corresponding to coordinates out of bounds of [layers] SpatRaster.")

      cells <- cells[!is.na(cells)]

      ## NOTE: preallocate the linear forward interpolation matrix, with
      ## dimensions q * p (health zones by cells in the SpatRaster).
      H <- base::matrix(0, nrow(healthZoneCoordinates), terra::ncell(layers))

      ## NOTE: these are the weightings used for the chess queen zeroth, first,
      ## and second order neighbours. The zeroth order neighbor is the position of
      ## the queen itself.
      neighbour.weights <-
        switch(neighbourhood.order + 1, # the first of ... applies to zero, etc.
               1,
               c(2, 1) * 0.1,
               c(3, 2, 1) * 35^-1)

      for (index in seq_along(cells)) {
        H[index, cells[index]] <- neighbour.weights[1]

        if (neighbourhood.order != 0) {
          neighbour.1st <- queensNeighbours(1, cells[index], terra::ncol(layers))
          H[index, neighbour.1st] <- neighbour.weights[2]
        }

        if (neighbourhood.order == 2) {
          neighbour.2nd <- queensNeighbours(2, cells[index], terra::ncol(layers))
          if(anyDuplicated(c(neighbour.1st, neighbour.2nd)) > 0)
            simpleError("Duplicate cell indices among neighbours of multiple localities.")
          H[index, neighbour.2nd] <- neighbour.weights[3]
        }
      }

      ## TODO: move the following NOTE to a better place than here; perhaps to the
      ## function documentation details. This should be tested using automatic
      ## testing with various input values. NOTE: this corresponds to the
      ## hand-written note I made after discussionwith Ashok. He told me that the
      ## sum of all cells needs to be equivalent to one; the sum of all cells is
      ## per-health zone, ergo the first condition checks that the sum of the entire
      ## matrix (with nrow := health zones) is the same as the number of health
      ## zones (because each should sum to one). NOTE: each row corresponds to one
      ## of the neighourhoods pictures in the plots "ashok.png" or "me.png".
      stopifnot(dplyr::near(sum(H), nrow(healthZoneCoordinates)))
      stopifnot(dplyr::near(sum(matrix(H[1, ],
                                       ncol = ncol(layers),
                                       byrow = TRUE)),
                            1))

      ## NOTE: the extended areas of the matrix are now dropped to return the matrix
      ## to the expected size for the input.
      H <-
        apply(X = H,
              MARGIN = 1, # apply the function to rows
              FUN =
                function(row) {
                  m <- matrix(row, byrow = TRUE, ncol = ncol(layers))
                  m[(extend.length + 1):(base::nrow(m) - extend.length),
                  (extend.length + 1):(base::ncol(m) - extend.length)] %>%
                    Matrix::t() %>% # row-major order (byrow)
                    as.vector()
                }) %>% Matrix::t() # rows should be health zones

      if (compartmentsReported == 2) H <- as.matrix(Matrix::bdiag(H, H))

      stopifnot(sum(.rowSums(H, m = nrow(H), n = ncol(H))) == (compartmentsReported * nrow(healthZoneCoordinates)))

      return(H)
    }

    states_observable <- 1
    healthZoneCoordinates <- openDataFile(sitRepData)
    stopifnot(hasName(healthZoneCoordinates, "HealthZone"))
    stopifnot(hasName(healthZoneCoordinates, "Latitude"))
    stopifnot(hasName(healthZoneCoordinates, "Longitude"))
    Hmat <- linearInterpolationOperator(terra::rast(stack$rasterStack), healthZoneCoordinates)
    nHealthZones <- nrow(healthZoneCoordinates)

    #------------------#
    # Read in Q matrix #
    #------------------#
    source("R/Q_matrix.R")

    QMat <- genQ(nrows, ncols, varCovarFunc, QVar, QCorrLength, nbhd, states_observable =  states_observable) #states_observable = 2

    Q <- QMat$Q
    # plot(Q[1:101,1])
    # plot(Q[1,1:101])
    # print(diag(Q)[1:200])
    # print(det(Q))

    # print(paste("Dimension of the Model Error Covariance Matrix: ", dim(Q)[1], dim(Q)[2]))

    QFull <- QMat$QFull
    # print(det(QFull))
    # print(paste("Dimension of the Block Diagonal Model Error Covariance Matrix: ", dim(QFull)[1], dim(QFull)[2]))

    QHt <- QFull%*%t(Hmat)

    HQHt <- Hmat%*%QHt

    #print(HQHt)
    #print(paste("Dimension of HQHt Matrix: ", dim(HQHt)[1], dim(HQHt)[2]))

    #print(HQHt[1:8, 1:8])

    ## HQHt is a square-symmetric matrix
      print(rowSums(HQHt))
      print(colSums(HQHt))
    # table(rowSums(HQHt))
    # table(colSums(HQHt))
      print(diag(HQHt))

      print(paste("det(HQHt) is:", det(HQHt)))

    # eigen(HQHt)$values
      print(sum(eigen(HQHt)$values)) # Sum of eigenvalues is equal to q

    #----------------------#
    # Plot the HQHt matrix #
    #----------------------#

    # source("R/plotHQHt.R")
    # plotHQHt(HQHt)
  }
  ################# DA Ends ##################

  #-------------------------------#
  # MAIN LOOP FOR TIME INCREMENTS #
  #-------------------------------#

  allRasters <- vector(mode = "list", length = timestep)

  for (t in 1:timestep)
  {					# time increments
    print(paste("time = ", t))

    summary[t, 1]  <- toString(as.Date(startDate) + days(t - 1)) # Print the date at each time step
    summary[t, 2]  <- round(sumS + sumV + sumE + sumI + sumR + sumD)
    summary[t, 3]  <- round(sumS)
    summary[t, 4]  <- round(sumV)            # Absorbing state
    summary[t, 5]  <- round(sumE)            # This is the prevalence (active exposed cases) at time t, NOT the cumulative sum
    summary[t, 6]  <- round(sumI)            # This is the prevalence (active infectious cases) at time t, NOT the cumulative sum
    summary[t, 7]  <- round(cumRecovered)    # round(sumR)   # Absorbing state
    summary[t, 8]  <- round(cumDead)         # round(sumD)   # Absorbing state

    summary[t, 14]  <- round(cumExposed)
    summary[t, 15]  <- round(cumInfected)
    summary[t, 16]  <- alpha
    summary[t, 17]  <- beta
    summary[t, 18]  <- gamma
    summary[t, 19]  <- sigma
    summary[t, 20]  <- delta
    summary[t, 21]  <- radius
    summary[t, 22]  <- lambda
    summary[t, 23]  <- model

    valInhabitable <- terra::as.matrix(Inhabitable, wide = TRUE)
    valSusceptible <- terra::as.matrix(Susceptible, wide = TRUE)
    valVaccinated <- terra::as.matrix(Vaccinated, wide = TRUE)
    valExposed <- terra::as.matrix(Exposed, wide = TRUE)
    valInfected <- terra::as.matrix(Infected, wide = TRUE)
    valRecovered <- terra::as.matrix(Recovered, wide = TRUE)
    valDead <- terra::as.matrix(Dead, wide = TRUE)

    nextSusceptible <- nextVaccinated <- nextExposed <- nextInfected <- nextRecovered <- nextDead <- matrix(0, nrows, ncols, byrow = T)
    pSusceptible <- newVaccinated <- nearbyInfected <- newExposed <- newInfected <- newRecovered <- newDead <- matrix(0, nrows, ncols, byrow = T)

    dailyVaccinated <- dailyExposed <- dailyInfected <- dailyRecovered <- dailyDead <- 0

    #-------------------------------#
    # Generating the I_tilda matrix #
    #-------------------------------#

    I_tilda <- wtd_nbrs_sum(input_matrix = valInfected, radius = radius, lambda = lambda)

    nLiving <- valSusceptible + valVaccinated + valExposed + valInfected + valRecovered

    #--------------------------------------------------------------------#
    # Some susceptible people are going to be newly vaccinated           #
    #--------------------------------------------------------------------#
    newVaccinated <- alpha*valSusceptible
    newVaccinated[valSusceptible < 1] <- 0

    dailyVaccinated <- sum(newVaccinated)

    #--------------------------------------------------------------------#
    # Some susceptible people who come in contact with nearby infected   #
    # are going to be newly exposed                                      #
    #--------------------------------------------------------------------#
    pSusceptible <- valSusceptible/nLiving
    pSusceptible[is.nan(pSusceptible)] <- 0

    if(deterministic) {
      newExposed <- beta*pSusceptible*I_tilda
    } else {
      rpois(1, beta*pSusceptible*I_tilda)
    }
    newExposed[valSusceptible < 1] <- 0
    newExposed[I_tilda < 1] <- 0

    dailyExposed <- sum(newExposed)
    cumExposed <- cumExposed + sum(newExposed)

    #----------------------------------------------------------#
    # Some exposed people are going to become newly infectious #
    #----------------------------------------------------------#
    newInfected <- gamma*valExposed
    newInfected[valExposed < 1] <- 0

    dailyInfected <- sum(newInfected)
    cumInfected   <- cumInfected + sum(newInfected)

    #-----------------------------------------------------------#
    # Some infectious people are going to either recover or die #
    #-----------------------------------------------------------#
    newRecovered <- sigma*valInfected
    newRecovered[valInfected < 1] <- 0

    dailyRecovered <- sum(newRecovered)
    cumRecovered <- cumRecovered + sum(newRecovered)

    newDead <- delta*valInfected
    newDead[valInfected < 1] <- 0

    dailyDead <- sum(newDead)
    cumDead <- cumDead + sum(newDead)

    #-----------------------------------#
    # Store the next state of each cell #
    #-----------------------------------#
    nextSusceptible <- valSusceptible - newExposed - newVaccinated
    nextVaccinated <- valVaccinated + newVaccinated
    nextExposed <- valExposed + newExposed - newInfected
    nextInfected <- valInfected + newInfected - newDead - newRecovered
    nextRecovered <- valRecovered + newRecovered
    nextDead <- valDead + newDead


    nextSusceptible[nLiving <= 0] <- 0
    nextVaccinated[nLiving <= 0] <- 0
    nextExposed[nLiving <= 0] <- 0
    nextInfected[nLiving <= 0] <- 0
    nextRecovered[nLiving <= 0] <- 0
    nextDead[nLiving <= 0] <- 0


    Susceptible <- nextSusceptible
    Vaccinated <- nextVaccinated
    Exposed <- nextExposed
    Infected <- nextInfected
    Recovered <- nextRecovered
    Dead <- nextDead

    Susceptible <- rast(Susceptible)
    Vaccinated <- rast(Vaccinated)
    Exposed <- rast(Exposed)
    Infected <- rast(Infected)
    Recovered <- rast(Recovered)
    Dead <- rast(Dead)

    ext(Susceptible) <- ext(Vaccinated) <- ext(Exposed) <- ext(Infected) <- ext(Recovered) <- ext(Dead) <- ext(stack$rasterStack)
    crs(Susceptible) <- crs(Vaccinated) <- crs(Exposed) <- crs(Infected) <- crs(Recovered) <- crs(Dead) <- crs(stack$rasterStack)

    # ramp <- c('#FFFFFF', '#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
    # pal <- colorRampPalette(ramp)
    #
    # plot(Infected, col = pal(8)[-2], axes = T, cex.main = 1,
    #      main = "Location of Infections after iteration t",
    #      xlab = expression(bold("Longitude")), ylab = expression(bold("Latitude")),
    #      legend = TRUE, horizontal = TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
    #
    # plot(Level1Identifier, add = TRUE)
    # print(Infected)
    #
    # writeRaster(Infected, file = "infectedRaster.tif", overwrite = TRUE)

    stack$rasterStack$Susceptible <- Susceptible
    stack$rasterStack$Vaccinated <- Vaccinated
    stack$rasterStack$Exposed <- Exposed
    stack$rasterStack$Infected <- Infected
    stack$rasterStack$Recovered <- Recovered
    stack$rasterStack$Dead <- Dead

    # print('check')
  # print(testData)
    summary[t, 9]   <- dailyVaccinated
    summary[t, 10]  <- dailyExposed
    summary[t, 11]  <- dailyInfected
    summary[t, 12]  <- dailyRecovered
    summary[t, 13]  <- dailyDead

    sumS <- sum(values(Susceptible)); sumV <- sum(values(Vaccinated));
    sumE <- sum(values(Exposed)); sumI <- sum(values(Infected));
    sumR <- sum(values(Recovered)); sumD <- sum(values(Dead))

    #print(sumS)

    ########## DA Begins ##########

    #setwd(outputDir) # Change working directory to output folder

    if (DA == T)
    {                     # DA T/F
      #NewoutputDir <- paste(outputDir, "/DA", sep="") # The directory for output files
      #if (!(file.exists(NewoutputDir))){
        #dir.create("DA") # Folder to store output files

      #setwd(NewoutputDir) # Change working directory to output folder

      if (t %% 7 == 0)
      {                   # elapsed week
        datarow <- datarow + 1

        if (datarow < 76)
        {                 # datarow cap
          #----------------------------------------#
          # Write forecast (prior) state to matrix #
          #----------------------------------------#

          # print(paste("Xf is printed on day", t))

          #---------------------#),
          # OSI: forecast state #
          #---------------------#
          # We track the  "Infectious" and "Dead" epidemic compartments

          print('Running Data Assimilation...')

          preDASusceptible <- Susceptible
          preDAVaccinated <- Vaccinated
          preDAExposed <- Exposed
          preDAInfected <- Infected
          preDARecovered <- Recovered
          preDADead <- Dead

          rat <- sum(terra::as.matrix(Exposed, wide = TRUE))/(sum(terra::as.matrix(Infected, wide = TRUE))+0.000000001)

          Infected <- terra::as.matrix(Infected, wide = TRUE) # default is byrow = F

          # print(dim(Infected))
          # Dead <- as.matrix(Dead, byrow = T) # default is byrow = F

          Xf.OSI <- t(t(as.vector(t(Infected))))

          # Xf.OSI <- t(cbind(t(as.vector(Infected)), t(as.vector(Dead))))

          # print(paste("Dimension of the state vector:")); print(dim(Xf.OSI))

           #print(sum(Xf.OSI))
          # table(Xf.OSI)

          HXf <- Hmat%*%Xf.OSI
          #print(HXf)
          #print(dim(HXf))
          #print(sum(HXf))

          #----------------------------------------------#
          # Importing DRC Ebola Incidence and Death Data #
          #----------------------------------------------#

          incidence <- as.vector(incidence_data[datarow, 1:nHealthZones+2]) # Pick a row every 7 days, select third column through to the last column
          #death <- as.vector(death_data[datarow, 1:nHealthZones+2])         # Pick a row every 7 days, select third column through to the last column

          # if (datarow > 1)
          # {                 # datarow > 1
          #    prevWHOIncidence <- sum(as.vector(incidence_data[1:(datarow-1), 3:nHealthZones+2]))
          #    currWHOIncidence <- sum(as.vector(incidence_data[1:datarow, 3:nHealthZones+2]))
          #
          #    currSIMIncidence <-
          #    prevSIMIncidence <-
          #
          #    slopeWHO <- (currWHOIncidence - prevWHOIncidence)/nDaysPerUnit
          #    slopeSIM <- (currSIMIncidence - prevSIMIncidence)/nDaysPerUnit
          #
          #    phi <- slopeWHO/slopeSIM
          #
          #   #beta = phi*beta
          #
          # }                 # datarow > 1

          #Dvector <- t(cbind(t(incidence), t(death)))
          Dvector <- incidence

          # print(Dvector)

          #-------------------------------------#
          # Measurement error covariance matrix #
          #-------------------------------------#

          # sum(Dvector < 1)
          Dvector_revised <- ifelse(Dvector < 1, psiDiag, Dvector) # If a diagonal entry is zero change it to 0.1/1.
          # sum(Dvector_revised < 1)

          q <- nHealthZones*states_observable

          M <- diag(as.vector(Dvector_revised))

          #M <- M*exp(-t/10)

          # print(M)

          # print(det(HQHt+M))

          # library(MASS)
          # write.matrix(M, file = 'mes_err.csv')

          # print(M) # check if D vector needs to be really revised

          # levelplot(M, col.regions= colorRampPalette(c("white", "red", "blue")))
          # table(M)
          # diag(M)
          # det(M)

          #---------------------#
          # Optimal Kalman Gain #
          #---------------------#

          # sum(QHt < 0)
          # sum(HQHt < 0)

          # levelplot(as.matrix(HQHt), col.regions= colorRampPalette(c("white", "red", "blue")))

          # diag(HQHt)
          # det(HQHt)
          # eigen(HQHt)$values # HQHt isn't positive definite since all of its eigenvalues are not strictly positive.
          # sum(eigen(HQHt)$values)
          #
          # log10(max(eigen(HQHt)$values)/min(eigen(HQHt)$values))
          #
          # det(solve(HQHt))
          # eigen(solve(HQHt))$values # Inverse of HQHt is also not positive definite since all of its eigenvalues are not strictly positive.
          # # sum(eigen(solve(HQHt))$values)

          # The gain matrix, Ke.OSI, determines how the observational data are to be assimilated
          Ke.OSI <- QHt%*%solve(HQHt + M)

          # print(dim(Ke.OSI))

          #write.matrix(Ke.OSI, file = 'Kal_Gain.csv') #solve((HQHt + M), t(QHt))

          #print(paste("Dimension of the Kalman Gain Matrix:")); print(dim(Ke.OSI))

          # Can the Kalman gain matrix have negative values? Yes.
          # Can the innovation or measurement residual have negative values? Yes.

          #------------------------------------#
          # Innovation or measurement residual #
          #------------------------------------#
          # HXf <- t(t(as.numeric(Dvector)))
          # print(dim(QFull))
          # print(dim(Hmat))
          # print(dim(t(Hmat)))
          cbind(t(t(as.numeric(Dvector))), HXf, t(t(as.numeric(Dvector))) - HXf)

          Y <- t(t(as.numeric(Dvector))) - HXf

          print(sum(Y))
          print(sum(Ke.OSI%*%Y))

          #---------------------------------#
          # OSI update step: analysis state #
          #---------------------------------#

          which(Xf.OSI > 0)

          #sum(Ke.OSI%*%Y < 0)
          length(which(Ke.OSI%*%Y < 0))
          length(which(Ke.OSI%*%Y > 0))
          length(which(Ke.OSI%*%Y == 0))

          Xa.OSI <- Xf.OSI + Ke.OSI%*%Y

          #print(length(Xa.OSI[Xa.OSI < 0]))

          #print(summary(Xa.OSI[Xa.OSI < 0]))

          # print(which.max(Xa.OSI))

          Xa.OSI[Xa.OSI < 0] <- 0 # This will set all negative values to zero. TBW convinced me.

          #print(length(Xa.OSI[Xa.OSI < 0]))

          # Xa.OSI <- abs(Xf.OSI + Ke.OSI%*%Y)

          # max(Ke.OSI%*%Y)
          # min(Ke.OSI%*%Y)
          #
          # sum(round(Ke.OSI%*%Y))
          #
          # print(sum(Xa.OSI))
          # print(tail(sort(Xa.OSI), 50))
          # print(sum(Xf.OSI))

          ###########################

          # sum(Xf.OSI < 0)         # Number of negative values in Xf.OSI.
          #
          # sum(QHt < 0)        # Number of negative values in QHt.
          #
          # sum(HQHt < 0)           # Number of negative values in HQHt.
          #
          # sum(Y < 0)              # Number of negative values in Y.
          #
          # sum(Ke.OSI < 0)         # Number of negative values in Ke.OSI.
          #
          # sum(Ke.OSI%*%Y < 0)     # Number of negative values in Ke.OSI*Y.
          #
          # sum(Xa.OSI < 0)         # Number of negative values in Xa.OSI.

          ###########################

          # HXf <- Hmat%*%Xf.OSI
          # print(dim(HXf))
          # print(sum(HXf))
          #
          # HXa <- Hmat%*%Xa.OSI
          # print(dim(HXa))
          # print(sum(HXa))

          # cbind(HXf, round(HXa), HXa)

          # print(cbind(Dvector, Hmat%*%Xf.OSI, Y, round(Hmat%*%Xa.OSI)))

          # NOTE: when restacking make sure byrow = T.

          I <- matrix(Xa.OSI[1:p], nrow = nrows, ncol = ncols, byrow = T) # AK

          # print('max index is')
          #
          # print(which(I == max(I), arr.ind=TRUE))

          # write.matrix(I, file = 'infected.csv')

          # I[I < 1] <- 0 # Prevent tiny values for the number of infectious

          # D <- matrix(Xa.OSI[(p+1):(2*p)], nrow = nrows, ncol = ncols, byrow = T)
          # print(sum(D))

          #D[D < 1] <- 0 # Prevent tiny values for the number of dead

          dim(Xa.OSI); dim(I);
          # dim(D);
          min(I);
          #min(D);
          max(I);
          #max(D)

          # For all uninhabitable cells set the number of infected and dead = 0. THIS IS VERY CRITICAL!!!

          # for(i in 1:nrows)
          # {                 # nrows
          #   for(j in 1:ncols)
          #   {               # ncols
          #     if (stack$rasterStack$Inhabitable[i,j] == 0)
          #     {             # Inhabitable
          #       #I[i,j] <- D[i,j] <- 0
          #       I[i,j] <- 0
          #     }
          #   }
          # }
          # I[stack$rasterStack$Inhabitable[stack$rasterStack$Inhabitable == 0]] <- 0
          I[valInhabitable == 0] <- 0

          values(stack$rasterStack$Infected) <- I
          # values(stack$rasterStack$Dead) <- D
          Infected <- stack$rasterStack$Infected
          # Dead <- stack$rasterStack$Dead
          cumInfected <- cumInfected + sum(I - terra::as.matrix(preDAInfected, wide = TRUE))

          # cumDead <- cumDead + sum(D - as.matrix(preDADead))
          # print(sum(D - as.matrix(preDADead)))

          # deadDiff <- preDADead - Dead
          # Recovered <- preDARecovered + deadDiff
            Exposed <- rat*Infected
          # exposedDiff <- preDAExposed - Exposed
          # Susceptible <- preDASusceptible + exposedDiff

          # stack$rasterStack$Recovered <- Recovered
            stack$rasterStack$Exposed <- Exposed
          # stack$rasterStack$Susceptible <- Susceptible

          # print('max index is'); print(which.max(Infected))
         } # datarow cap
        } # If t is divisible by 7
      # elapsed week
    }
    allRasters[[t]] <- stack
  }   # time increments
# DA T/F
   ########## DA Ends ##########

  # save(stack$rasterStack[["Infected"]], file = "infectedRaster.RData")

  # plot(allRasters[[t]]$rasterStack[["Infected"]], col = pal(8)[-2], axes = T, cex.main = 1, main = "Location of Initial Infections", plg = list(title = expression(bold("Persons")), title.cex = 1, horiz=TRUE, x.intersp=0.6, inset=c(0, -0.2), cex=1.15), pax = list(cex.axis=1.15), legend=TRUE, mar=c(8.5, 3.5, 2.5, 2.5), add = F)
  #
  # plot(Level1Identifier, add = TRUE)

  # Print a PNG for the infected variable
  rasterLayer <- "Infected"
  #print(allRasters[[1]]$rasterStack[[rasterLayer]])
  maxRasterLayerVal <- 0

  for (t in 1:timestep){
    tempMax <- minmax(allRasters[[t]]$rasterStack[[rasterLayer]])
    maxRasterLayerVal <- max(maxRasterLayerVal, tempMax)
  }

  ramp <- c('#FFFFFF', '#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
  pal <- colorRampPalette(ramp)

  for (t in 1:timestep){
    fname = paste0("MP4/", inputISO, "_", rasterLayer, "_", sprintf("%04d", t), ".png")
    printStackLayer(rasterStack = allRasters[[t]]$rasterStack,
                    rasterLayer = rasterLayer,
                    directOutput = directOutput,
                    Level1Identifier = stack$Level1Identifier,
                    selectedCountry = selectedCountry,
                    rasterAgg = rasterAgg,
                    fname = fname,
                    maxVal = maxRasterLayerVal,
                    includeLabels = T,
                    isCropped)

    # fname = paste0("MP4/", "paper/", inputISO, "_", rasterLayer, "_", sprintf("%04d", t), "_paper", ".png")
    # printStackLayer(rasterStack = allRasters[[t]]$rasterStack, rasterLayer = rasterLayer, directOutput = directOutput, Level1Identifier = stack$Level1Identifier, selectedCountry, rasterAgg = rasterAgg, fname = fname, maxVal = maxRasterLayerVal, includeLabels = F)
  }

  # MERGE THE PNGs TO A GET AN MP4 VIDEO
  setwd("www/MP4")
  videoDuration <- 15 # in seconds
  av::av_encode_video(list.files(pattern = ".png"),
                      framerate = timestep/videoDuration,
                      output = paste0(rasterLayer, "_MP4.mp4"))
  setwd("./../..")

  summary[is.na(summary)] <- 0

  write_xlsx(summary, path = paste0("www/MP4/", inputISO, "_summary.xlsx"), col_names = T)

  #print(tail(summary))

  return(summary)
} # End of function

#--------------#
# Example Call #
#--------------#

# Arguments

# model, startDate, selectedCountry, directOutput, rasterAgg,
# alpha, beta, gamma, sigma, delta, radius, lambda, timestep, seedFile,
# deterministic, isCropped, level1Names, DA = F,
# sitRepData, dataI, dataD, varCovarFunc, QVar, QCorrLength

# model <- "SVEIRD" # "SEIRD"
# startDate <- "2018-08-05" # today()
# selectedCountry <- "Democratic Republic of Congo"
# directOutput <- F
# rasterAgg <- 10

#t <- 1

#------------#
# Parameters #
#------------#

# alpha <- 0.000035  # Daily fraction that move out of the susceptible compartment to the vaccinated compartment
# beta  <- 0.007 # 0.006     # Daily fraction that move out of the susceptible compartment to the exposed compartment
# gamma <- 1/7  # 0.1428571      # Daily fraction that move out of the exposed compartment to the infectious compartment **** Gamma has to remain the same for all scenarios
# sigma <- 1/36 # 0.02777778     # Daily fraction that move out of the infectious compartment to the recovered compartment
# delta <- 2/36 # 0.05555556     # Daily fraction that move out of the infectious compartment to the dead compartment

# radius <- 1 # apply formula as discussed
# lambda <- 15
# timestep <- 564
#
# seedFile <- "seeddata/COD_InitialSeedData.csv"
# seedRadius <- 1
#
# deterministic <- T
# isCropped <- T
# level1Names <- c("Ituri", "Nord-Kivu")
#
# DA <- T # F #
#
# sitRepData <- "observeddata/Ebola_Health_Zones_LatLon.csv"
# dataI <- "observeddata/Ebola_Incidence_Data.xlsx"
# dataD <- "observeddata/Ebola_Death_Data.xlsx"
#
# varCovarFunc <- "DBD" # "Balgovind"
# QVar <- 0.55
# QCorrLength <- 0.675
# nbhd <- 3
# psiDiag <- 0.001

#------------#
# DA is TRUE #
#------------#

#SpatialCompartmentalModelWithDA(model, startDate, selectedCountry, directOutput, rasterAgg, alpha, beta, gamma, sigma = 1/21, delta = 2/21, radius, lambda, timestep, seedFile = "seeddata/COD_InitialSeedData.csv", seedRadius, deterministic, isCropped, level1Names, DA = F, "observeddata/Ebola_Health_Zones_LatLon.csv", "observeddata/Ebola_Incidence_Data.xlsx", "observeddata/Ebola_Death_Data.xlsx", varCovarFunc = "DBD", QVar, QCorrLength, nbhd, psiDiag)

#-------------#
# DA is FALSE #
#-------------#

#SpatialCompartmentalModelWithDA(model, startDate, selectedCountry = "Czech Republic", directOutput, rasterAgg, alpha = 0.0015, beta = 0.05, gamma = 0.16, sigma = 0.065, delta = 0.002, radius, lambda, timestep = 120, seedFile = "seeddata/CZE_InitialSeedDataSep 1, 2020.csv", seedRadius, deterministic, isCropped = F, level1Names = NULL, DA = F, "observeddata/Ebola_Health_Zones_LatLon.csv", "observeddata/Ebola_Incidence_Data.xlsx", "observeddata/Ebola_Death_Data.xlsx", varCovarFunc = "DBD", QVar, QCorrLength, nbhd, psiDiag)
#SpatialCompartmentalModelWithDA(model, startDate, selectedCountry, directOutput, rasterAgg, alpha, beta, gamma = gamma/10, sigma, delta, radius, lambda, timestep = 200, seedFile = "seeddata/COD_InitialSeedData.csv", seedRadius, deterministic, isCropped, level1Names, DA = F, "observeddata/Ebola_Health_Zones_LatLon.csv", "observeddata/Ebola_Incidence_Data.xlsx", "observeddata/Ebola_Death_Data.xlsx", varCovarFunc = "DBD", QVar, QCorrLength, nbhd, psiDiag)
