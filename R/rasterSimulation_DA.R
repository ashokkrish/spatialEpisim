# options(conflicts.policy = list(warn = FALSE))
# shhh <- suppressPackageStartupMessages # It's a library, so shhh!
# shhh(library(av))
# shhh(library(countrycode))
# shhh(library(cptcity))
# shhh(library(lattice))
# shhh(library(lubridate))
# shhh(library(magick))
# shhh(library(Matrix))
# options("rgdal_show_exportToProj4_warnings"="none")
# shhh(library(rgdal, warn.conflicts=FALSE))
# shhh(library(raster, warn.conflicts=FALSE))
# shhh(library(rasterVis))
# shhh(library(rstudioapi))
# shhh(library(readxl))
# shhh(library(sp))
# shhh(library(sf))     # classes and functions for vector data
# shhh(library(terra, warn.conflicts=FALSE))
# shhh(library(writexl))
# 
# source("R/rasterStack.R")  # This code generates the base RasterStack
# source("R/rasterPlot.R")   # This code generates the .png and .mp4 files for RasterStack
# source("R/distwtRaster.R") # This code sets the Euclidean distance and the weight matrix
# 
# # Arguments
# 
# # model, startDate, selectedCountry, directOutput, rasterAgg,
# # alpha, beta, gamma, sigma, delta, radius, lambda, timestep, seedFile,
# # deterministic, isCropped, level1Names, DA = F,
# # sitRepData, dataI, dataD, QMatType, QVar, QCorrLength
# 
# model <- "SVEIRD" # "SEIRD"
# startDate <- "2018-08-01" # today()
# selectedCountry <- "Democratic Republic of Congo"
# directOutput <- F
# rasterAgg <- 10
# 
# t <- 1
# 
# #------------#
# # Parameters #
# #------------#
# 
# alpha <- 0 #.0001  # Daily fraction that move out of the susceptible compartment to the vaccinated compartment
# beta  <- 0.0055*3    # Daily fraction that move out of the susceptible compartment to the exposed compartment
# gamma <- 0.0055    # Daily fraction that move out of the exposed compartment to the infectious compartment **** Gamma has to remain the same for all scenarios
# sigma <- 0 #.01    # Daily fraction that move out of the infectious compartment to the recovered compartment
# delta <- 0.02    # Daily fraction that move out of the infectious compartment to the dead compartment
# 
# radius <- 1 # apply formula as discussed
# lambda <- 15
# timestep <- 36 # 440
# 
# seedFile <- "seeddata/COD_InitialSeedData.csv"
# 
# deterministic <- T
# isCropped <- T
# level1Names <- c("Ituri", "Nord-Kivu")
# 
# DA <- T
# 
# sitRepData <- "observeddata/Ebola_Health_Zones_LatLon.csv"
# dataI <- "observeddata/Ebola_Incidence_Data.xlsx"
# dataD <- "observeddata/Ebola_Death_Data.xlsx"
# 
# QMatType <- "DBD"
# QVar <- 1
# QCorrLength <- 0.8
# 
# 
# 
# #---------------------------------------#
# # Compartmental model simulation begins #
# #---------------------------------------#
# 
#  # SpatialCompartmentalModelWithDA <- function(model, startDate, selectedCountry, directOutput, rasterAgg, alpha, beta, gamma, sigma, delta, radius, lambda, timestep, seedFile, deterministic, isCropped, level1Names, DA = F, sitRepData, dataI, dataD, QMatType, QVar, QCorrLength)
#  # {
#   unlink("www/MP4", recursive = TRUE) # Delete the MP4
#   dir.create("www/MP4")               # Create empty MP4 folder before running new simulation
#   dir.create("www/MP4/paper")         # Create paper folder before for plots without labels
# 
#   inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c') #Converts country name to ISO Alpha
#   rs <- createRasterStack(selectedCountry, rasterAgg, isCropped, level1Names)
# 
#   Susceptible <- rs$rasterStack$Susceptible
#   Vaccinated <- rs$rasterStack$Vaccinated
#   Exposed <- rs$rasterStack$Exposed
#   Infected <- rs$rasterStack$Infected
#   Recovered <- rs$rasterStack$Recovered
#   Dead <- rs$rasterStack$Dead
# 
#   Inhabitable <- rs$rasterStack$Inhabitable
#   Level1Raster <- rs$rasterStack$Level1Raster
# 
#   Level1Identifier <- rs$Level1Identifier
# 
#   # print(rs$rasterStack)
#   # print(Level1Identifier$NAME_1)  # List of states/provinces/regions
# 
#   # plot(Level1Raster)
#   # plot(Level1Identifier, add = TRUE)
# 
#   # print(Susceptible);  print(Vaccinated); print(Exposed); print(Infected); print(Recovered); print(Dead)
# 
#   # dim(Susceptible); dim(Vaccinated); dim(Exposed); dim(Infected); dim(Recovered); dim(Dead); dim(Inhabitable); dim(Level1Raster)
# 
#   # print(table(values(Inhabitable)))
# 
#   names <- c("Date", "N", "S", "V", "E", "I", "R", "D",
#              "newV", "newE", "newI", "newR","newD", "cumE", "cumI", "Alpha", "Beta", "Gamma", "Sigma", "Delta",
#              "Radius", "Lambda", "Model", "DA")
# 
#   summary <- data.frame(matrix(data = 0, ncol = length(names), nrow = timestep))
# 
#   colnames(summary) <- names
# 
#   nrows <- rs$nRows # nrow(rs$rasterStack) #
#   ncols <- rs$nCols # ncol(rs$rasterStack) #
# 
#   p <- nrows * ncols
# 
#   # ULCornerLongitude <- extent(rs$rasterStack)[1] + res(rs$rasterStack)[1]/2 # 27.165417 for COD
#   # ULCornerLatitude <- extent(rs$rasterStack)[4] - res(rs$rasterStack)[1]/2  # 3.682917 for COD
#   #
#   # LLCornerLongitude <- extent(rs$rasterStack)[1] + res(rs$rasterStack)[1]/2 # 27.165417 for COD
#   # LLCornerLatitude <- extent(rs$rasterStack)[3] + res(rs$rasterStack)[1]/2  # -2.150416 for COD
# 
#   ULCornerLongitude <- xmax(rs$rasterStack)
#   ULCornerLatitude <- ymax(rs$rasterStack)
# 
#   LLCornerLongitude <- xmin(rs$rasterStack)
#   LLCornerLatitude <- ymin(rs$rasterStack)
# 
#   print(c(ULCornerLongitude, ULCornerLatitude, LLCornerLongitude, LLCornerLatitude))
# 
#   hcellSize <- res(rs$rasterStack)[1]
#   vcellSize <- res(rs$rasterStack)[2]
# 
#   #------------------------#
#   # Initial seed locations #
#   #------------------------#
# 
#   if (missing(seedFile)){
#     seedFolder <- "seeddata/"         # .csv or .xlsx files may be stored in local seeddata/ folder
#     seedData <<- read_excel(paste0(seedFolder, inputISO, "_InitialSeedData.csv"), header = T)
#     seedData <<- read_excel(paste0(seedFolder, inputISO, "_InitialSeedData.xlsx"), 1, header = T)
#   } else {
#     seedData <<- read.csv(seedFile)
#   }
# 
#   # print(seedData)
# 
#   numLocations <- dim(seedData)[1]
#   # print(numLocations)
# 
#   midLongitude <- (LLCornerLongitude + ULCornerLongitude)/2
#   midCol <- trunc(abs((midLongitude - (ULCornerLongitude-hcellSize/2))/hcellSize)) + 1
# 
#   for (ff in 1:numLocations)
#   {
#     #print(paste("Seed location = ", seedData[ff,1]))
# 
#     row <- trunc(abs((seedData[ff,2] - (ULCornerLatitude+vcellSize/2))/vcellSize)) + 1
#     col <- trunc(abs((seedData[ff,3] - (ULCornerLongitude-hcellSize/2))/hcellSize)) + 1
# 
#     # print(paste("row = ", row, "col = ", col))
#     # print(Inhabitable[(row-radius):(row+radius),(col-radius):(col+radius)])
#     # print(sum(Inhabitable[(row-radius):(row+radius),(col-radius):(col+radius)]))
# 
#     numCellsPerRegion    <- (2*radius + 1)^2 # Seed the initial infections equitably in a Moore Neighborhood of cells
#     newVaccinatedPerCell <- seedData[ff,4]/numCellsPerRegion    #round(seedData[ff,8]/numCellsPerRegion)
#     newExpPerCell        <- seedData[ff,5]/numCellsPerRegion    #round(seedData[ff,5]/numCellsPerRegion)
#     newInfPerCell        <- seedData[ff,6]/numCellsPerRegion    #round(seedData[ff,4]/numCellsPerRegion)
#     newRecoveredPerCell  <- seedData[ff,7]/numCellsPerRegion    #round(seedData[ff,6]/numCellsPerRegion)
#     newDeadPerCell       <- seedData[ff,8]/numCellsPerRegion    #round(seedData[ff,7]/numCellsPerRegion)
# 
#     # print(newVaccinatedPerCell)
#     # print(newExpPerCell)
#     # print(newInfPerCell)
#     # print(newRecoveredPerCell)
#     # print(newDeadPerCell)
# 
#     Vaccinated[(row-radius):(row+radius),(col-radius):(col+radius)] <- Vaccinated[(row-radius):(row+radius),(col-radius):(col+radius)] + newVaccinatedPerCell
#     Exposed[(row-radius):(row+radius),(col-radius):(col+radius)] <- Exposed[(row-radius):(row+radius),(col-radius):(col+radius)] + newExpPerCell
#     Infected[(row-radius):(row+radius),(col-radius):(col+radius)] <- Infected[(row-radius):(row+radius),(col-radius):(col+radius)] + newInfPerCell
#     Recovered[(row-radius):(row+radius),(col-radius):(col+radius)] <- Recovered[(row-radius):(row+radius),(col-radius):(col+radius)] + newRecoveredPerCell
#     Dead[(row-radius):(row+radius),(col-radius):(col+radius)] <- Dead[(row-radius):(row+radius),(col-radius):(col+radius)] + newDeadPerCell
# 
#     #print(Exposed)
#     #print(paste("Susceptible = ", sum(values(Susceptible))))
#   }
# 
#   # ramp <- c('#FFFFFF', '#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
#   # pal <- colorRampPalette(ramp)
#   #
#   # plot(Infected, col = pal(8)[-2], axes = T, cex.main = 1, main = "Location of Initial Infections", plg = list(title = expression(bold("Persons")), title.cex = 1, horiz=TRUE, x.intersp=0.6, inset=c(0, -0.2), cex=1.15), pax = list(cex.axis=1.15), legend=TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
#   #
#   # plot(Level1Identifier, add = TRUE)
#   #
#   # plot(Inhabitable, col = pal(8)[-2], axes = T, cex.main = 1, main = "Inhabitable Cells", plg = list(title = expression(bold("Persons")), title.cex = 1, horiz=TRUE, x.intersp=0.6, inset=c(0, -0.2), cex=1.15), pax = list(cex.axis=1.15), legend=TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
#   #
#   # plot(Level1Identifier, add = TRUE)
# 
#   #writeRaster(Infected, "seed.tif", overwrite = TRUE)
# 
#   sumS <- sum(values(Susceptible)); sumV <- sum(values(Vaccinated));
#   sumE <- sum(values(Exposed)); sumI <- sum(values(Infected));
#   sumR <- sum(values(Recovered)); sumD <- sum(values(Dead))
# 
#   # print(sumS)
#   # print(sumV)
#   # print(sumE)
#   # print(sumI)
#   # print(sumR)
#   # print(sumD)
# 
#   propVaccinated <- sumV/sumS
#   propExposed <- sumE/sumS
#   propInfected <- sumI/sumS
#   propRecovered <- sumR/sumS
#   propDead <- sumD/sumS
# 
#   # print(propVaccinated)
#   # print(propExposed)
#   # print(propInfected)
#   # print(propRecovered)
#   # print(propDead)
# 
#   print(paste("Susceptible Count before removing initial seed values: ", sumS))
# 
#   Susceptible <- Susceptible - (Susceptible*propVaccinated) - (Susceptible*propExposed) - (Susceptible*propInfected) - (Susceptible*propRecovered) - (Susceptible*propDead)
# 
#   sumS <- sum(values(Susceptible))
# 
#   print(paste("Susceptible Count after removing initial seed values: ", sumS)) #sum(values(Susceptible)<0)
# 
#   datarow <- 1 # 0 # pre-allocating the row from which we read the data to assimilate each week
#   cumVaccinated <- round(sumV)
#   cumExposed <- round(sumE)
#   cumInfected <- round(sumI)
#   cumRecovered <- round(sumR)
#   cumDead <- round(sumD)
# 
#   cumIncidence <- round(sumI)
# 
#   ################# DA Begins ##################
# 
#   if (DA == T)
#   {
#     #-------------------------------------------#
#     # Import the Ebola Incidence and Death Data #
#     #-------------------------------------------#
# 
#     incidence_data <- read_excel(dataI)
#     death_data <- read_excel(dataD)
# 
#     print(paste("Dimension of Incidence Matrix: ", dim(incidence_data)[1], dim(incidence_data)[2]))
# 
#     print(paste("Dimension of Death Matrix: ", dim(death_data)[1], dim(death_data)[2]))
# 
#     source("R/H_matrix.R") # read in H matrix code
# 
#     Hlist <- generateLIO(rs$rasterStack, sitRepData, states_observable =  2)
#     Hmat <- Hlist$Hmat
#     Locations <- Hlist$Locations
#     nHealthZones <- as.numeric(dim(Locations)[1])
# 
#     #--------------------#
#     # Read in QHt matrix #
#     #--------------------#
# 
#     source("R/Q_matrix_ver4.R")
# 
#     QHt <- generateQHt(Hlist, QMatType, QVar, QCorrLength, makeQ = F)
# 
#     HQHt <- Hmat%*%QHt$QHt
# 
#     print(HQHt[1:5, 1:5])
#   }
#   ################# DA Ends ##################
# 
#   #-------------------------------#
#   # MAIN LOOP FOR TIME INCREMENTS #
#   #-------------------------------#
# 
#   allRasters <- vector(mode ="list", length = timestep)
# 
#   for (t in 1:timestep)
#   {					# time increments
#     print(paste("time = ", t))
# 
#     summary[t, 1]  <- toString(as.Date(startDate) + days(t - 1)) # Print the date at each time step
#     summary[t, 2]  <- round(sumS + sumV + sumE + sumI + sumR + sumD)
#     summary[t, 3]  <- round(sumS)
#     summary[t, 4]  <- round(sumV)            # Absorbing state
#     summary[t, 5]  <- round(sumE)            # This is the prevalence (active exposed cases) at time t, NOT the cumulative sum
#     summary[t, 6]  <- round(sumI)            # This is the prevalence (active infectious cases) at time t, NOT the cumulative sum
#     summary[t, 7]  <- round(cumRecovered)    # round(sumR)   # Absorbing state
#     summary[t, 8]  <- round(cumDead)         # round(sumD)   # Absorbing state
# 
#     summary[t, 14]  <- cumExposed
#     summary[t, 15]  <- cumInfected
#     summary[t, 16]  <- alpha
#     summary[t, 17]  <- beta
#     summary[t, 18]  <- gamma
#     summary[t, 19]  <- sigma
#     summary[t, 20]  <- delta
# 
#     summary[t, 21]  <- radius
#     summary[t, 22]  <- lambda
#     summary[t, 23]  <- model
# 
#     nextSusceptible <- nextVaccinated <- nextExposed <- nextInfected <- nextRecovered <- nextDead <- matrix(0, nrows, ncols, byrow = T)
# 
#     dailyVaccinated <- dailyExposed <- dailyInfected <- dailyRecovered <- dailyDead <- 0
# 
#     #-------------------------------#
#     # Generating the I_tilda matrix #
#     #-------------------------------#
# 
#     I_tilda <- wtd_nbrs_sum(input_matrix = raster::as.matrix(Infected), radius = radius, lambda = lambda)
# 
#     for(i in 1:nrows)
#     { 							# nrows
#       for(j in 1:ncols)
#       {							# ncols
#         if (Inhabitable[i,j] == 1)
#         {						     # Inhabitable
#           nLiving <- newVaccinated <- nearbyInfected <- newExposed <- newInfected <- newRecovered <- newDead <- 0
# 
#           nLiving <- Susceptible[i,j] + Vaccinated[i,j] + Exposed[i,j] + Infected[i,j] + Recovered[i,j]
# 
#           if (nLiving > 0)			# nLiving
#           {
#             if (Susceptible[i,j] >= 1)
#             {
#               nearbyInfected <- I_tilda[i,j]
# 
#               #--------------------------------------------------------------------#
#               # Some susceptible people are going to be newly vaccinated           #
#               #--------------------------------------------------------------------#
# 
#               newVaccinated <- alpha*Susceptible[i,j]
# 
#               dailyVaccinated <- dailyVaccinated + newVaccinated
# 
#               #--------------------------------------------------------------------#
#               # Some susceptible people who come in contact with nearby infected   #
#               # are going to be newly exposed                                      #
#               #--------------------------------------------------------------------#
# 
#               if (nearbyInfected >= 1)
#               {
#                 pSusceptible <- Susceptible[i,j]/nLiving
#                 if (deterministic){
#                   newExposed <- beta*pSusceptible*nearbyInfected
#                 } else {
#                   rpois(1, beta*pSusceptible*nearbyInfected)
#                 }
# 
#                 dailyExposed <- dailyExposed + newExposed
#                 cumExposed <- cumExposed + newExposed
#               }
#             }
# 
#             #----------------------------------------------------------#
#             # Some exposed people are going to become newly infectious #
#             #----------------------------------------------------------#
#             if (Exposed[i,j] >= 1)
#             {
#               newInfected <- gamma*Exposed[i,j]
# 
#               dailyInfected <- dailyInfected + newInfected
#               cumInfected   <- cumInfected + newInfected
#             }
# 
#             #-----------------------------------------------------------#
#             # Some infectious people are going to either recover or die #
#             #-----------------------------------------------------------#
#             if (Infected[i,j] >= 1)
#             {
#               newRecovered <- sigma*Infected[i,j]
# 
#               dailyRecovered <- dailyRecovered + newRecovered
#               cumRecovered <- cumRecovered + newRecovered
# 
#               newDead <- delta*Infected[i,j]
# 
#               dailyDead <- dailyDead + newDead
#               cumDead <- cumDead + newDead
#             }
# 
#             #-----------------------------------#
#             # Store the next state of each cell #
#             #-----------------------------------#
# 
#             nextSusceptible[i,j] <- Susceptible[i,j] - newExposed - newVaccinated
#             nextVaccinated[i,j] <- Vaccinated[i,j] + newVaccinated
#             nextExposed[i,j] <- Exposed[i,j] + newExposed - newInfected
#             nextInfected[i,j] <- Infected[i,j] + newInfected - newDead - newRecovered
#             nextRecovered[i,j] <- Recovered[i,j] + newRecovered
#             nextDead[i,j] <- Dead[i,j] + newDead
#           }					# nLiving
#         } 					# Inhabitable
#       }							# ncols
#     } 							# nrows
# 
#     nextSusceptible[nextSusceptible < 0] = 0
#     nextVaccinated[nextVaccinated < 0] = 0
#     nextExposed[nextExposed < 0] = 0
#     nextInfected[nextInfected < 0] = 0
#     nextRecovered[nextRecovered < 0] = 0
#     nextDead[nextDead < 0] = 0
# 
#     Susceptible <- nextSusceptible
#     Vaccinated <- nextVaccinated
#     Exposed <- nextExposed
#     Infected <- nextInfected
#     Recovered <- nextRecovered
#     Dead <- nextDead
# 
#     # plot(Infected, col = pal(8)[-2], axes = T, cex.main = 1, main = "Location of Initial Infections", plg = list(title = expression(bold("Persons")), title.cex = 1, horiz=TRUE, x.intersp=0.6, inset=c(0, -0.2), cex=1.15), pax = list(cex.axis=1.15), legend=TRUE, mar=c(8.5, 3.5, 2.5, 2.5), add = F)
#     #
#     # plot(Level1Identifier, add = TRUE)
#     #
#     # print(Infected)
# 
#     # infectedRaster <- raster(Infected)
# 
#     Susceptible <- raster(Susceptible)
#     Vaccinated <- raster(Vaccinated)
#     Exposed <- raster(Exposed)
#     Infected <- raster(Infected)
#     Recovered <- raster(Recovered)
#     Dead <- raster(Dead)
# 
#     extent(Susceptible) <- extent(Vaccinated) <- extent(Exposed) <- extent(Infected) <- extent(Recovered) <- extent(Dead) <- extent(rs$rasterStack)
# 
#     # print(extent(infectedRaster))
#     # print(extent(rs$rasterStack))
#     #
#     # writeRaster(infectedRaster, file = "infectedRaster.tif", overwrite = TRUE)
#     #
# 
#     rs$rasterStack$Susceptible <- Susceptible
#     rs$rasterStack$Vaccinated <- Vaccinated
#     rs$rasterStack$Exposed <- Exposed
#     rs$rasterStack$Infected <- Infected
#     rs$rasterStack$Recovered <- Recovered
#     rs$rasterStack$Dead <- Dead
# 
#     # print('check')
# 
#     summary[t, 9]   <- dailyVaccinated
#     summary[t, 10]  <- dailyExposed
#     summary[t, 11]  <- dailyInfected
#     summary[t, 12]  <- dailyRecovered
#     summary[t, 13]  <- dailyDead
# 
#     sumS <- sum(values(Susceptible)); sumV <- sum(values(Vaccinated));
#     sumE <- sum(values(Exposed)); sumI <- sum(values(Infected));
#     sumR <- sum(values(Recovered)); sumD <- sum(values(Dead))
# 
#     #print(sumS)
# 
#     ########## DA Begins ##########
# 
#     #setwd(outputDir) # Change working directory to output folder
# 
#     if (DA == T)
#     {                     # DA T/F
#       #NewoutputDir <- paste(outputDir, "/DA", sep="") # The directory for output files
#       #if (!(file.exists(NewoutputDir))){
#         #dir.create("DA") # Folder to store output .nc files
# 
#       #setwd(NewoutputDir) # Change working directory to output folder
# 
#       if (t %% 7 == 0)
#       {                   # elapsed week
#         datarow <- datarow + 1
# 
#         if (datarow < 76)
#         {                 # datarow cap
#           #----------------------------------------#
#           # Write forecast (prior) state to matrix #
#           #----------------------------------------#
# 
#           #print(paste("Xf is printed on day", t))
# 
#           #---------------------#
#           # OSI: forecast state #
#           #---------------------#
#           # We track "Infectious" and "Dead" epidemic states only
#           Infected <- as.matrix(Infected)
#           Dead <- as.matrix(Dead)
# 
#           #print(Infected[21:40,])
# 
#           Xf.OSI <- t(cbind(t(as.vector(Infected)), t(as.vector(Dead))))
# 
#           print(paste("Dimension of the state vector:")); print(dim(Xf.OSI))
# 
#           #print(sum(Xf.OSI))
#           #table(Xf.OSI)
# 
#           HXf <- Hmat%*%Xf.OSI
#           #print(HXf)
#           print(dim(HXf))
#           #print(sum(HXf))
# 
#           #----------------------------------------------#
#           # Importing DRC Ebola Incidence and Death Data #
#           #----------------------------------------------#
# 
#           incidence <- as.vector(incidence_data[datarow, 1:nHealthZones+2]) # Pick a row every 7 days, select third column through to the last column
#           death <- as.vector(death_data[datarow, 1:nHealthZones+2])         # Pick a row every 7 days, select third column through to the last column
# 
#           # if (datarow > 1)
#           # {                 # datarow > 1
#           #    prevWHOIncidence <- sum(as.vector(incidence_data[1:(datarow-1), 3:nHealthZones+2]))
#           #    currWHOIncidence <- sum(as.vector(incidence_data[1:datarow, 3:nHealthZones+2]))
#           #
#           #    currSIMIncidence <-
#           #    prevSIMIncidence <-
#           #
#           #    slopeWHO <- (currWHOIncidence - prevWHOIncidence)/nDaysPerUnit
#           #    slopeSIM <- (currSIMIncidence - prevSIMIncidence)/nDaysPerUnit
#           #
#           #    phi <- slopeWHO/slopeSIM
#           #
#           #   #beta = phi*beta
#           #
#           # }                 # datarow > 1
# 
#           Dvector <- t(cbind(t(incidence), t(death)))
# 
#           # print(Dvector)
#            print(dim(Dvector))
#           # sum(incidence)
#           # sum(death)
# 
#           #-------------------------------------#
#           # Measurement error covariance matrix #
#           #-------------------------------------#
# 
#           # sum(Dvector < 1)
#           Dvector_revised <- ifelse(Dvector < 1, 1, Dvector) # If a diagonal entry is zero change it to 1.
#           # sum(Dvector_revised < 1)
# 
#           M <- diag(as.vector(Dvector_revised))
# 
#           # library(MASS)
#           # write.matrix(M, file = 'mes_err.csv')
# 
#           # print(M) # check if D vector needs to be really revised
# 
#           # levelplot(M, col.regions= colorRampPalette(c("white", "red", "blue")))
#           # table(M)
#           # diag(M)
#           # det(M)
# 
#           #---------------------#
#           # Optimal Kalman Gain #
#           #---------------------#
# 
#           # QHt  <- Qf.OSI%*%t(Hmat) # Calculate this only once
#           # HQHt <- Hmat%*%QHt       # Calculate this only once
# 
#           # sum(QHt < 0)
#           # sum(HQHt < 0)
# 
#           # dim(HQHt)
# 
#           # levelplot(as.matrix(HQHt), col.regions= colorRampPalette(c("white", "red", "blue")))
# 
#           # diag(HQHt)
#           # det(HQHt)
#           # eigen(HQHt)$values # HQHt is positive definite since all of its eigenvalues are strictly positive.
#           # sum(eigen(HQHt)$values)
#           #
#           # log10(max(eigen(HQHt)$values)/min(eigen(HQHt)$values))
#           #
#           # det(solve(HQHt))
#           # eigen(solve(HQHt))$values # Inverse of HQHt is also positive definite since all of its eigenvalues are strictly positive.
#           # sum(eigen(solve(HQHt))$values)
# 
#           # The gain matrix, Ke.OSI, determines how the observational data are to be assimilated
#           Ke.OSI <- QHt$QHt%*%solve(HQHt + M)
#           #write.matrix(Ke.OSI, file = 'Kal_Gain.csv')#solve((HQHt + M), t(QHt))
# 
#           #print(paste("Dimension of the Kalman Gain Matrix:")); print(dim(Ke.OSI))
# 
#           # Questions
#           # Can the Kalman gain matrix have negative values?
#           # Can the innovation or measurement residual have negative values?
# 
#           #------------------------------------#
#           # Innovation or measurement residual #
#           #------------------------------------#
#           #HXf <- t(t(as.numeric(Dvector)))
#           Y <- t(t(as.numeric(Dvector))) - HXf
# 
#           #---------------------------------#
#           # OSI update step: analysis state #
#           #---------------------------------#
# 
#           Xa.OSI <- Xf.OSI + Ke.OSI%*%Y
# 
#           #Xa.OSI[Xa.OSI < 0] <- 0 # This will set all negative values to zero
# 
#           #Xa.OSI <- abs(Xf.OSI + Ke.OSI%*%Y)
# 
#           # max(Ke.OSI%*%Y)
#           # min(Ke.OSI%*%Y)
#           #
#           # sum(round(Ke.OSI%*%Y))
#           #
#           # print(sum(Xa.OSI))
#           # print(tail(sort(Xa.OSI), 30))
#           # print(sum(Xf.OSI))
# 
#           ###########################
# 
#           # sum(Xf.OSI < 0)         # Number of negative values in Xf.OSI.
#           #
#           # sum(QHt < 0)            # Number of negative values in QHt.
#           #
#           # sum(HQHt < 0)           # Number of negative values in HQHt.
#           #
#           # sum(Y < 0)              # Number of negative values in Y.
#           #
#           # sum(Ke.OSI < 0)         # Number of negative values in Ke.OSI.
#           #
#           # sum(Ke.OSI%*%Y < 0)     # Number of negative values in Ke.OSI*Y.
#           #
#           # sum(Xa.OSI < 0)         # Number of negative values in Xa.OSI.
# 
#           ###########################
# 
#           # HXf <- Hmat%*%Xf.OSI
#           # print(dim(HXf))
#           # print(sum(HXf))
#           #
#           # print(dim(Dvector))
#           # sum(incidence)
#           # sum(death)
#           #
#           # HXa <- Hmat%*%Xa.OSI
#           # print(dim(HXa))
#           # print(sum(HXa))
# 
#           # 42.10018 with Q_matrix_ver1
#           # 43.00399 with Q_matrix_ver2
# 
#           #cbind(HXf, round(HXa), HXa)
# 
#           #print(cbind(Dvector, Hmat%*%Xf.OSI, Y, round(Hmat%*%Xa.OSI)))
# 
#           # NOTE: when restacking make sure byrow = T.
# 
#           I <- matrix(Xa.OSI[1:p], nrow = nrows, ncol = ncols, byrow = F)
#           #write.matrix(I, file = 'infected.csv')
# 
#           # I <- raster(I)
#           # print(I)
#           # extent(I) <- extent(rs$rasterStack)
#           # print(I)
# 
#           #I[I < 0.5] <- 0 # Prevent tiny values for the number of infectious
# 
#           D <- matrix(Xa.OSI[(p+1):(2*p)], nrow = nrows, ncol = ncols, byrow = F)
# 
#           D[D < 1] <- 0 # Prevent tiny values for the number of dead
# 
#           dim(Xa.OSI); dim(I); dim(D); min(I); min(D); max(I); max(D)
# 
#           # For all uninhabitable cells set the number of infected and dead = 0. THIS IS VERY CRITICAL!!!
# 
#           # for(i in 1:nrows)
#           # { 								# nrows
#           #   for(j in 1:ncols)
#           #   {							  # ncols
#           #     if (rs$rasterStack$Inhabitable[i,j] == 0)
#           #     {						  # Inhabitable
#           #       I[i,j] <- D[i,j] <- 0
#           #     }
#           #   }
#           # }
#           values(rs$rasterStack$Infected) <- I
#           values(rs$rasterStack$Dead) <- D
#           Infected <- rs$rasterStack$Infected
#           Dead <- rs$rasterStack$Dead
#          } # datarow cap
#         } # If t is divisible by 7
#       # elapsed week
#     }
#     allRasters[[t]] <- rs
#   }
# # DA T/F
#     ########## DA Ends ##########
# 
#      # save(rs$rasterStack[["Infected"]], file = "infectedRaster.RData")
# 
#     # plot(allRasters[[t]]$rasterStack[["Infected"]], col = pal(8)[-2], axes = T, cex.main = 1, main = "Location of Initial Infections", plg = list(title = expression(bold("Persons")), title.cex = 1, horiz=TRUE, x.intersp=0.6, inset=c(0, -0.2), cex=1.15), pax = list(cex.axis=1.15), legend=TRUE, mar=c(8.5, 3.5, 2.5, 2.5), add = F)
#     #
#     # plot(Level1Identifier, add = TRUE)
#   		      	# time increments
# 
#   ########## DA Begins ##########
#   #
#   # setwd(outputDir) # Change working directory to output folder
#   #
#   # if (DA == T)
#   # {                     # DA T/F
#   #   NewoutputDir <- paste(outputDir, "/DA", sep="") # The directory for output files
#   #   if (!(file.exists(NewoutputDir))){
#   #     dir.create("DA") # Folder to store output .nc files
#   #   }
#   #   setwd(NewoutputDir) # Change working directory to output folder
#   #
#   #   if (t %% 7 == 0)
#   #   {                   # elapsed week
#   #     datarow <- datarow + 1
#   #
#   #     if (datarow < 76)
#   #     {                 # datarow cap
#   #       #-----------------------------------------------#
#   #       # Write forecast (prior) state to a NetCDF file #
#   #       #-----------------------------------------------#
#   #
#   #       #print(paste("Xf is printed on day", t))
#   #
#   #       #---------------------#
#   #       # OSI: forecast state #
#   #       #---------------------#
#   #
#   #       # We track "Infectious" and "Dead" epidemic states only
#   #
#   #       Xf.OSI <- rbind(cbind(as.vector(t(I))), cbind(as.vector(t(D))))
#   #
#   #       #print(paste("Dimension of the state vector:")); print(dim(Xf.OSI))
#   #
#   #       #print(sum(Xf.OSI))
#   #       #table(Xf.OSI)
#   #
#   #       HXf <- Hmat%*%Xf.OSI
#   #       #print(dim(HXf))
#   #       #print(sum(HXf))
#   #
#   #       #----------------------------------------------#
#   #       # Importing DRC Ebola Incidence and Death Data #
#   #       #----------------------------------------------#
#   #
#   #       incidence <- as.vector(incidence_data[datarow, 3:nHealthZones+2]) # Pick a row every 7 days, select third column through to the last column
#   #       death <- as.vector(death_data[datarow, 3:nHealthZones+2])         # Pick a row every 7 days, select third column through to the last column
#   #
#   #       # if (datarow > 1)
#   #       # {                 # datarow > 1
#   #       #    prevWHOIncidence <- sum(as.vector(incidence_data[1:(datarow-1), 3:nHealthZones+2]))
#   #       #    currWHOIncidence <- sum(as.vector(incidence_data[1:datarow, 3:nHealthZones+2]))
#   #       #
#   #       #    currSIMIncidence <-
#   #       #    prevSIMIncidence <-
#   #       #
#   #       #    slopeWHO <- (currWHOIncidence - prevWHOIncidence)/nDaysPerUnit
#   #       #    slopeSIM <- (currSIMIncidence - prevSIMIncidence)/nDaysPerUnit
#   #       #
#   #       #    phi <- slopeWHO/slopeSIM
#   #       #
#   #       #   #beta = phi*beta
#   #       #
#   #       # }                 # datarow > 1
#   #
#   #       Dvector <- rbind(t(incidence), t(death))
#   #
#   #       #print(dim(Dvector))
#   #       sum(incidence)
#   #       sum(death)
#   #
#   #       #-------------------------------------#
#   #       # Measurement error covariance matrix #
#   #       #-------------------------------------#
#   #
#   #       sum(Dvector < 1)
#   #       Dvector_revised <- ifelse(Dvector < 1, 1, Dvector) # If a diagonal entry is zero change it to 1.
#   #       sum(Dvector_revised < 1)
#   #
#   #       M <- diag(as.vector(Dvector_revised)) # check if D vector needs to be really revised
#   #
#   #       #levelplot(M, col.regions= colorRampPalette(c("white", "red", "blue")))
#   #       # table(M)
#   #       # diag(M)
#   #       # det(M)
#   #       #print(M)
#   #
#   #       #---------------------#
#   #       # Optimal Kalman Gain #
#   #       #---------------------#
#   #
#   #       # QHt  <- Qf.OSI%*%t(Hmat) # Calculate this only once
#   #       # HQHt <- Hmat%*%QHt       # Calculate this only once
#   #
#   #       # sum(QHt < 0)
#   #       # sum(HQHt < 0)
#   #
#   #       # dim(HQHt)
#   #
#   #       # levelplot(as.matrix(HQHt), col.regions= colorRampPalette(c("white", "red", "blue")))
#   #
#   #       # diag(HQHt)
#   #       # det(HQHt)
#   #       # eigen(HQHt)$values # HQHt is positive definite since all of its eigenvalues are strictly positive.
#   #       # sum(eigen(HQHt)$values)
#   #       #
#   #       # log10(max(eigen(HQHt)$values)/min(eigen(HQHt)$values))
#   #       #
#   #       # det(solve(HQHt))
#   #       # eigen(solve(HQHt))$values # Inverse of HQHt is also positive definite since all of its eigenvalues are strictly positive.
#   #       # sum(eigen(solve(HQHt))$values)
#   #
#   #       # The gain matrix, Ke.OSI, determines how the observational data are to be assimilated
#   #
#   #       Ke.OSI <- QHt%*%solve(HQHt + M)  #solve((HQHt + M), t(QHt))
#   #
#   #       #print(paste("Dimension of the Kalman Gain Matrix:")); print(dim(Ke.OSI))
#   #
#   #       # Questions
#   #       # Can the Kalman gain matrix have negative values?
#   #       # Can the innovation or measurement residual have negative values?
#   #
#   #       #------------------------------------#
#   #       # Innovation or measurement residual #
#   #       #------------------------------------#
#   #
#   #       Y <- Dvector - HXf
#   #
#   #       #---------------------------------#
#   #       # OSI update step: analysis state #
#   #       #---------------------------------#
#   #
#   #       Xa.OSI <- Xf.OSI + Ke.OSI%*%Y
#   #
#   #       #Xa.OSI[Xa.OSI < 0] <- 0 # This will set all negative values to zero
#   #
#   #       #Xa.OSI <- abs(Xf.OSI + Ke.OSI%*%Y)
#   #
#   #       # max(Ke.OSI%*%Y)
#   #       # min(Ke.OSI%*%Y)
#   #       #
#   #       # sum(round(Ke.OSI%*%Y))
#   #       #
#   #       # print(sum(Xa.OSI))
#   #       # print(tail(sort(Xa.OSI), 30))
#   #       # print(sum(Xf.OSI))
#   #
#   #       ###########################
#   #
#   #       # sum(Xf.OSI < 0)         # Number of negative values in Xf.OSI.
#   #       #
#   #       # sum(QHt < 0)            # Number of negative values in QHt.
#   #       #
#   #       # sum(HQHt < 0)           # Number of negative values in HQHt.
#   #       #
#   #       # sum(Y < 0)              # Number of negative values in Y.
#   #       #
#   #       # sum(Ke.OSI < 0)         # Number of negative values in Ke.OSI.
#   #       #
#   #       # sum(Ke.OSI%*%Y < 0)     # Number of negative values in Ke.OSI*Y.
#   #       #
#   #       # sum(Xa.OSI < 0)         # Number of negative values in Xa.OSI.
#   #
#   #       ###########################
#   #
#   #       # HXf <- Hmat%*%Xf.OSI
#   #       # print(dim(HXf))
#   #       # print(sum(HXf))
#   #       #
#   #       # print(dim(Dvector))
#   #       # sum(incidence)
#   #       # sum(death)
#   #       #
#   #       # HXa <- Hmat%*%Xa.OSI
#   #       # print(dim(HXa))
#   #       # print(sum(HXa))
#   #
#   #       # 42.10018 with Q_matrix_ver1
#   #       # 43.00399 with Q_matrix_ver2
#   #
#   #       #cbind(HXf, round(HXa), HXa)
#   #
#   #       #print(cbind(Dvector, Hmat%*%Xf.OSI, Y, round(Hmat%*%Xa.OSI)))
#   #
#   #       # NOTE: when restacking make sure byrow = T.
#   #
#   #       I <- matrix(Xa.OSI[1:p], nrow = nrows, ncol = ncols, byrow = T)
#   #
#   #       I[I < 0] <- 0 # Prevent negative values for the number of infectious
#   #
#   #       D <- matrix(Xa.OSI[(p+1):(2*p)], nrow = nrows, ncol = ncols, byrow = T)
#   #
#   #       D[D < 0] <- 0 # Prevent negative values for the number of dead
#   #
#   #       dim(Xa.OSI); dim(I); dim(D); min(I); min(D); max(I); max(D)
#   #
#   #       # For all uninhabitable cells set the number of infected and dead = 0. THIS IS VERY CRITICAL!!!
#   #
#   #       for(i in 1:nrows)
#   #       { 								# nrows
#   #         for(j in 1:ncols)
#   #         {							  # ncols
#   #           if (rs$rasterStack$Inhabitable[i,j] == 0)
#   #           {						  # Inhabitable
#   #             I[i,j] <- D[i,j] <- 0
#   #           }
#   #         }
#   #       }
#   #     }                  # datarow cap
#   #   }                   # elapsed week
#   #    # DA T/F
#   # ########## DA Ends ##########
#   #
#   # }else
#   # {
#   #   NewoutputDir <- paste(outputDir, "/No_DA", sep="") # The directory for output files
#   #   if (!(file.exists(NewoutputDir))){
#   #     dir.create("No_DA") # Folder to store output .nc files
#   #   }
#   #   setwd(NewoutputDir) # Change working directory to output folder
#   # }
#   #
#   # Print a PNG for the infected variable
#   rasterLayer <- "Infected"
#   print(allRasters[[1]]$rasterStack[[rasterLayer]])
#   maxRasterLayerVal <- 0
# 
#   for (t in 1:timestep){
#     maxRasterLayerVal <- max(maxRasterLayerVal, maxValue(allRasters[[t]]$rasterStack[[rasterLayer]]))
#   }
# 
#   ramp <- c('#FFFFFF', '#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
#   pal <- colorRampPalette(ramp)
# 
#   for (t in 1:timestep){
#     fname = paste0("MP4/", inputISO, "_", rasterLayer, "_", sprintf("%04d", t), ".png")
#     printStackLayer(rasterStack = allRasters[[t]]$rasterStack, rasterLayer = rasterLayer, directOutput = directOutput, Level1Identifier = rs$Level1Identifier, selectedCountry, rasterAgg = rasterAgg, fname = fname, maxVal = maxRasterLayerVal, includeLabels = T)
# 
#     # fname = paste0("MP4/", "paper/", inputISO, "_", rasterLayer, "_", sprintf("%04d", t), "_paper", ".png")
#     # printStackLayer(rasterStack = allRasters[[t]]$rasterStack, rasterLayer = rasterLayer, directOutput = directOutput, Level1Identifier = rs$Level1Identifier, selectedCountry, rasterAgg = rasterAgg, fname = fname, maxVal = maxRasterLayerVal, includeLabels = F)
#   }
# 
#   # MERGE THE PNGs TO A GET AN MP4 VIDEO
#   setwd("www/MP4")
#   videoDuration <- 15 # in seconds
#   av::av_encode_video(list.files(pattern = ".png"), framerate = timestep/videoDuration, output = paste0(rasterLayer, "_MP4.mp4"))
#   setwd("./../..")
# 
#   summary[is.na(summary)] <- 0
# 
#   write_xlsx(summary, path = paste0("www/MP4/", inputISO, "_summary.xlsx"), col_names = T)
# 
#   #print(tail(summary))
# 
#   return(summary)
# # } # End of function
# 
# #--------------#
# # Example Call #
# #--------------#
# 
# # SpatialCompartmentalModelWithDA(model, startDate, selectedCountry, directOutput, rasterAgg, alpha, beta, gamma, sigma, delta, radius, lambda, timestep, seedFile = "seeddata/COD_InitialSeedData.csv", deterministic, isCropped, level1Names, DA = T, "observeddata/Ebola_Health_Zones_LatLon_4zones.csv", "observeddata/Ebola_Incidence_Data_4zones.xlsx", "observeddata/Ebola_Death_Data_4zones.xlsx", QMatType = "DBD", QVar = 1, QCorrLength = 0.8)
# # SpatialCompartmentalModelWithDA(model, startDate, selectedCountry, directOutput, rasterAgg, alpha, beta, gamma, sigma, delta, radius, lambda, timestep, seedFile = "seeddata/COD_InitialSeedData.csv", deterministic, isCropped, level1Names, DA = F, "observeddata/Ebola_Health_Zones_LatLon_nozones.csv", "observeddata/Ebola_Incidence_Data_nozones.xlsx", "observeddata/Ebola_Death_Data_nozones.xlsx", QMatType = "DBD", QVar = 1, QCorrLength = 0.8)