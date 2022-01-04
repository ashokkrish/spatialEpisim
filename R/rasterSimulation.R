#rm(list = ls())

library(sp)
library(sf)
library(raster)
library(terra)
library(rgdal)
library(countrycode)
library(rstudioapi)
library(av)
library(xlsx)
library(lubridate)

source("R/rasterStack.R") # This code generates the base RasterStack
source("R/rasterPlot.R")  # This code generates the .png and .mp4 files for RasterStack

avg_euclidean_distance <- function(p, q, lambda)
{
  exp(-sqrt(sum((p - q)^2))/lambda)
}

wtd_nbrs_sum <- function(input_matrix, radius, lambda)
{
  temp_1 <- matrix(data = 0, nrow = nrow(x = input_matrix), ncol = radius)
  
  temp_2 <- matrix(data = 0, nrow = radius, ncol = ((2 * radius) + ncol(x = input_matrix)))
  
  input_matrix_modified <- rbind(temp_2, cbind(temp_1, input_matrix, temp_1), temp_2)
  
  #print(input_matrix_modified)
  #print(dim(input_matrix_modified))
  
  output_matrix <- matrix(nrow = nrow(x = input_matrix), ncol = ncol(x = input_matrix))
  
  #Generating the weight matrix
  weight_matrix <- matrix(0, nrow = 1 + 2*radius, ncol = 1 + 2*radius)
  
  for(i in seq_len(1 + 2*radius))
  {
    for(j in seq_len(1 + 2*radius))
    {
      weight_matrix[i,j] <- avg_euclidean_distance(c(i,j), c(radius+1, radius+1), lambda)
    }
  }
  
  #print(weight_matrix)
  
  for(i in seq_len(length.out = nrow(x = input_matrix)))
  {
    for(j in seq_len(length.out = ncol(x = input_matrix))) 
    {
      row_min <- (radius + (i - radius))
      row_max <- (radius + (i + radius))
      column_min <- (radius + (j - radius))
      column_max <- (radius + (j + radius))
      neighbours <- input_matrix_modified[(row_min:row_max), (column_min:column_max)]
      weighted_sum <- sum(neighbours * weight_matrix) # casewise multiplication
      output_matrix[i, j] <- weighted_sum
    }
  }
  return(output_matrix)
}

#---------------------------------------#
# Compartmental model simulation begins #
#---------------------------------------#

 SpatialCompartmentalModel <- function(model, startDate, selectedCountry, directOutput, rasterAgg, alpha, beta, gamma, sigma, delta, radius, lambda, timestep, seedFile, deterministic)
 {
  unlink("www/MP4", recursive = TRUE) # Delete the MP4
  dir.create("www/MP4")               # Create empty MP4 folder before running new simulation
  dir.create("www/MP4/paper")         # Create paper folder before for plots without labels
  
  inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c') #Converts country name to ISO Alpha
  rs <- createRasterStack(selectedCountry, rasterAgg)
  
  Susceptible <- rs$rasterStack$Susceptible
  Vaccinated <- rs$rasterStack$Vaccinated
  Exposed <- rs$rasterStack$Exposed
  Infected <- rs$rasterStack$Infected
  Recovered <- rs$rasterStack$Recovered
  Dead <- rs$rasterStack$Dead

  Inhabitable <- rs$rasterStack$Inhabitable
  Level1Raster <- rs$rasterStack$Level1Raster
  
  Level1Identifier <- rs$Level1Identifier
  
  print(rs$rasterStack)
  print(Level1Identifier$NAME_1) # List of all states/provinces/regions

  names <- c("Date", "N", "S", "V", "E", "I", "R", "D", 
             "newV", "newE", "newI", "newR","newD", "cumE", "cumI", "Alpha", "Beta", "Gamma", "Sigma", "Delta",
             "Radius", "Lambda", "Model")
  
  summary <- data.frame(matrix(data = 0, ncol = length(names), nrow = timestep))
  
  colnames(summary) <- names
  
  nrows <- nrow(rs$rasterStack)
  ncols <- ncol(rs$rasterStack)

  ULCornerLongitude <- extent(rs$rasterStack)[1] + res(rs$rasterStack)[1]/2 # 12.13208 for CZE
  ULCornerLatitude <- extent(rs$rasterStack)[4] - res(rs$rasterStack)[1]/2  # 51.01625 for CZE

  LLCornerLongitude <- extent(rs$rasterStack)[1] + res(rs$rasterStack)[1]/2 # 12.13208 for CZE
  LLCornerLatitude <- extent(rs$rasterStack)[3] + res(rs$rasterStack)[1]/2  # 48.51625 for CZE
  
  #print(c(ULCornerLongitude, ULCornerLatitude, LLCornerLongitude, LLCornerLatitude))
  
  hcellSize <- res(rs$rasterStack)[1]
  vcellSize <- res(rs$rasterStack)[2]

  Susceptible
  Vaccinated
  Exposed
  Infected
  Recovered
  Dead

  # m <- raster::as.matrix(Inhabitable)
  # 
  # Vaccinated <- raster(m) # Coerce matrix to a RasterLayer object
  # 
  # Vaccinated 
  # 
  # crs(Vaccinated) <- crs(Inhabitable)
  # extent(Vaccinated) <- extent(Inhabitable)
  # 
  # Vaccinated
  # 
  # origin(Vaccinated)
  # origin(Inhabitable)
   
  # raster::as.matrix(Inhabitable) 

  # dim(Susceptible); dim(Vaccinated); dim(Exposed); dim(Infected); dim(Recovered); dim(Dead); dim(Inhabitable); dim(Level1Raster)
  
  # print(table(values(Inhabitable)))

  #------------------------#
  # Initial seed locations #
  #------------------------#
  
  if (missing(seedFile)){
    seedFolder <- "seeddata/"         # .csv or .xlsx files may be stored in local seeddata/ folder
    seedData <<- read.csv(paste0(seedFolder, inputISO, "_InitialSeedData.csv"), header = T)
    seedData <<- read.xlsx(paste0(seedFolder, inputISO, "_InitialSeedData.xlsx"), 1, header=T)
  } else {
    seedData <<- seedFile
  }
  
  # print(seedFile)
  # print(seedData)
  
  # seedFolder <- "seeddata/"         # .csv or .xlsx files may be stored in local seeddata/ folder
  # seedData <<- read.xlsx(paste0(seedFolder, inputISO, "_InitialSeedData.xlsx"), 1, header=T)
  # seedData <<- read.csv(paste0(seedFolder, inputISO, "_InitialSeedData.csv"), header = T)

  numLocations <- dim(seedData)[1] #nrow(data())
  
  #print(numLocations)
  
  for (ff in 1:numLocations)
  {
    #print(paste("Region Identifier = ", seedData[ff,9]))
    
    row <- trunc(abs((seedData[ff,2] - (ULCornerLatitude+vcellSize/2))/vcellSize)) + 1
    col <- trunc(abs((seedData[ff,3] - (ULCornerLongitude-hcellSize/2))/hcellSize)) + 1
    
    # print(paste("row = ", row, "col = ", col))
    # print(Inhabitable[(row-radius):(row+radius),(col-radius):(col+radius)])
    # print(sum(Inhabitable[(row-radius):(row+radius),(col-radius):(col+radius)]))
    
    numCellsPerRegion    <- (2*radius + 1)^2
    newVaccinatedPerCell <- seedData[ff,4]/numCellsPerRegion    #round(seedData[ff,8]/numCellsPerRegion)
    newExpPerCell        <- seedData[ff,5]/numCellsPerRegion    #round(seedData[ff,5]/numCellsPerRegion)
    newInfPerCell        <- seedData[ff,6]/numCellsPerRegion    #round(seedData[ff,4]/numCellsPerRegion)
    newRecoveredPerCell  <- seedData[ff,7]/numCellsPerRegion    #round(seedData[ff,6]/numCellsPerRegion)
    newDeadPerCell       <- seedData[ff,8]/numCellsPerRegion    #round(seedData[ff,7]/numCellsPerRegion)

    Vaccinated[(row-radius):(row+radius),(col-radius):(col+radius)] <- Vaccinated[(row-radius):(row+radius),(col-radius):(col+radius)] + newVaccinatedPerCell
    Exposed[(row-radius):(row+radius),(col-radius):(col+radius)] <- Exposed[(row-radius):(row+radius),(col-radius):(col+radius)] + newExpPerCell
    Infected[(row-radius):(row+radius),(col-radius):(col+radius)] <- Infected[(row-radius):(row+radius),(col-radius):(col+radius)] + newInfPerCell
    Recovered[(row-radius):(row+radius),(col-radius):(col+radius)] <- Recovered[(row-radius):(row+radius),(col-radius):(col+radius)] + newRecoveredPerCell
    Dead[(row-radius):(row+radius),(col-radius):(col+radius)] <- Dead[(row-radius):(row+radius),(col-radius):(col+radius)] + newDeadPerCell

    #print(paste("Susceptible = ", sum(values(Susceptible))))
  }
  
  sumS <- sum(values(Susceptible)); sumV <- sum(values(Vaccinated));
  sumE <- sum(values(Exposed)); sumI <- sum(values(Infected));
  sumR <- sum(values(Recovered)); sumD <- sum(values(Dead))

  propVaccinated <- sumV/sumS
  propExposed <- sumE/sumS
  propInfected <- sumI/sumS
  propRecovered <- sumR/sumS
  propDead <- sumD/sumS
  
  print(paste("Susceptible Count before removing initial seed values: ", sumS))
  
  Susceptible <- Susceptible - (Susceptible*propVaccinated) - (Susceptible*propExposed) - (Susceptible*propInfected) - (Susceptible*propRecovered) - (Susceptible*propDead)

  sumS <- sum(values(Susceptible))

  print(paste("Susceptible Count after removing initial seed values: ", sumS)) #sum(values(Susceptible)<0)
  
  datarow <- 0
  cumVaccinated <- round(sumV)
  cumExposed <- round(sumE)
  cumInfected <- round(sumI)
  cumRecovered <- round(sumR)
  cumDead <- round(sumD)

  #-------------------------------#
  # MAIN LOOP FOR TIME INCREMENTS #
  #-------------------------------#

  allRasters <- vector(mode ="list", length = timestep)
  
  for (t in 1:timestep)
  {						# time increments
    print(paste("time = ", t))
    
    summary[t, 1]  <- toString(as.Date(startDate) + days(t - 1)) # Print the date at each time step
    summary[t, 2]  <- round(sumS + sumV + sumE + sumI + sumR + sumD)
    summary[t, 3]  <- round(sumS)
    summary[t, 4]  <- round(sumV)            # Absorbing state
    summary[t, 5]  <- round(sumE)            # This is the prevalence (active exposed cases) at time t, NOT the cumulative sum
    summary[t, 6]  <- round(sumI)            # This is the prevalence (active infectious cases) at time t, NOT the cumulative sum
    summary[t, 7]  <- round(cumRecovered)    # round(sumR)   # Absorbing state
    summary[t, 8]  <- round(cumDead)         # round(sumD)   # Absorbing state
    
    summary[t, 14]  <- cumExposed
    summary[t, 15]  <- cumInfected
    summary[t, 16]  <- alpha
    summary[t, 17]  <- beta
    summary[t, 18]  <- gamma
    summary[t, 19]  <- sigma
    summary[t, 20]  <- delta
    
    summary[t, 21]  <- radius
    summary[t, 22]  <- lambda
    summary[t, 23]  <- model
    
    nextSusceptible <- nextVaccinated <- nextExposed <- nextInfected <- nextRecovered <- nextDead <- matrix(0, nrows, ncols, byrow = T)
    
    dailyVaccinated <- dailyExposed <- dailyInfected <- dailyRecovered <- dailyDead <- 0

    #-------------------------------#
    # Generating the I_tilda matrix #
    #-------------------------------#

    I_tilda <- wtd_nbrs_sum(input_matrix = raster::as.matrix(Infected), radius = radius, lambda = lambda)
    
    for(i in 1:nrows)
    { 							# nrows
      for(j in 1:ncols)
      {							# ncols
        if (Inhabitable[i,j] == 1)
        {						     # Inhabitable
          nLiving <- newVaccinated <- nearbyInfected <- newExposed <- newInfected <- newRecovered <- newDead <- 0
          
          nLiving <- Susceptible[i,j] + Vaccinated[i,j] + Exposed[i,j] + Infected[i,j] + Recovered[i,j] 
          
          if (nLiving > 0)			# nLiving
          {
            if (Susceptible[i,j] >= 1)
            {
              nearbyInfected <- I_tilda[i,j]
              
              #--------------------------------------------------------------------#
              # Some susceptible people are going to be newly vaccinated           #
              #--------------------------------------------------------------------# 
              
              newVaccinated <- alpha*Susceptible[i,j]
              
              dailyVaccinated <- dailyVaccinated + newVaccinated
              
              #--------------------------------------------------------------------#
              # Some susceptible people who come in contact with nearby infected   #
              # are going to be newly exposed                                      #
              #--------------------------------------------------------------------# 
              
              if (nearbyInfected >= 1)
              {
                pSusceptible <- Susceptible[i,j]/nLiving
                if (deterministic){
                  newExposed <- beta*pSusceptible*nearbyInfected
                } else {
                  rpois(1, beta*pSusceptible*nearbyInfected)
                }
               
                
                dailyExposed <- dailyExposed + newExposed
                cumExposed <- cumExposed + newExposed
              }
            }
            
            #----------------------------------------------------------#
            # Some exposed people are going to become newly infectious #
            #----------------------------------------------------------#
            if (Exposed[i,j] >= 1)
            {
              newInfected <- gamma*Exposed[i,j] 

              dailyInfected <- dailyInfected + newInfected              
              cumInfected   <- cumInfected + newInfected
            }
            
            #-----------------------------------------------------------#
            # Some infectious people are going to either recover or die #
            #-----------------------------------------------------------#
            if (Infected[i,j] >= 1)
            {
              newRecovered <- sigma*Infected[i,j]
              
              dailyRecovered <- dailyRecovered + newRecovered
              cumRecovered <- cumRecovered + newRecovered 
              
              newDead <- delta*Infected[i,j]
              
              dailyDead <- dailyDead + newDead
              cumDead <- cumDead + newDead 
            }
            
            #-----------------------------------#
            # Store the next state of each cell #
            #-----------------------------------#
            
            nextSusceptible[i,j] <- Susceptible[i,j] - newExposed - newVaccinated 
            nextVaccinated[i,j] <- Vaccinated[i,j] + newVaccinated
            nextExposed[i,j] <- Exposed[i,j] + newExposed - newInfected
            nextInfected[i,j] <- Infected[i,j] + newInfected - newDead - newRecovered
            nextRecovered[i,j] <- Recovered[i,j] + newRecovered
            nextDead[i,j] <- Dead[i,j] + newDead
          }					# nLiving
        } 					# Inhabitable
      }							# ncols
    } 							# nrows
    
    nextSusceptible[nextSusceptible < 0] = 0
    nextVaccinated[nextVaccinated < 0] = 0
    nextExposed[nextExposed < 0] = 0
    nextInfected[nextInfected < 0] = 0
    nextRecovered[nextRecovered < 0] = 0
    nextDead[nextDead < 0] = 0
    
    Susceptible <- nextSusceptible
    Vaccinated <- nextVaccinated
    Exposed <- nextExposed
    Infected <- nextInfected
    Recovered <- nextRecovered
    Dead <- nextDead      
    
    rs$rasterStack$Susceptible <- Susceptible
    rs$rasterStack$Vaccinated <- Vaccinated
    rs$rasterStack$Exposed <- Exposed
    rs$rasterStack$Infected <- Infected
    rs$rasterStack$Recovered <- Recovered
    rs$rasterStack$Dead <- Dead
    
    summary[t, 9]   <- dailyVaccinated
    summary[t, 10]  <- dailyExposed
    summary[t, 11]  <- dailyInfected
    summary[t, 12]  <- dailyRecovered
    summary[t, 13]  <- dailyDead

    sumS <- sum(Susceptible); sumV <- sum(Vaccinated);
    sumE <- sum(Exposed); sumI <- sum(Infected);
    sumR <- sum(Recovered); sumD <- sum(Dead)
    
    allRasters[[t]] <- rs;
  }		      	# time increments
  
  # Print a PNG for the infected variable
  rasterLayer <- "Infected"
  
  maxRasterLayerVal <- 0
  
  for (t in 1:timestep){
    maxRasterLayerVal <- max(maxRasterLayerVal, max(maxValue(allRasters[[t]]$rasterStack[[rasterLayer]])))
  }
  
  for (t in 1:timestep){
    fname = paste0("MP4/", inputISO, "_", rasterLayer, "_", sprintf("%04d", t), ".png")
    printStackLayer(rasterStack = allRasters[[t]]$rasterStack, rasterLayer = rasterLayer, directOutput = directOutput, Level1Identifier = rs$Level1Identifier, selectedCountry, rasterAgg = rasterAgg, fname = fname, maxVal = maxRasterLayerVal, includeLabels = T)
    
    fname = paste0("MP4/", "paper/", inputISO, "_", rasterLayer, "_", sprintf("%04d", t), "_paper", ".png")
    printStackLayer(rasterStack = allRasters[[t]]$rasterStack, rasterLayer = rasterLayer, directOutput = directOutput, Level1Identifier = rs$Level1Identifier, selectedCountry, rasterAgg = rasterAgg, fname = fname, maxVal = maxRasterLayerVal, includeLabels = F)
  }
    
  # MERGE THE PNGs TO A GET AN MP4 VIDEO  
  setwd("www/MP4")
  videoDuration <- 8 # in seconds
  av::av_encode_video(list.files(pattern = "*.png"), framerate = timestep/videoDuration, output = paste0(rasterLayer, "_MP4.mp4"))
  setwd("./../..")
  
  summary[is.na(summary)] <- 0
  
  write.xlsx(summary, file = paste0("www/MP4/", inputISO, "_summary.xlsx"), col.names = T, row.names = F, append = F)
  #print(tail(summary))
  
  return(summary)
  
} # End of function

# SpatialCompartmentalModel(model = "SVEIRD", startDate = "2020-06-01", selectedCountry = "Czech Republic", directOutput = FALSE, rasterAgg = 10, alpha = 0.00015, beta = 0.030, gamma = 0.010, sigma = 0.065, delta = 0.002, radius = 1, lambda = 15, timestep = 30, deterministic = T)
# 
# SpatialCompartmentalModel(model = "SVEIRD", startDate = "2020-01-01", selectedCountry = "Nigeria", directOutput = FALSE, rasterAgg = 25, alpha = 0.00015, beta = 0.030, gamma = 0.010, sigma = 0.065, delta = 0.002, radius = 1, lambda = 30, timestep = 25, deterministic = T)

 #-------------#
 # END OF CODE #
 #-------------#
 
 #----------------------------#
 # Set your working directory #
 #----------------------------#
 
 # setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # RStudio IDE preferred
 # getwd() # Path to your working directory
 
 # timestep <- 50 #3650
 # lambda <- 15
 # rasterAgg <- 10
 # radius <- 1 # apply formula as discussed
 # model <- "SVEIRD" #"SEIRD"
 # selectedCountry <- "Czech Republic"
 # t <- 1
 # startDate <- "2021-06-01" #today()
 # deterministic <- T
 # directOutput <- F #T
 
 # #------------#
 # # Parameters #
 # #------------#
 # 
 # alpha <- 0.00015  # Daily fraction that move out of the susceptible compartment to the vaccinated compartment
 # beta  <- 0.030    # Daily fraction that move out of the susceptible compartment to the exposed compartment
 # gamma <- 0.010    # Daily fraction that move out of the exposed compartment to the infectious compartment **** Gamma has to remain the same for all scenarios
 # sigma <- 0.065    # Daily fraction that move out of the infectious compartment to the recovered compartment
 # delta <- 0.002    # Daily fraction that move out of the infectious compartment to the dead compartment
 
 # for (ff in 1:numLocations)
 # {
 #     row <- trunc(abs((data()[ff,2] - (ULCornerLatitude+vcellSize/2))/vcellSize)) + 1
 #     col <- trunc(abs((data()[ff,3] - (ULCornerLongitude-hcellSize/2))/hcellSize)) + 1
 #     
 #     #print(paste("row = ", row, "col = ", col))
 #     #print(Inhabitable[(row-radius):(row+radius),(col-radius):(col+radius)])
 #     #print(sum(Inhabitable[(row-radius):(row+radius),(col-radius):(col+radius)]))
 #     
 #     numCellsPerRegion    <- (2*radius + 1)^2
 #     newVaccinatedPerCell <- data()[ff,4]/numCellsPerRegion    #round(data()[ff,8]/numCellsPerRegion)
 #     newExpPerCell        <- data()[ff,5]/numCellsPerRegion    #round(data()[ff,5]/numCellsPerRegion)
 #     newInfPerCell        <- data()[ff,6]/numCellsPerRegion    #round(data()[ff,4]/numCellsPerRegion)
 #     newRecoveredPerCell  <- data()[ff,7]/numCellsPerRegion    #round(data()[ff,6]/numCellsPerRegion)
 #     newDeadPerCell       <- data()[ff,8]/numCellsPerRegion    #round(data()[ff,7]/numCellsPerRegion)
 #     
 #     Exposed[(row-radius):(row+radius),(col-radius):(col+radius)] <- Exposed[(row-radius):(row+radius),(col-radius):(col+radius)] + newExpPerCell
 #     Infected[(row-radius):(row+radius),(col-radius):(col+radius)] <- Infected[(row-radius):(row+radius),(col-radius):(col+radius)] + newInfPerCell
 #     Recovered[(row-radius):(row+radius),(col-radius):(col+radius)] <- Recovered[(row-radius):(row+radius),(col-radius):(col+radius)] + newRecoveredPerCell
 #     Dead[(row-radius):(row+radius),(col-radius):(col+radius)] <- Dead[(row-radius):(row+radius),(col-radius):(col+radius)] + newDeadPerCell
 #     Vaccinated[(row-radius):(row+radius),(col-radius):(col+radius)] <- Vaccinated[(row-radius):(row+radius),(col-radius):(col+radius)] + newVaccinatedPerCell
 #     
 #     #print(paste("Susceptible = ", sum(values(Susceptible))))
 # }
 