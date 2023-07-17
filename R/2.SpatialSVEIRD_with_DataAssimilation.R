##########################################################################
#                                                                        #
# Spatial tracking of the ongoing Ebola outbreak in Congo                #
#                                                                        #
# This source code is issued under the GNU General Public License, v3.0. #
#                                                                        #
# This script is free software; you can redistribute it and/or modify    #
# it under the terms of the GNU General Public License as published by   #
# the Free Software Foundation; either version 3.0 of the License, or    #
# (at your option) any later version.                                    #
#                                                                        #
# See the GNU General Public License for more details.                   #
#                                                                        #
# https://www.gnu.org/licenses/gpl-3.0.en.html                           #
##########################################################################

rm(list = ls())

library(ncdf4)
library(raster)
library(lattice)
library(writexl) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

baseDir <- getwd()
outputDir <- paste(baseDir, "/Output", sep="") # The directory for output files
if (!(file.exists(outputDir))){
  dir.create("Output") # Folder to store output .nc files
}
#setwd(outputDir) # Change working directory to output folder

#source('1.ClippingProvinces_DRC.R')

avg_euclidean_distance <- function(p, q, spaceKm)
{
  exp(-sqrt(sum((p - q)^2))/spaceKm)
}

wtd_nbrs_sum <- function(input_matrix, radius, spaceKm)
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
      weight_matrix[i,j] <- avg_euclidean_distance(c(i,j), c(radius+1, radius+1), spaceKm)
    }
  }
  
  #print(weight_matrix)
  
  for(i in seq_len(length.out = nrow(x = input_matrix)))
  {
    for(j in seq_len(length.out = ncol(x = input_matrix))) # this was nrow previously
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
 
#         last = 440
#         radius = 1
#         spaceKm = 10

SpatialSVEIRD <- function(last = 730, radius = 1, spaceKm = 10, mergeNCDF = T, DA = F)
 {
  #setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  summary <- array(0, c(last, 16), dimnames = list(c(1:last), c("| t |", "| N |", "| S |", "| V |", "| E |", "| I |", "| R |", "| D |", "| cumE |", "| newI |", "| cumI |", "| Alpha |", "| Beta |", "| Gamma |", "| Sigma |", "| Delta |")))
  
  # , "| newD |","| cumD |", "| Ituri |","| North-Kivu |"
  
  episim <- nc_open("Congo_0000.nc", write = TRUE)
  
  #print(episim)
  
  episim$dim$latitude$vals;  length(episim$dim$latitude$vals)     # lat is vertical axis  (or) 47 rows in our case
  episim$dim$longitude$vals; length(episim$dim$longitude$vals)		# lon is horizontal axis (or) 58 columns in our case
  
  nrows <- ncatt_get(episim, 0, "nRows")$value
  ncols <- ncatt_get(episim, 0, "nCols")$value
  
  ULCornerLongitude <- ncatt_get(episim, 0, "ULCornerLongitude")$value
  ULCornerLatitude <- ncatt_get(episim, 0, "ULCornerLatitude")$value
   
  # LLCornerLongitude <- ncatt_get(episim, 0, "LLCornerLongitude")$value
  # LLCornerLatitude <- ncatt_get(episim, 0, "LLCornerLatitude")$value
  
  hcellSize <- ncatt_get(episim, 0, "hcellSize")$value
  vcellSize <- ncatt_get(episim, 0, "vcellSize")$value
  
  # IMPORTANT: Take the transpose when importing a state variable from a ncdf file because
  # the raster is rotated counterclockwise 90 degrees
  
  S <- t(ncvar_get(episim, episim$var[[2]]))
  V <- t(ncvar_get(episim, episim$var[[3]]))
  E <- t(ncvar_get(episim, episim$var[[4]]))
  I <- t(ncvar_get(episim, episim$var[[5]]))
  R <- t(ncvar_get(episim, episim$var[[6]]))
  D <- t(ncvar_get(episim, episim$var[[7]]))
  currInhabitable <- t(ncvar_get(episim, episim$var[[8]]))
  Level1Identifier <- t(ncvar_get(episim, episim$var[[9]]))
  
  dim(S); dim(E); dim(I); dim(R); dim(D); dim(currInhabitable); dim(Level1Identifier)
  
  table(Level1Identifier)

  #-------------------------#
  # Avoid "magic constants" #
  #-------------------------#
  
  s <- 3                           # Number of transient states (S, E, I)
  p <- ncols*nrows                 # Dimensionality of the state space
  nDaysPerUnit <- 7
  q <- 27                          # Number of Health Zones
  
  #----------------------------#
  # Initial outbreak locations #
  #----------------------------#
  
  outbreak <- read.csv("cities-lat-lon.csv", header = T)
  
  print(paste("Focus of initial infection is at four rural areas in DRC: Beni, Butembo and Mabalako (in North-Kivu province) and Mandima (in Ituri province)."))
  
  numLocations <- dim(outbreak)[1]
  
  # neighborhood <- 1 # 1 is called Moore neighbourhood
  # citylimits <- (2*neighborhood + 1)^2 # citylimits a block of grid cells.
  ##For ex: At the 2.5 arc minute resolution neighborhood of 2 means citylimits consists of 25 grids
  # 5 rows and 5 columns, 25 square kilometers
  # corners <- 0#4
  
  for (ff in 1:numLocations)
  {
    # row <- abs(round((outbreak[ff,2] - yLLCorner)/vcellSize))
    # col <- abs(round((outbreak[ff,3] - xLLCorner)/hcellSize))
    
    # When raster::cellFromXY function was used to translate the city lat/lng to matrix row, col values
    # they did not line up the row/col values calculated in the above two lines. To reproduce the row/col values
    # calculated by the raster package function, the row/col calculations should be:
    
    # row/col starts counting at 1, not zero
    # ncdf4 provides the lat/lng for the center of each cell, We need the lat/lng for the corner
    
    # row <- trunc(abs((outbreak[ff,2] - (yLLCorner+vcellSize/2))/vcellSize)) + 1
    # col <- trunc(abs((outbreak[ff,3] - (xLLCorner-hcellSize/2))/hcellSize)) + 1
    
    row <- trunc(abs((outbreak[ff,2] - (ULCornerLatitude+vcellSize/2))/vcellSize)) + 1
    col <- trunc(abs((outbreak[ff,3] - (ULCornerLongitude-hcellSize/2))/hcellSize)) + 1
    
    #print(paste("row = ", row, "col = ", col))
    
    newInf <- outbreak[ff, 4]
    newExp <- outbreak[ff, 5]
    newDead <- outbreak[ff, 6]
    
    #print(currInhabitable[(row-neighborhood):(row+neighborhood), (col-neighborhood):(col+neighborhood)])
    
    #print(c(S[row,col], E[row,col], I[row,col], D[row,col]))
    
    if (currInhabitable[row, col] == 1 & S[row, col] > newInf)
    {
      S[row,col] <- S[row,col] - newExp - newInf - newDead
      E[row,col] <- E[row,col] + newExp
      I[row,col] <- I[row,col] + newInf
      D[row,col] <- D[row,col] + newDead
    }

    #print(c(S[row,col], E[row,col], I[row,col], D[row,col]))
  }
  
  episimDay0 <- nc_create(paste("Congo_Day0.nc", collapse="", sep=""), episim$var)

  ncvar_put(episimDay0, episimDay0$var[[2]], t(S))
  ncvar_put(episimDay0, episimDay0$var[[3]], t(V))
  ncvar_put(episimDay0, episimDay0$var[[4]], t(E))
  ncvar_put(episimDay0, episimDay0$var[[5]], t(I))
  ncvar_put(episimDay0, episimDay0$var[[6]], t(R))
  ncvar_put(episimDay0, episimDay0$var[[7]], t(D))
  
  ncvar_put(episimDay0, episimDay0$var[[8]], t(currInhabitable))
  ncvar_put(episimDay0, episimDay0$var[[9]], t(Level1Identifier))
  nc_close(episimDay0)
  
  #-------------------------------#
  # Generating the I_tilda matrix #
  #-------------------------------#
  
  # radius <- 1
  # spaceKm <- 1
  # 
  # I_tilda <- wtd_nbrs_sum(input_matrix = I, radius = radius, spaceKm = spaceKm)

  #------------#
  # Parameters #
  #------------#
  
  # The incubation period, that is, the time interval from infection with the Ebola virus to onset of symptoms, is from 2 to 21 days. On average the incubation period is about 9 days.
  
  # The infectious period, that is, the time during which an infected person can infect others with the Ebola virus, is from 5 to 7 days. On average the infectious period is about 6 days.
  
  # DON'T CHANGE THESE NUMBERS: 0.2100, 0.0055, 0.0060, 0.0090, 0.0210, radius = 2, km = 10
  
  alpha <- 0.2100  # Daily fraction that move out of the susceptible compartment to the vaccinated compartment
  beta  <- 0.0055  # Daily fraction that move out of the susceptible compartment to the exposed compartment
  gamma <- 0.0055  # Daily fraction that move out of the exposed compartment to the infectious compartment
  sigma <- 0.0100  # Daily fraction that move out of the infectious compartment to the recovered compartment
  delta <- 0.0210  # Daily fraction that move out of the infectious compartment to the dead compartment
  
  # Beta calculation  1 - (1 - (1/9))^(1/7)   # nDaysPerUnit <- 7
  # Sigma calculation 1 - (1 - (1/5.6))^(1/7) # nDaysPerUnit <- 7(1/nDaysPerUnit)
  # (1/5.6)*(1/7) # where 5.6 days is the average infectious period
  # (1/9)*(1/7)   # where 9 days is the average incubation period
  
  # Lambda is the expected maximum distance (in km) traveled per day per individual.
  # The expected maximum distance from home achieved in any day.
  
  # Radius is how many lambda units you want to go out in the simulation.
  # Moore neighborhood depends on the cell size, it has nothing to do with the radius.
  
  datarow <- 0
  cumExposed <- round(sum(E))
  cumIncidence <- round(sum(I))
  cumDead <- round(sum(D))

  ################# DA Begins ##################
  
  if (DA == T)
  {
      #-------------------------------------------#
      # Import the Ebola Incidence and Death Data #
      #-------------------------------------------#
  
      incidence_data <- read.csv("Ebola_Incidence_Data_SitRpt54.csv", header = T)
      death_data <- read.csv("Ebola_Death_Data_SitRpt54.csv", header = T)
  
      #dim(incidence_data); dim(death_data)
  
      # ## read in Q matrix code
      #source('Q_matrix_ver2.R')
      source('Q_matrix_ver1.R')
  
      # ## read in H matrix code
      source('H_matrix.R')
  
      QHt  <- Qf.OSI%*%t(Hmat) # Calculate this only once
      HQHt <- Hmat%*%QHt       # Calculate this only once
      
      #'%!in%' <- function(x,y)!('%in%'(x,y))
  }
  
  ################# DA Ends ##################
  
  #-------------------------------#
  # MAIN LOOP FOR TIME INCREMENTS #
  #-------------------------------#

  for (t in 1:last)
  {						# time increments
    print(paste("time = ", t))

    summary[t, 1]  <- t
    summary[t, 2]  <- round(sum(S) + sum(V) + sum(E) + sum(I) + sum(R) + sum(D))
    summary[t, 3]  <- round(sum(S)) 
    summary[t, 4]  <- round(sum(V)) # Absorbing state
    summary[t, 5]  <- round(sum(E)) # This is the prevalence at time t, NOT the cumulative sum
    summary[t, 6]  <- round(sum(I)) # This is the prevalence (active cases) at time t, NOT the cumulative sum
    summary[t, 7]  <- round(sum(R)) # Absorbing state
    summary[t, 8]  <- round(sum(D)) # Absorbing state

    summary[t, 12]  <- alpha
    summary[t, 13]  <- beta
    summary[t, 14]  <- gamma
    summary[t, 15]  <- sigma
    summary[t, 16]  <- delta

    nextSusceptible <- nextVaccinated <- nextExposed <- nextInfected <- nextRecovered <- nextDead <- matrix(0, nrows, ncols, byrow = T)
    
    dailyIncidence <- 0
    deadIncidence <- 0
    
    #-------------------------------#
    # Generating the I_tilda matrix #
    #-------------------------------#

    # radius = 2; spaceKm = 1
    
    I_tilda <- wtd_nbrs_sum(input_matrix = I, radius = radius, spaceKm = spaceKm)

    for(i in 1:nrows)
    { 								# nrows
      for(j in 1:ncols)
      {							# ncols
        if (currInhabitable[i,j] == 1)
        {						# Inhabitable
          nLiving <- newVaccinated <- nearbyInfected <- newExposed <- newInfected <- newRecovered <- newDead <- 0
          
          nLiving <- S[i,j] + V[i,j] + E[i,j] + I[i,j] + R[i,j]
          
          if (nLiving > 0)			# nLiving
          {
            if (S[i,j] >= 1)
            {
              #------------------------------------------------------------------#
              # Using the weight matrix we get the nearbyInfected for any radius #
              #------------------------------------------------------------------#
              
              nearbyInfected <- I_tilda[i,j]
              
              #--------------------------------------------------------------------#
              # Some susceptible people who come in contact with nearby infected   #
              # are going to be newly vaccinated or newly exposed                  #
              #--------------------------------------------------------------------# 

              if (nearbyInfected >= 1)
              {
                pSusceptible <- S[i,j]/nLiving
                newVaccinated <- alpha*pSusceptible*nearbyInfected
                newExposed <- beta*pSusceptible*nearbyInfected #rpois(1, beta*pSusceptible*nearbyInfected)
                
                if (newVaccinated + newExposed > S[i,j])
                {
                  newExposed <- S[i,j]
                  newVaccinated <- 0
                }
                cumExposed <- cumExposed + newExposed
              }
            }
            
            #----------------------------------------------------------#
            # Some exposed people are going to become newly infectious #
            #----------------------------------------------------------#
            if (E[i,j] >= 1)
            {
              newInfected <- gamma*E[i,j]
              
              cumIncidence   <- cumIncidence + newInfected
              dailyIncidence <- dailyIncidence + newInfected
            }
            
            #-----------------------------------------------------------#
            # Some infectious people are going to either recover or die #
            #-----------------------------------------------------------#
            if (I[i,j] >= 1)
            {
              newRecovered <- sigma*I[i,j]
              newDead <- delta*I[i,j]
              
              deadIncidence <- deadIncidence + newDead
              
              cumDead <- cumDead + newDead # new cumulative dead column 
            }
            
            #----------------------------------#
            # Store the next state of the cell #
            #----------------------------------#
            
            nextSusceptible[i,j] <- S[i,j] - newExposed - newVaccinated 
            nextVaccinated[i,j] <- V[i,j] + newVaccinated
            nextExposed[i,j] <- E[i,j] + newExposed - newInfected
            nextInfected[i,j] <- I[i,j] + newInfected - newDead - newRecovered
            nextRecovered[i,j] <- R[i,j] + newRecovered
            nextDead[i,j] <- D[i,j] + newDead
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
    
    S <- nextSusceptible
    V <- nextVaccinated
    E <- nextExposed
    I <- nextInfected
    R <- nextRecovered
    D <- nextDead      
    
    summary[t, 9]   <- cumExposed
    summary[t, 10]  <- dailyIncidence
    summary[t, 11]  <- cumIncidence
    
    ########## DA Begins ##########
    
    setwd(outputDir) # Change working directory to output folder
    
    if (DA == T)
    {                     # DA T/F
      NewoutputDir <- paste(outputDir, "/DA", sep="") # The directory for output files
      if (!(file.exists(NewoutputDir))){
        dir.create("DA") # Folder to store output .nc files
      }
      setwd(NewoutputDir) # Change working directory to output folder
      
      if (t %% 7 == 0)
      {                   # elapsed week
        datarow <- datarow + 1
        
         if (datarow < 55)
         {                 # datarow cap
            #-----------------------------------------------#
            # Write forecast (prior) state to a NetCDF file #
            #-----------------------------------------------#
        
            #print(paste("Xf is printed on day", t))
        
            #---------------------#
            # OSI: forecast state #
            #---------------------#
        
            # We track "Infectious" and "Dead" epidemic states only
        
            Xf.OSI <- rbind(cbind(as.vector(t(I))), cbind(as.vector(t(D))))
        
            #print(paste("Dimension of the state vector:")); print(dim(Xf.OSI))
        
            #print(sum(Xf.OSI))
            #table(Xf.OSI)
        
            HXf <- Hmat%*%Xf.OSI
            #print(dim(HXf))
            #print(sum(HXf))
        
            #----------------------------------------------#
            # Importing DRC Ebola Incidence and Death Data #
            #----------------------------------------------#

            incidence <- as.vector(incidence_data[datarow, 3:29]) # Pick a row every 7 days, select third column through to the last column
            death <- as.vector(death_data[datarow, 3:29])         # Pick a row every 7 days, select third column through to the last column

            # if (datarow > 1)
            # {                 # datarow > 1
            #    prevWHOIncidence <- sum(as.vector(incidence_data[1:(datarow-1), 3:29]))
            #    currWHOIncidence <- sum(as.vector(incidence_data[1:datarow, 3:29]))
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
            
            Dvector <- rbind(t(incidence), t(death))
        
            #print(dim(Dvector))
            sum(incidence)
            sum(death)
        
            #-------------------------------------#
            # Measurement error covariance matrix #
            #-------------------------------------#
        
            sum(Dvector < 1)
            Dvector_revised <- ifelse(Dvector < 1, 1, Dvector) # If a diagonal entry is zero change it to 1.
            sum(Dvector_revised < 1)
        
            M <- diag(as.vector(Dvector_revised)) # check if D vector needs to be really revised
        
            #levelplot(M, col.regions= colorRampPalette(c("white", "red", "blue")))
            # table(M)
            # diag(M)
            # det(M)
            #print(M)
        
            #---------------------#
            # Optimal Kalman Gain #
            #---------------------#
        
            # QHt  <- Qf.OSI%*%t(Hmat) # Calculate this only once
            # HQHt <- Hmat%*%QHt       # Calculate this only once
        
            # sum(QHt < 0)
            # sum(HQHt < 0)
         
            # dim(HQHt)
        
            # levelplot(as.matrix(HQHt), col.regions= colorRampPalette(c("white", "red", "blue")))
        
            # diag(HQHt)
            # det(HQHt)
            # eigen(HQHt)$values # HQHt is positive definite since all of its eigenvalues are strictly positive.
            # sum(eigen(HQHt)$values)
            # 
            # log10(max(eigen(HQHt)$values)/min(eigen(HQHt)$values))
            # 
            # det(solve(HQHt))
            # eigen(solve(HQHt))$values # Inverse of HQHt is also positive definite since all of its eigenvalues are strictly positive.
            # sum(eigen(solve(HQHt))$values)
        
            # The gain matrix, Ke.OSI, determines how the observational data are to be assimilated
        
            Ke.OSI <- QHt%*%solve(HQHt + M)  #solve((HQHt + M), t(QHt))
        
            #print(paste("Dimension of the Kalman Gain Matrix:")); print(dim(Ke.OSI))

            # Questions
            # Can the Kalman gain matrix have negative values?
            # Can the innovation or measurement residual have negative values?
        
            #------------------------------------#
            # Innovation or measurement residual #
            #------------------------------------#
        
            Y <- Dvector - HXf
        
            #---------------------------------#
            # OSI update step: analysis state #
            #---------------------------------#
        
            Xa.OSI <- Xf.OSI + Ke.OSI%*%Y
            
            #Xa.OSI[Xa.OSI < 0] <- 0 # This will set all negative values to zero
            
            #Xa.OSI <- abs(Xf.OSI + Ke.OSI%*%Y)
            
            # max(Ke.OSI%*%Y)
            # min(Ke.OSI%*%Y)
            # 
            # sum(round(Ke.OSI%*%Y))
            # 
            # print(sum(Xa.OSI))
            # print(tail(sort(Xa.OSI), 30))
            # print(sum(Xf.OSI))
        
            ###########################
        
            # sum(Xf.OSI < 0)         # Number of negative values in Xf.OSI.
            # 
            # sum(QHt < 0)            # Number of negative values in QHt.
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
            # print(dim(Dvector))
            # sum(incidence)
            # sum(death)
            # 
            # HXa <- Hmat%*%Xa.OSI
            # print(dim(HXa))
            # print(sum(HXa))
        
            # 42.10018 with Q_matrix_ver1
            # 43.00399 with Q_matrix_ver2
        
            #cbind(HXf, round(HXa), HXa)

            #print(cbind(Dvector, Hmat%*%Xf.OSI, Y, round(Hmat%*%Xa.OSI)))
            
            # NOTE: when restacking make sure byrow = T.
            
            I <- matrix(Xa.OSI[1:p], nrow = nrows, ncol = ncols, byrow = T)
            
            I[I < 0] <- 0 # Prevent negative values for the number of infectious
            
            D <- matrix(Xa.OSI[(p+1):(2*p)], nrow = nrows, ncol = ncols, byrow = T)
            
            D[D < 0] <- 0 # Prevent negative values for the number of dead
            
            dim(Xa.OSI); dim(I); dim(D); min(I); min(D); max(I); max(D)
            
            # For all uninhabitable cells set the number of infected and dead = 0. THIS IS VERY CRITICAL!!!
            
            for(i in 1:nrows)
            { 								# nrows
              for(j in 1:ncols)
              {							  # ncols
                if (currInhabitable[i,j] == 0)
                {						  # Inhabitable
                  I[i,j] <- D[i,j] <- 0
                }
              }
            }
          }                  # datarow cap
        }                   # elapsed week
    }   # DA T/F
    ########## DA Ends ##########
    
    else
      {
        NewoutputDir <- paste(outputDir, "/No_DA", sep="") # The directory for output files
        if (!(file.exists(NewoutputDir))){
          dir.create("No_DA") # Folder to store output .nc files
        }
        setwd(NewoutputDir) # Change working directory to output folder
      }
    
    #--------------------------------------#
    # Write current state to a NetCDF file #
    #--------------------------------------#
    
    if (mergeNCDF == T)
    {
     # if (t %% 10 == 0)
     # {
      #if (t < 10)
      #print(paste("Epidemic state saved to a netCDF file on day", t))

      episimNew <- nc_create(paste("Congo_", t, ".nc", collapse="", sep=""), episim$var)

      ncvar_put(episimNew, episimNew$var[[2]], t(S))
      ncvar_put(episimNew, episimNew$var[[3]], t(V))
      ncvar_put(episimNew, episimNew$var[[4]], t(E))
      ncvar_put(episimNew, episimNew$var[[5]], t(I))
      ncvar_put(episimNew, episimNew$var[[6]], t(R))
      ncvar_put(episimNew, episimNew$var[[7]], t(D))
      
      ncvar_put(episimNew, episimNew$var[[8]], t(currInhabitable))
      ncvar_put(episimNew, episimNew$var[[9]], t(Level1Identifier))

      nc_close(episimNew)
     #}
    }
              
  }		      	# time increments

  nc_close(episim)

  if (DA == F)
  {
    write.table(summary, file="summary_without_DA.csv", row.names=F, sep=",", col.names=T, append=F)
    #write_xlsx(summary,"summary.xlsx") # col.names = TRUE, row.names = FALSE
  }
  else
  {
    write.table(summary, file="summary_with_DA.csv", row.names=F, sep=",", col.names=T, append=F)
  }

  print(tail(summary[,1:11]))

  setwd(baseDir)
  
  if (mergeNCDF == T)
  {
  setwd(paste(getwd(), "/Output/No_DA", sep="")) # Change working directory to output folder
  
  var_names <- c('Infected')
  
  numFiles <- last
  
  x <- raster('Congo_1.nc', varname = var_names)
  
  for (k in 1:numFiles)
  {
    #print(k)
    x <- stack(x, raster(paste("Congo_", k, ".nc", collapse="", sep=""), varname = var_names))
  }
  
  writeRaster(x = x, filename = paste0(var_names, '_merged.nc'), overwrite = TRUE, format = 'CDF')
  
  Infected_merged <- nc_open(paste0(var_names, '_merged.nc'), write=TRUE)
  old_varname <- 'variable'
  new_varname <- 'ActiveCases'
  Infected_merged <- ncvar_rename(Infected_merged, old_varname, new_varname)
  
  nc_close(Infected_merged)
  }
  
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} # End of function

# SpatialSVEIRD(last = 440, radius = 1, spaceKm = 10, mergeNCDF = F, DA = F)
# 
# SpatialSVEIRD(last = 440, radius = 1, spaceKm = 20, mergeNCDF = F, DA = F)
#  
# SpatialSVEIRD(last = 440, radius = 1, spaceKm = 30, mergeNCDF = F, DA = F)
#
# SpatialSVEIRD(last = 440, radius = 2, spaceKm = 10, mergeNCDF = T, DA = T)
#
# SpatialSVEIRD(last = 440, radius = 2, spaceKm = 20, mergeNCDF = F, DA = F)
# 
# SpatialSVEIRD(last = 440, radius = 2, spaceKm = 30, mergeNCDF = F, DA = F)