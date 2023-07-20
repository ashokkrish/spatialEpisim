source('R/H_Matrix.R')

generateQHt <- function(HList, varCovarFunc, Qvar, QCorrLength, makeQ){
  
  rasterStack <- HList$rasterStack
  #print(rasterStack)
  
  nrows <- nrow(rasterStack) #Getting p from H_Matrix.R
  ncols <- ncol(rasterStack)
  # print(nrows)
  # print(ncols)
  
  p = nrows * ncols
  #print(p)
  
  observableStates <- HList$states_observable
  
  Locations <- HList$Locations
  nHealthZones <- as.numeric(dim(Locations)[1]) #Getting row/column/position indices from H_Matrix.R
  
  
  QHt <- Q0<- matrix(0,p,nHealthZones) #Pre-allocating Q
  
  for (n in 1:nHealthZones){
    HZRow <- Locations[n,4]
    HZCol <- Locations[n,5]
    for (i in 1:nrows){
      for (j in 1:ncols){
        pos <- ((i-1)*ncols)+j
        d <- sqrt((HZRow - i)^2 + (HZCol - j)^2) #Finding Euclidean distance
        if (varCovarFunc == 'DBD'){
          val <- QCorrLength^d
        }
        else if (varCovarFunc == 'Balgovind'){
          val <- (1+d/(QCorrLength))*exp(-d/QCorrLength) #Generating entries of Q based on selected variance-covariance function
        }
        else if (varCovarFunc == 'Exponential'){
          val <- exp(-d/QCorrLength)
        }
        else if (varCovarFunc == 'Gaussian'){
          val <- exp(-(d^2)/2*(QCorrLength^2))
        }
        else {
          stop('Invalid variance-covariance function selected. Currently supported functions are: "DBD", "Balgovind", "Exponential" and "Gaussian".') 
          #Error if selected variance-covariance function is invalid
        }
        QHt[pos,n] <- Qvar*val
      }
    }
  }
  
  #Block diagonalization if there are multiple observable states
  QFull <- QHt
  if (observableStates > 1){ 
    for (n in seq(from = 1, to = observableStates-1, by = 1)){  
      QFull <- rbind(QFull,Q0)
    } 
    for (n in seq(from = 1, to = observableStates-1, by = 1)){
      Qtop <- matrix(0, n*p, nHealthZones)
      if ((n+1 - observableStates) !=  0){
        Qbottom <- matrix(0, (observableStates-n-1)*p, nHealthZones)
         print(dim(Qtop))
         print(dim(QHt))
         print(dim(Qbottom))
        QFull <- cbind(QFull, rbind(Qtop, QHt, Qbottom))
      }
      else {
        # print(dim(QHt))
        # print(dim(Qtop))
        QFull<- cbind(QFull, rbind(Qtop, QHt))
      }
    }
  }
  #print(Locations)
  #Generating the full Q matrix if desired. Warning: doing this increases the computation time significantly
  if (makeQ == T) {
    Q <- matrix(0,p,p)
    for (rowa in 0:nrows-1){
      for (cola in 0:ncols-1){
        a <- rowa * ncols + cola
        for (rowb in 0:nrows-1){
          for (colb in 0:ncols-1){
            b = rowb * ncols + colb
            d = sqrt((rowa - rowb)^2 + (cola - colb)^2) #Finding Euclidean distance
            if (varCovarFunc == 'DBD'){
              val <- QCorrLength^d
            }
            else if (varCovarFunc == 'Balgovind'){
              val <- (1+d/(QCorrLength))*exp(-d/QCorrLength) #Generating entries of Q based on selected variance-covariance function
            }
            else if (varCovarFunc == 'Exponential'){
              val <- exp(-d/QCorrLength)
            }
            else if (varCovarFunc == 'Gaussian'){
              val <- exp(-(d^2)/2*(QCorrLength^2))
            }
            else {
              stop('Invalid variance-covariance function selected. Currently supported functions are: "DBD", "Balgovind", "Exponential" and "Gaussian".') 
            #Error if selected variance-covariance function is invalid
            }
            Q[a+1, b+1] <- Qvar*val
          }
        }
      }
    }
    return(list("QHt" = QFull, "Q" = Q))
  }
  else{
    return(QFull)
  }
}

#Example Call:
# test <- generateQHt(generateLIO(createRasterStack("Democratic Republic of Congo", rasterAgg=10, isCropped=T, c("Ituri", "Nord-Kivu"))$rasterStack, "observeddata/Ebola_Health_Zones_LatLon.csv", states_observable = 3), "Exponential", Qvar = 1, QCorrLength = 0.8, makeQ = F)	
# print(test)