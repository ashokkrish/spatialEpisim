source("R/H_Matrix.R")

generateQHt <- function(HList, varCovarFunc, Qvar, QCorrLength, makeQ = F) {
  
  # HList <- generateLIO(createRasterStack(selectedCountry = "Democratic Republic of Congo", rasterAgg = 10, isCropped = T, level1Names = c("Ituri", "Nord-Kivu"))$rasterStack, sitRepData = "observeddata/Ebola_Health_Zones_LatLon.csv", states_observable = 2)
  # varCovarFunc <- "DBD" # "Balgovind" # 
  # Qvar <- 1
  # QCorrLength <- 0.8 # 1 # 
  # makeQ <- T
  
  rasterStack <- HList$rasterStack
  #print(rasterStack)
  
  nrows <- nrow(rasterStack) 
  ncols <- ncol(rasterStack)
  # print(nrows)
  # print(ncols)
  
  p <- nrows * ncols # Get p from H_Matrix.R
  # print(p)
  
  observableStates <- HList$states_observable
  
  Locations <- HList$Locations # Row/column/position indices are sourced from H_Matrix.R
  # print(Locations)
  
  nHealthZones <- as.numeric(dim(Locations)[1]) 
  
  QHt <- Q0 <- matrix(0, p, nHealthZones) # Pre-allocating QHt
  Q <- matrix(0, p, p) # Pre-allocating Q
  
  for (n in 1:nHealthZones){
    HZRow <- Locations[n, 4]
    HZCol <- Locations[n, 5]
    
    for (i in 1:nrows){
      for (j in 1:ncols){
        pos <- ((i-1)*ncols)+j
        d <- sqrt((HZRow - i)^2 + (HZCol - j)^2) # Finding Euclidean distance
        
        # Generating entries of QHt based on selected variance-covariance function
        if (varCovarFunc == "DBD"){
          val <- QCorrLength^d
        } else if (varCovarFunc == "Balgovind"){
          val <- (1 + (d/QCorrLength))*exp(-d/QCorrLength)
        } else if (varCovarFunc == "Exponential"){
          val <- exp(-d/QCorrLength)
        } else if (varCovarFunc == "Gaussian"){
          val <- exp(-(d^2)/2*(QCorrLength^2))
        } else if (varCovarFunc == "Spherical") {
          # Note that "QCorrLength" actually refers to the radius for the
          # spherical variance-covariance function
          if (d < QCorrLength) {
            val <- (3*d)/(2*QCorrLength) - (d^3)/(2*(QCorrLength^3))
          } else {
            val <- 0
          }
        } else {
          stop('Invalid variance-covariance function selected. Currently supported functions are: "DBD", "Balgovind", "Exponential", "Gaussian" and "Spherical"') 
          # Error if selected variance-covariance function is invalid
        }
        
        QHt[pos, n] <- Qvar*val
      }
    }
  }
  
  #print(dim(QHt))

  # Block diagonalization if there are multiple observable states
  QHtFull <- QHt
  
  # print(dim(Q0))
  # print(dim(QHt))
  # print(dim(QHtFull))
  # print(head(QHtFull))
  
  if (observableStates > 1){
    for (n in seq(from = 1, to = observableStates-1, by = 1)){
      QHtFull <- rbind(QHtFull, Q0)
    }
    
    # print(fivenum(QHtFull))
    # print(table(QHtFull))
    
    for (n in seq(from = 1, to = observableStates-1, by = 1)){
      Qtop <- matrix(0, n*p, nHealthZones)
      if ((n+1 - observableStates) !=  0){
        Qbottom <- matrix(0, (observableStates-n-1)*p, nHealthZones)
        print(dim(Qtop))
        print(dim(QHt))
        print(dim(Qbottom))
        QHtFull <- cbind(QHtFull, rbind(Qtop, QHt, Qbottom))
      }
      else {
        # print(dim(QHt))
        # print(dim(Qtop))
        QHtFull <- cbind(QHtFull, rbind(Qtop, QHt))
      }
    }
  }
  
  # print(dim(QHtFull))  
  # print(QHtFull[1:5, 1:5])
  # print(dim(HList$Hmat))

  # Generating the full Q matrix if desired. Warning: doing this increases the computation time significantly
  
  if (makeQ == T) {
    
    alpha <- matrix(rep(1:p, p), nrow = p, ncol = p)
    JJ <- (alpha - 1) %% nrows + 1
    II <- floor((alpha - JJ) / ncols) + 1
    LL <- t(JJ)
    KK <- t(II)
    d <- sqrt((LL - JJ)^2 + (KK - II)^2)
    
    # Generating entries of Q based on selected variance-covariance function
    if (varCovarFunc == "DBD"){
      val <- QCorrLength^d
    } else if (varCovarFunc == "Balgovind"){
      val <- (1 + (d/QCorrLength))*exp(-d/QCorrLength)
    } else if (varCovarFunc == "Exponential"){
      val <- exp(-d/QCorrLength)
    } else if (varCovarFunc == "Gaussian"){
      val <- exp(-(d^2)/2*(QCorrLength^2))
    } else if (varCovarFunc == "Spherical") {
      # Note that "QCorrLength" actually refers to the radius for the
      # spherical variance-covariance function
      if (d < QCorrLength) {
        val <- (3*d)/(2*QCorrLength) - (d^3)/(2*(QCorrLength^3))
      } else {
        val <- 0
      }
    } else {
      stop('Invalid variance-covariance function selected. Currently supported functions are: DBD, Balgovind, Exponential, Gaussian and Spherical')
      # Error if selected variance-covariance function is invalid
    }
    
    Q <- Qvar*val
    
    # print(dim(Q))
    # print(Q[1:5, 1:5])
    # print(varCovarFunc)
    #
    # print(det(Q))
    
    return(list("QHt" = QHtFull, "Q" = Q))
  } else {  
    return(list("QHt" = QHtFull))
    }
}

#--------------#
# Example Call #
#--------------#

#HList <- generateLIO(createRasterStack(selectedCountry = "Democratic Republic of Congo", rasterAgg = 10, isCropped = T, level1Names = c("Ituri", "Nord-Kivu"))$rasterStack, sitRepData = "observeddata/Ebola_Health_Zones_LatLon.csv", states_observable = 2)

#generateQHt(HList, varCovarFunc = "DBD", Qvar = 1, QCorrLength = 0.8, makeQ = F)
#
# generateQHt(HList, varCovarFunc = "DBD", Qvar = 1, QCorrLength = 0.8, makeQ = T)
#
# generateQHt(HList, varCovarFunc = "Balgovind", Qvar = 1, QCorrLength = 0.8, makeQ = F)
# 
# generateQHt(HList, varCovarFunc = "Exponential", Qvar = 1, QCorrLength = 0.8, makeQ = F)
# 
# generateQHt(HList, varCovarFunc = "Gaussian", Qvar = 1, QCorrLength = 0.8, makeQ = F)
