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
  QFull <- QHt
  
  # print(dim(Q0))
  # print(dim(QHt))
  # print(dim(QFull))
  # print(head(QFull))
  
  if (observableStates > 1){
    for (n in seq(from = 1, to = observableStates-1, by = 1)){
      QFull <- rbind(QFull, Q0)
    }
    
    # print(fivenum(QFull))
    # print(table(QFull))
    
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
        QFull <- cbind(QFull, rbind(Qtop, QHt))
      }
    }
  }
  
  # print(dim(QFull))
  # print(dim(HList$Hmat))
  
  # print(QFull[1:5, 1:5])
  
  # QHtH <- QFull%*%HList$Hmat
  # 
  # print(dim(QHtH))
  # print(QHtH[1:5, 1:5])

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
    
    #print(dim(Q))
    
    # print(varCovarFunc)
    # 
    # print(Q[1:5, 1:5])
    
    #print(det(Q))

    # x <- 1:ncol(Q)
    # y <- 1:nrow(Q)
    
    # # Create a grid of x and y values
    # X <- matrix(x, nrow = nrow(Q), ncol = ncol(Q), byrow = TRUE)
    # Y <- matrix(y, nrow = nrow(Q), ncol = ncol(Q), byrow = FALSE)
    
    # Plot the 3D surface
    # library(rgl)
    # library(plot3D)
    # persp3D(X, Y, Q, scale = FALSE, theta = 90, expand = 0.2)
    # persp3D(X, Y, Q, col = "lightpink", xlab = "Columns", ylab = "Rows", zlab = "Values", 
            #facets = FALSE, main = "3D Surface Plot of Q")
  }
  
  return(list("QHt" = QFull, "Q" = Q))
}

#--------------#
# Example Call #
#--------------#

# HList <- generateLIO(createRasterStack(selectedCountry = "Democratic Republic of Congo", rasterAgg = 10, isCropped = T, level1Names = c("Ituri", "Nord-Kivu"))$rasterStack, sitRepData = "observeddata/Ebola_Health_Zones_LatLon.csv", states_observable = 2)
# 
# generateQHt(HList, varCovarFunc = "DBD", Qvar = 1, QCorrLength = 0.8, makeQ = T)
#
# generateQHt(HList, varCovarFunc = "Balgovind", Qvar = 1, QCorrLength = 0.8, makeQ = T)
# 
# generateQHt(HList, varCovarFunc = "Exponential", Qvar = 1, QCorrLength = 0.8, makeQ = T)
# 
# generateQHt(HList, varCovarFunc = "Gaussian", Qvar = 1, QCorrLength = 0.8, makeQ = T)
