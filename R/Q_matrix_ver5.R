generateQ <- function(nrows, ncols, varCovarFunc, Qvar, QCorrLength, Qplot = F) {
  
  # varCovarFunc <- "DBD" # "Balgovind" # 
  # Qvar <- 1
  # QCorrLength <- 0.8 # 1 # 

  # nrows <- 59 #71 
  # ncols <- 42 #50
  # print(nrows)
  # print(ncols)
  
  p <- nrows * ncols
  # print(p)
  
  observableStates <- 2
  
  Q <- matrix(0, p, p) # Pre-allocating Q

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
    
    print(varCovarFunc)
    
    #print(Q[1:5, 1:5])
    
    #print(det(Q)) # Takes a long time to calculate. The determinant is zero meaning Q is non-invertible.
    
    # Generating the full Q matrix
    
    # Block diagonalization if there are multiple observable states
    QFull <- Q
    
    Q0 <- matrix(0, p, p)
    
    Qtop <- cbind(QFull, Q0)
    #print(dim(Qtop))
    
    Qbottom <- cbind(Q0, QFull)
    #print(dim(Qbottom))
    
    QFull <- rbind(Qtop, Qbottom)

    print(dim(QFull))
    
    #print(QFull[1:5, 1:5])

    return(list("Q" = Q, "QFull" = QFull))
}

#--------------#
# Example Call #
#--------------#
#
# generateQ(nrows = 71, ncols = 50, varCovarFunc = "DBD", Qvar = 1, QCorrLength = 0.8, Qplot = F)
#
# generateQ(nrows = 71, ncols = 50, varCovarFunc = "Balgovind", Qvar = 1, QCorrLength = 0.8, Qplot = F)
# 
# generateQ(nrows = 71, ncols = 50, varCovarFunc = "Exponential", Qvar = 1, QCorrLength = 0.8, Qplot = F)
# 
# generateQ(nrows = 71, ncols = 50, varCovarFunc = "Gaussian", Qvar = 1, QCorrLength = 0.8, Qplot = F)
