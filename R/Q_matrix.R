generateQ <- function(nrows, ncols, varCovarFunc, Qvar, QCorrLength, states_observable = 2) {
  
  # varCovarFunc <- "DBD" # "Balgovind" # 
  # Qvar <- 1
  # QCorrLength <- 0.8 # 1 # 

  # nrows <- 71
  # ncols <- 50

  p <- nrows * ncols
  # print(p)
  
  observableStates <- 2
  
  Q <- Q0 <- matrix(0, p, p) # Pre-allocating Q

    alpha <- matrix(rep(1:p, p), nrow = p, ncol = p)
    JJ <- (alpha %% ncols) + 1
    II <- floor((alpha - JJ) / nrows) + 1
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
    
    #print(dim(Q))
    
    #print(varCovarFunc)
    
    #print(Q[1:5, 1:5])
    
    #print(det(Q)) # Takes a long time to calculate. The determinant is zero meaning Q is non-invertible.
    
    # Generating the full Q matrix
    
    # Block diagonalization if there are multiple observable states
    
    QFull <- Q
    
    if (states_observable > 1)
    {
      for (n in seq(from = 1, to = states_observable-1, by = 1)){
        QFull <- cbind(QFull, Q0)
      }
      
      for (n in seq(from = 1, to = states_observable-1, by = 1)){
        QTop <- matrix(0, nHealthZones, n*p)
        if ((n+1 - states_observable) !=  0){
          QBottom <- matrix(0, nHealthZones, (states_observable-n-1)*p)
          # print(dim(Htop))
          # print(dim(H))
          # print(dim(Hbottom))
          QFull <- rbind(QFull, cbind(QTop, Q, QBottom))
        }
        else {
          # print(dim(H))
          # print(dim(Htop))
          Hmat <- rbind(Hmat, cbind(QTop, Q))
        }
      }
    }
    return(list("Q" = Q, "QFull" = QFull))
}

#--------------#
# Example Call #
#--------------#
#
# generateQ(nrows = 71, ncols = 50, varCovarFunc = "DBD", Qvar = 1, QCorrLength = 0.8, states_observable =  2)
#
# generateQ(nrows = 71, ncols = 50, varCovarFunc = "Balgovind", Qvar = 1, QCorrLength = 0.8, states_observable =  2)
# 
# generateQ(nrows = 71, ncols = 50, varCovarFunc = "Exponential", Qvar = 1, QCorrLength = 0.8, states_observable =  2)
# 
# generateQ(nrows = 71, ncols = 50, varCovarFunc = "Gaussian", Qvar = 1, QCorrLength = 0.8, states_observable =  2)
