generateQ <- function(varCovarFunc, Qvar, QCorrLength) {
  
  # varCovarFunc <- "DBD" # "Balgovind" # 
  # Qvar <- 1
  # QCorrLength <- 0.8 # 1 # 

  nrows <- 71 
  ncols <- 50
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
    
    print(dim(Q))
    
    print(varCovarFunc)
    
    print(Q[1:10, 1:10])
    
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
    
    print(QFull[1:10, 1:10])
    
    print(QFull[3545:3550, 3545:3550])

    # x <- 1:ncol(Q)
    # y <- 1:nrow(Q)

    # # Create a grid of x and y values
    # X <- matrix(x, nrow = nrow(Q), ncol = ncol(Q), byrow = TRUE)
    # Y <- matrix(y, nrow = nrow(Q), ncol = ncol(Q), byrow = FALSE)

    # #Plot the 3D surface
    # library(rgl)
    # library(plot3D)
    # persp3D(X, Y, Q, scale = FALSE, theta = 90, expand = 0.2)
    # persp3D(X, Y, Q, col = "lightpink", xlab = "Columns", ylab = "Rows", zlab = "Values",
    # facets = FALSE, main = "3D Surface Plot of Q")

  return(list("Q" = Q))
}

#--------------#
# Example Call #
#--------------#
#
# generateQ(varCovarFunc = "DBD", Qvar = 1, QCorrLength = 0.8)
#
# generateQ(varCovarFunc = "Balgovind", Qvar = 1, QCorrLength = 0.8)
# 
# generateQ(varCovarFunc = "Exponential", Qvar = 1, QCorrLength = 0.8)
# 
# generateQ(varCovarFunc = "Gaussian", Qvar = 1, QCorrLength = 0.8)
