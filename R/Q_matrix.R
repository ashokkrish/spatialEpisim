source("R/rasterWorldPop.R") 
source("R/rasterStack.R") 

genQ <- function(nrows, ncols, varCovarFunc, QVar, QCorrLength, nbhd, states_observable = 2) {
  
  p <- nrows*ncols
  print(p)
  
  Q <- matrix(0, p, p)
  
  # uninhabitableCells <- c()
  # 
  # for (a in 1:nrows) {    
  #   for (b in 1:ncols){    
  #     if (rs$rasterStack[["Inhabitable"]][a,b] == 0){
  #       uninhabitableCells <- append(uninhabitableCells, cellFromRowCol(rs$rasterStack, a, b))
  #     }
  #   }
  # }

  rows <- rep(1:(p / ncols), each = ncols)
  cols <- rep(1:ncols, times = (p / ncols))
  
  irow <- matrix(rep(rows, length(rows)), nrow = p, byrow = TRUE)
  icol <- matrix(rep(cols, length(cols)), nrow = p, byrow = TRUE)
  jrow <- t(irow)  # Transpose of irow matrix
  jcol <- t(icol)  # Transpose of icol matrix
  d <- sqrt((irow - jrow)^2 + (icol - jcol)^2)

  if (varCovarFunc == "DBD"){
    varMatrix <- QVar*QCorrLength^d
  } else if (varCovarFunc == "Balgovind"){
    varMatrix <- QVar*(1 + (d/QCorrLength))*exp(-d/QCorrLength)
  } else if (varCovarFunc == "Exponential"){
    varMatrix <- QVar*exp(-d/QCorrLength)
  } else if (varCovarFunc == "Gaussian"){
    varMatrix <- QVar*exp(-(d^2)/2*(QCorrLength^2))
  } else if (varCovarFunc == "Spherical") {
    # Note that "QCorrLength" actually refers to the radius for the
    # spherical variance-covariance function
    varMatrix <- QVar*((3*d)/(2*QCorrLength) - (d^3)/(2*(QCorrLength^3)))
    varMatrix[d >= QCorrLength] <- 0
  } else {
    stop('Invalid variance-covariance function selected. Currently supported functions are: DBD, Balgovind, Exponential, Gaussian and Spherical')
    # Error if selected variance-covariance function is invalid
  }

  Q[d < nbhd] <- varMatrix[d < nbhd]

  
  # for (i in 1:p){
  #   #if (!(i %in% uninhabitableCells)) {
  #     icol <- ceiling(i/ncols)
  #     print(paste("icol =", icol))
  #     irow <- (i-1)%%ncols + 1
  #     print(paste("irow =", irow))
  #     for (j in 1:p) {
  #       #if (!(i %in% uninhabitableCells)) {
  #         jcol <- ceiling(j/ncols)
  #         print(paste("jcol =", jcol))
  #         jrow <- (j-1)%%ncols + 1
  #         print(paste("jrow =", jrow))
  #         d <- sqrt((irow-jrow)^2 + (icol - jcol)^2)
  #         print(d)
  # 
  #         if (d < nbhd) {
  #           if (varCovarFunc == "DBD"){
  #             val <- QVar*QCorrLength^d
  #           } else if (varCovarFunc == "Balgovind"){
  #             val <- QVar*(1 + (d/QCorrLength))*exp(-d/QCorrLength)
  #           } else if (varCovarFunc == "Exponential"){
  #             val <- QVar*exp(-d/QCorrLength)
  #           } else if (varCovarFunc == "Gaussian"){
  #             val <- QVar*exp(-(d^2)/2*(QCorrLength^2))
  #           } else if (varCovarFunc == "Spherical") {
  #             # Note that "QCorrLength" actually refers to the radius for the
  #             # spherical variance-covariance function
  #             if (d < QCorrLength) {
  #               val <- QVar*((3*d)/(2*QCorrLength) - (d^3)/(2*(QCorrLength^3)))
  #             } else {
  #               val <- 0
  #             }
  #           } else {
  #             stop('Invalid variance-covariance function selected. Currently supported functions are: DBD, Balgovind, Exponential, Gaussian and Spherical')
  #             # Error if selected variance-covariance function is invalid
  #           }
  #          Q[i,j] = val
  #          print(val)
  #          }
  #       #}
  #     }
  #   #}
  # }

  diag(Q) <- ifelse(diag(Q) == 0, QVar, diag(Q))
  
  print(dim(Q))
  QFull <- Q
  
  if (states_observable == 2) {
    
    Q0 <- matrix(0, p, p)
    
    Qtop <- cbind(QFull, Q0)
    #print(dim(Qtop))
    
    Qbottom <- cbind(Q0, QFull)
    #print(dim(Qbottom))
    
    QFull <- rbind(Qtop, Qbottom)
  }
    
    print(dim(QFull))
    print(QFull[1:5, 1:5]) 
    
    return(list("Q" = Q, "QFull" = QFull))
}
