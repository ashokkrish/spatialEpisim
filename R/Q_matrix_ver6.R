# rs <- createRasterStack("Democratic Republic of Congo", rasterAgg = 10, isCropped = T, c("Ituri", "Nord-Kivu"))
# varCovarFunc <- "DBD"
# QVar <- 2
# QCorrLength <- 0.8
# states_observable <- 2

source("R/rasterStack.R") 

genQ = function(rs, varCovarFunc, QVar, QCorrLength, nbhd, states_observable = 2) {
  
  nrows <- nrow(rs$rasterStack)
  #print(nrows)
  ncols <- ncol(rs$rasterStack)
  #print(ncols)
  p <- nrows*ncols
  #print(p)
  
  Q <- matrix(0,p,p)
  # uninhabitableCells <- c()
  # 
  # for (a in 1:nrows) {    
  #   for (b in 1:ncols){    
  #     if (rs$rasterStack[["Inhabitable"]][a,b] == 0){
  #       uninhabitableCells <- append(uninhabitableCells, cellFromRowCol(rs$rasterStack, a, b))
  #     }
  #   }
  # }

  for (i in 1:p){
    #if (!(i %in% uninhabitableCells)) {
      icol <- ceiling(i/ncols)
      irow <- (i-1)%%ncols + 1
      for (j in 1:p) {
        #if (!(i %in% uninhabitableCells)) {        
          jcol <- ceiling(j/ncols)
          jrow <- (j-1)%%ncols + 1
          d <- sqrt((irow-jrow)^2 + (icol - jcol)^2)
          
          if (d < nbhd) { 
            if (varCovarFunc == "DBD"){
              val <- QVar*QCorrLength^d
            } else if (varCovarFunc == "Balgovind"){
              val <- QVar*(1 + (d/QCorrLength))*exp(-d/QCorrLength)
            } else if (varCovarFunc == "Exponential"){
              val <- QVar*exp(-d/QCorrLength)
            } else if (varCovarFunc == "Gaussian"){
              val <- QVar*exp(-(d^2)/2*(QCorrLength^2))
            } else if (varCovarFunc == "Spherical") {
              # Note that "QCorrLength" actually refers to the radius for the
              # spherical variance-covariance function
              if (d < QCorrLength) {
                val <- QVar*((3*d)/(2*QCorrLength) - (d^3)/(2*(QCorrLength^3)))
              } else {
                val <- 0
              }
            } else {
              stop('Invalid variance-covariance function selected. Currently supported functions are: DBD, Balgovind, Exponential, Gaussian and Spherical')
              # Error if selected variance-covariance function is invalid
            }
           Q[i,j] = val
           }
        #}
      }
    #}
  }
  
  # print(dim(Q))
  
  if (states_observable == 2) {
    QFull <- Q
    
    Q0 <- matrix(0, p, p)
    
    Qtop <- cbind(QFull, Q0)
    #print(dim(Qtop))
    
    Qbottom <- cbind(Q0, QFull)
    #print(dim(Qbottom))
    
    QFull <- rbind(Qtop, Qbottom)
    
    #print(dim(QFull))
    
    #print(QFull[1:5, 1:5]) 
    
    return(list("Q" = Q, "QFull" = QFull))
  }
  
}

# Qmat <- genQ(createRasterStack("Democratic Republic of Congo", rasterAgg = 10, isCropped = T, c("Ituri", "Nord-Kivu")), "DBD", QVar = 1, QCorrLength = 0.8, nbhd = 4, states_observable = 2)
# 
# x <- 1:10
# y <- 1:10
# 
# X <- matrix(x, nrow = 10, ncol = 10, byrow = TRUE)
# Y <- matrix(y, nrow = 10, ncol = 10, byrow = FALSE)
# 
# library(plot3D)
# persp3D(x = X, y = Y, z = Qmat$Q[1:10,1:10], theta = 90, expand = 0.5,
#         xlab = "Columns", ylab = "Rows", scale = FALSE, clim = c(0, 1),
#         colkey = list(side = 1))
# 
#  plot(Qmat$Q[1775,1600:1900])
