# rm(list = ls())

options(conflicts.policy = list(warn = FALSE))
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(Matrix))
shhh(library(raster))

source('R/rasterStack.R')

# rasterStack <- createRasterStack(selectedCountry = "Democratic Republic of Congo", rasterAgg = 10, isCropped = T, level1Names = c("Ituri", "Nord-Kivu"))$rasterStack
# sitRepData <- "observeddata/Ebola_Health_Zones_LatLon_4zones.csv"
# states_observable <- 2 # 1 #

generateLIO2 <- function(rasterStack, sitRepData, states_observable = 2) {
  nrows <- nrow(rasterStack)
  ncols <- ncol(rasterStack)
  p <- ncell(rasterStack) 
  
  Locations <- read.csv(file = sitRepData, header = T)
  nHealthZones <-  dim(Locations)[1]
  
  cellFromXY(rasterStack, cbind(27.13,3.72)) # 1. Note: This is the top left corner cell
  cellFromXY(rasterStack, cbind(29.47306, 0.49113)) # 1929. Note: This is the Lon, Lat for Beni
  cellFromXY(rasterStack, cbind(0.49113, 29.47306)) # NA will be produced if you flip the (Lon, Lat) to (Lat, Lon)
  cellFromXY(rasterStack, cbind(31.29,-2.19)) # 3550. Note This is the bottom righ corner cell
  
  Hpos <- cellFromXY(rasterStack, as.matrix(Locations[ ,3:2]))
  print('Hpos is')
  print(Hpos)

  ##########FOR DIAGNOSTICS##########################
  rows <- rowFromY(rasterStack, as.vector(Locations[,2]))
  rows
  cols <- colFromX(rasterStack, as.vector(Locations[,3]))
  cols
  
  # print('A test:')
  # print(Hpos[5])
  # print(rows[5])
  # print(cols[5])
  
  # Hpos for Beni is obtained as = (rows - 1)*ncols + cols = (39-1)*50 + 29 = 1929
  ###################################################
  
  if (!(anyDuplicated(Hpos) == 0)){
    print('Warning: Duplicate Indices')
  }
  
  #------------------------------------------------------------------------#
  # Compute H matrix, the linear interpolation operator of dimension q x p #
  #------------------------------------------------------------------------#
  
  H <- H0 <- matrix(0, nHealthZones, p)
  
  for (i in 1:nHealthZones){
    #print(paste("Hposition:", Hpos[i]))
    H[i, Hpos[i]-1] <- 0.08
    H[i, Hpos[i]+2] <- 0.04
    H[i, Hpos[i]-2] <- 0.04
    H[i, Hpos[i]+2*ncols] <- 0.04
    H[i, Hpos[i]+2*ncols+1] <- 0.04
    H[i, Hpos[i]+2*ncols-1] <- 0.04
    H[i, Hpos[i]-2*ncols] <- 0.04
    H[i, Hpos[i]+2+ncols] <- 0.04
    H[i, Hpos[i]+2+2*ncols] <- 0.04
    H[i, Hpos[i]+2-2*ncols] <- 0.04
    H[i, Hpos[i]+2-ncols] <- 0.04
    H[i, Hpos[i]-2+2*ncols] <- 0.04
    H[i, Hpos[i]-1+2*ncols] <- 0.04
    H[i, Hpos[i]+1+2*ncols] <- 0.04
    H[i, Hpos[i]-2-ncols] <- 0.04
    H[i, Hpos[i]-2-2*ncols] <- 0.04
    H[i, Hpos[i]-2+ncols] <- 0.04
    H[i, Hpos[i]+1] <- 0.08
    H[i, Hpos[i]-ncols] <- 0.08
    H[i, Hpos[i]+ncols] <- 0.08
    H[i, Hpos[i]-ncols -1] <- 0.08
    H[i, Hpos[i]-ncols +1] <- 0.08
    H[i, Hpos[i]+ncols -1] <- 0.08
    H[i, Hpos[i]+ncols +1] <- 0.08
    H[i, Hpos[i]] <- 0.12
  }
  H <- H*5/7
  
  print(paste("Number of Health Zones:", nHealthZones))

  Hmat <- H
  
  # if (states_observable == 2)
  # {
  #   Htop <- cbind(H, H0)
  #   Hbottom <- cbind(H0, H)
  #   
  #   Hmat <- rbind(Htop, Hbottom)
  # } else {
  #   Hmat <- H
  # }
  
  if (states_observable > 1)
  {
    for (n in seq(from = 1, to = states_observable-1, by = 1)){
      Hmat <- cbind(Hmat, H0)
    }
    
    for (n in seq(from = 1, to = states_observable-1, by = 1)){
      Htop <- matrix(0, nHealthZones, n*p)
      if ((n+1 - states_observable) !=  0){
        Hbottom <- matrix(0, nHealthZones, (states_observable-n-1)*p)
        # print(dim(Htop))
        # print(dim(H))
        # print(dim(Hbottom))
        Hmat <- rbind(Hmat, cbind(Htop, H, Hbottom))
      }
      else {
        # print(dim(H))
        # print(dim(Htop))
        Hmat <- rbind(Hmat, cbind(Htop, H))
        }
      }
    }

  print(paste("Dimension of the linear interpolation operator, H:")); print(dim(Hmat))
  
  print(paste("Row sums of H matrix:")); print(rowSums(Hmat))
  # print(table(colSums(Hmat)))
  # print(sum(Hmat))
  # print(table(Hmat))
  
  return(list("Hmat" = Hmat, "Locations" = Locations, "rasterStack" = rasterStack, "states_observable" = states_observable))
}

# test <- generateLIO2(rasterStack = createRasterStack(selectedCountry = "Democratic Republic of Congo", rasterAgg = 10, isCropped = T, level1Names = c("Ituri", "Nord-Kivu"))$rasterStack, sitRepData = "observeddata/Ebola_Health_Zones_LatLon.csv", states_observable = 2)
# 
# H <- test$Hmat
# 
# HHt <- H%*%t(H)
# dim(HHt)
# rowSums(HHt)
# diag(HHt)
# det(HHt)