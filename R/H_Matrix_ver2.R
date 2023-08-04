#rm(list = ls())

options(conflicts.policy = list(warn = FALSE))
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(Matrix))
shhh(library(raster))

source('R/rasterStack.R')

# rasterStack <- createRasterStack(selectedCountry = "Democratic Republic of Congo", rasterAgg = 10, isCropped = T, level1Names = c("Ituri", "Nord-Kivu"))$rasterStack
# sitRepData <- "observeddata/Ebola_Health_Zones_LatLon_4zones.csv"
# states_observable <- 2

generateLIO2 <- function(rasterStack, sitRepData, states_observable = 2) {
  nrows <- nrow(rasterStack)
  ncols <- ncol(rasterStack)
  p <- ncols*nrows 
  
  Locations <- read.csv(file = sitRepData, header = T)
  nHealthZones <-  dim(Locations)[1]
  
  Hpos <- cellFromXY(rasterStack, as.matrix(Locations[ ,3:2]))
  #Hpos
  
  ##########FOR DIAGNOSTICS##########################
  rows <- rowFromY(rasterStack, as.vector(Locations[,2]))
  row
  cols <- colFromX(rasterStack, as.vector(Locations[,3]))
  cols
  ###################################################
  
  if (!(anyDuplicated(Hpos) == 0)){
    print('Warning: Duplicate Indices')
  }
  
  H <- H0 <- matrix(0, nHealthZones, p)
  
  for (i in 1:nHealthZones){
    #print(paste("Hposition:", Hpos[i]))

    H[i, Hpos[i]] <- 1
  }
  
  print(paste("Number of Health Zones:", sum(H)))
  rowSums(H)
  table(colSums(H))
  
  Htop <- cbind(H, H0)
  Hbottom <- cbind(H0, H)
  
  Hmat <- rbind(Htop, Hbottom)
  print(paste("Dimension of the linear interpolation operator, H:")); print(dim(Hmat))
  
  print(rowSums(Hmat))
  print(table(colSums(Hmat)))
  #print(sum(Hmat))
  #print(table(Hmat))
  
  return(list("Hmat" = Hmat, "Locations" = Locations, "rasterStack" = rasterStack, "states_observable" = states_observable))
}

#test <- generateLIO2(rasterStack = createRasterStack(selectedCountry = "Democratic Republic of Congo", rasterAgg = 10, isCropped = T, level1Names = c("Ituri", "Nord-Kivu"))$rasterStack, sitRepData = "observeddata/Ebola_Health_Zones_LatLon.csv", states_observable = 2)
# 
# # r <- raster(ncols=10, nrows=10)
# # 
# # cellFromXY(r, cbind(c(0.5,5), c(15, 88)))
# 
# rowSums(test$Hmat)
