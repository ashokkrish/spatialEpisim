options(conflicts.policy = list(warn = FALSE))
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(Matrix))
shhh(library(raster))

generateLIO2 <- function(rasterStack, sitRepData, states_observable = 2) {

  nrows <- nrow(rasterStack)
  ncols <- ncol(rasterStack)
  p <- ncols*nrows 
  
  Locations <- read.csv(file = sitRepData, header = T)
  nHealthZones <-  dim(Locations)[1]
  
  Hpos <- cellFromXY(rasterStack, as.matrix(Locations[,3:2]))
  
  H <- H0 <- matrix(0,nHealthZones,p)
  for (i in 1:nHealthZones){
    H[i,Hpos[i]] <- 1
  }
  Htop <- cbind(H, H0)
  Hbottom <- cbind(H0, H)
  
  Hmat <- rbind(Htop, Hbottom)  
  
  return(list("Hmat" = Hmat, "Locations" = Locations, "rasterStack" = rasterStack, "states_observable" = states_observable))
}

# test <- generateLIO2(rasterStack = createRasterStack(selectedCountry = "Democratic Republic of Congo", rasterAgg = 10, isCropped = T, level1Names = c("Ituri", "Nord-Kivu"))$rasterStack, sitRepData = "observeddata/Ebola_Health_Zones_LatLon.csv", states_observable = 2)
# 
# # r <- raster(ncols=10, nrows=10)
# # 
# # cellFromXY(r, cbind(c(0.5,5), c(15, 88)))
# 
# rowSums(test$Hmat)
