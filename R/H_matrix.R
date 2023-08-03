#rm(list = ls())

library(Matrix)

# Avoid "magic constants"
# rasterStack <- createRasterStack(selectedCountry = "Democratic Republic of Congo", rasterAgg = 10, isCropped = T, level1Names = c("Ituri", "Nord-Kivu"))$rasterStack
# sitRepData = 'observeddata/Ebola_Health_Zones_LatLon.csv'
# states_observable <- 2
# states_total <- 6

generateLIO <- function(rasterStack, sitRepData, states_observable = 2) {

  source('R/rasterStack.R')
  
  print(rasterStack)
  print(xmax(rasterStack));  print(ymax(rasterStack));   print(xmin(rasterStack));  print(ymin(rasterStack))
  
  nrows <- nrow(rasterStack)
  ncols <- ncol(rasterStack)
  
  p <- ncols*nrows                    # Dimentionality of the state space
  #print(p)
  
  Locations <- read.csv(file = sitRepData, header = T)
  nHealthZones <-  dim(Locations)[1]    # Number of health zones in North Kivu and Ituri provinces of DRC
  #print(nHealthZones)                   # Dimentionality of the data space
  
  rindex <- cindex <- Hposition <- numeric(nHealthZones)
  
  for (i in 1:nHealthZones) {
    # rindex[i] <- floor((ymax(rasterStack) - as.numeric(Locations[i,2]))/yres(rasterStack)) # TBW
    # cindex[i] <- floor((as.numeric(Locations[i,3]) - xmin(rasterStack))/xres(rasterStack)) # TBW
    
    rindex[i] <- floor((as.numeric(Locations[i,3]) - xmin(rasterStack))/xres(rasterStack)) # AK
    cindex[i] <- floor((ymax(rasterStack) - as.numeric(Locations[i,2]))/yres(rasterStack)) # AK
  
    Hposition[i] <- ncols*(rindex[i]-1) + cindex[i] # TBW Check this line closely
    
    print(c(rindex[i],  cindex[i], Hposition[i]))
  }
  
  Locations <- cbind(Locations, rindex, cindex, Hposition)
  print(Locations)
  
  #------------------------------------------------------------------------#
  # Compute H matrix, the linear interpolation operator of dimension q x p #
  #------------------------------------------------------------------------#
  
  H <- H0 <- matrix(0, nHealthZones, p)
  
  for(k in 1:nHealthZones)
  {
    H[k, Hposition[k]] <- 1 
  }
  
  # print(rasterStack[23,33])
  # print(rasterStack[22,36])
  # print(rasterStack[21,33])
  # print(rasterStack[19,24])
  
  # print(rasterStack[33,23])
  # print(rasterStack[36,22])
  # print(rasterStack[33,21])
  # print(rasterStack[24,19])
  
  print(paste("Number of Health Zones:", sum(H)))
  # print(dim(H))
  # print(sum(H))
  # print(table(H))
  # 
  # print(dim(H0))
  # print(sum(H0))
  # print(table(H0))
  
  #--------------------------------------------------------------------------------------------------#
  # Combine states_observable blocks of H matrix to form the full operator for all states_observable #
  #--------------------------------------------------------------------------------------------------#
    
  # Hmat <- H
  #   
  # print(dim(Hmat))
  # print(list(rep(H,states_observable)))
  
  # if (states_observable > 1){
  #   for (n in seq(from = 1, to = states_observable-1, by = 1)){
  #   Hmat <- cbind(Hmat, H0)
  #   }
  #   
  #   for (n in seq(from = 1, to = states_observable-1, by = 1)){
  #    Htop <- matrix(0, nHealthZones, n*p)
  #    if ((n+1 - states_observable) !=  0){
  #     Hbottom <- matrix(0, nHealthZones, (states_observable-n-1)*p)
  #     # print(dim(Htop))
  #     # print(dim(H))
  #     # print(dim(Hbottom))
  #     Hmat <- rbind(Hmat, cbind(Htop, H, Hbottom))
  #    }
  #    else {
  #      # print(dim(H))
  #      # print(dim(Htop))
  #      Hmat <- rbind(Hmat, cbind(Htop, H))
  #    }
  #   }
  #  }
  
  Htop <- cbind(H, H0)
  Hbottom <- cbind(H0, H)
    
  Hmat <- rbind(Htop, Hbottom)   
  print(paste("Dimension of the linear interpolation operator, H:")); print(dim(Hmat))
   
  #print(dim(Hmat))
  #print(sum(Hmat))
  #print(table(Hmat))
  
  #print(rowSums(Hmat))
  #print(Locations)
   
  return(list("Hmat" = Hmat, "Locations" = Locations, "rasterStack" = rasterStack, "states_observable" = states_observable))
}

#--------------#
# Example Call #
#--------------#

# test <- generateLIO(rasterStack = createRasterStack(selectedCountry = "Democratic Republic of Congo", rasterAgg = 10, isCropped = T, level1Names = c("Ituri", "Nord-Kivu"))$rasterStack, sitRepData = "observeddata/Ebola_Health_Zones_LatLon.csv", states_observable = 2)
# 
# test
# dim(test$Hmat)
# test$Locations
# H <- test$Hmat
# rowSums(H)
# table(colSums(H))
# 
# HHt <- H%*%t(H)
# dim(HHt)
# rowSums(HHt)
# 
# det(HHt)
# eigen(HHt)$values
# # When all eigenvalues of a square matrix are equal to 1,
# # it means that the matrix behaves like the identity matrix.