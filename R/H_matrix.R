# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(Matrix))
# Avoid "magic constants"
source('R/rasterStack.R')

# rasterStack <- createRasterStack("Democratic Republic of Congo", 10, isCropped = T, level1Names = c("Ituri", "Nord-Kivu"))$rasterStack
# sitRepData = 'observeddata/Ebola_Health_Zones_LatLon.csv'
# states_observable <- 2

 generateLIO <- function(rasterStack, sitRepData, states_observable = 2) {

nrows <- nrow(rasterStack) # 48
ncols <- ncol(rasterStack)  # 34
# states_total <- 6               
p <- ncols*nrows                 # Dimentionality of the state space

Locations <- read.csv(file = sitRepData, header = T)
nHealthZones <-  dim(Locations)[1] # Number of health zones in North Kivu and Ituri provinces of DRC
rindex <- cindex <- Hposition <- numeric(nHealthZones)

#print(Locations[1,3])

for (i in 1:nHealthZones) {
  rindex[i] <- floor((ymax(rasterStack) - as.numeric(Locations[i,2]))/yres(rasterStack))
  cindex[i] <- floor((as.numeric(Locations[i,3]) - xmin(rasterStack))/xres(rasterStack))
  # print(rindex[i])
  # print(cindex[i])
  # print(nrows)
  Hposition[i] <- ncols*(rindex[i]-1) + cindex[i] 
}

Locations <- cbind(Locations, rindex, cindex, Hposition)

  #-------------------#
  # Import Ebola Data #
  #-------------------#
  # Ebola_Incidence_Data <- read.csv("Ebola_Incidence_Data.csv", header = T)
  # 
  # names(Ebola_Incidence_Data)
  # dim(Ebola_Incidence_Data)

  # Ebola_Death_Data <- read_excel("observeddata/Ebola_Death_Data.xlsx")
  # 
  # names(Ebola_Death_Data)
  # dim(Ebola_Death_Data)
  
  #----------------------------------------------------------------------------#
  # Compute H matrix, the linear operator of dimension d x states_observable*p #
  #----------------------------------------------------------------------------#
  
#print(nHealthZones)
#print(p)

  H <- H0 <- matrix(0, nHealthZones, p)
  
  #print(H)
  
  for(k in 1:nHealthZones)
  {
    H[k, Hposition[k]] <- 1 
  }
  
  # dim(H)
  # print(paste("Number of Health Zones:", sum(H))) # 1 * nHealthZones = 1*q
  
  #---------------------------------------------------------------------------------------------------------#
  # Combine states_observable blocks of H matrix to form the full operator for all states_observable states #
  #---------------------------------------------------------------------------------------------------------#
  Hmat <- H
 #print(list(rep(H,states_observable)))

if (states_observable > 1){   
  for (n in seq(from = 1, to = states_observable-1, by = 1)){  
  Hmat <- cbind(Hmat,H0)
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
 
 #print(Hmat)
 
 return(list("Hmat" = Hmat,"Locations" = Locations, "rasterStack" = rasterStack, "states_observable" = states_observable))
 
 #print(paste("Dimension of the linear interpolation operator, H:")); print(dim(Hmat))
 
 }
   
  #print(sum(Hmat)) # states_obervable * nHealthZones = 2*q
  
   

#------------------------------Example Call---------------------------------------------

#test <- generateLIO(createRasterStack("Democratic Republic of Congo", rasterAgg=10, isCropped=T, c("Ituri", "Nord-Kivu"))$rasterStack, "observeddata/Ebola_Health_Zones_LatLon.csv", states_observable = 2)	
# print(test)
# print(dim(test$Hmat))
#print(test$Locations)
