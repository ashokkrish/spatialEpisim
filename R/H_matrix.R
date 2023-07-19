# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Avoid "magic constants"
source('R/rasterStack.R')

rasterStack <- createRasterStack("Democratic Republic of Congo", 10, isCropped = T, level1Names = c("Ituri", "Nord-Kivu"))$rasterStack
sitRepData = 'observeddata/Ebola_Health_Zones_LatLon.csv'
states_observable <- 2  

generateLIO <- function(rasterStack, sitRepData, states_observable = 2)

nrows <- nrow(rasterStack) # 48
ncols <- ncol(rasterStack)  # 34
# states_total <- 6               
p <- ncols*nrows                 # Dimentionality of the state space

Locations <- read.csv(sitRepData, header = T)
nHealthZones <-  dim(Locations)[1] # Number of health zones in North Kivu and Ituri provinces of DRC

for (i in 1:nHealthZones) {
  rindex(i) <- floor((ymax(rasterStack) - locations[3,i])/yres(rasterStack))
  cindex(i) <- floor((locations[4,i] - xmin(rasterStack))/xres(rasterStack))
  Hposition(i) <- nrows*cindex + rindex 
}

Locations <- cbind(Locations, rindex, cindex, HPosition)


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
  
  H <- H0 <- matrix(0, nHealthZones, p)
  
  for(k in 1:nHealthZones)
  {
    H[k, Hposition[k]] <- 1 
  }
  
  dim(H)
  print(paste("Number of Health Zones:", sum(H))) # 1 * nHealthZones = 1*q
  
  #---------------------------------------------------------------------------------------------------------#
  # Combine states_observable blocks of H matrix to form the full operator for all states_observable states #
  #---------------------------------------------------------------------------------------------------------#
  
  Hmat <- rbind(cbind(H,H0), cbind(H0,H))
   
  #print(sum(Hmat)) # states_obervable * nHealthZones = 2*q
  
  print(paste("Dimension of the linear interpolation operator, H:")); print(dim(Hmat))
	  