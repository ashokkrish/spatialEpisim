# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Avoid "magic constants"
nrows <- 71 # 48
ncols <- 50  # 34
states_total <- 6                # Total number of states: S, V, E, I, R, D
states_observable <- 2           # Number of observable states: I and D
p <- ncols*nrows                 # Dimentionality of the state space

Locations <- read.csv("Ebola_Health_Zones_LatLon_SitRpt54.csv", header = T)

nHealthZones <-  dim(Locations)[1] # Number of health zones in North Kivu and Ituri provinces of DRC

  #-------------------#
  # Import Ebola Data #
  #-------------------#
  # Ebola_Incidence_Data <- read.csv("Ebola_Incidence_Data.csv", header = T)
  # 
  # names(Ebola_Incidence_Data)
  # dim(Ebola_Incidence_Data)

  Ebola_Death_Data <- read_excel("Ebola_Death_Data.xlsx")

  names(Ebola_Death_Data)
  dim(Ebola_Death_Data)
  
  #----------------------------------------------------------------------------#
  # Compute H matrix, the linear operator of dimension d x states_observable*p #
  #----------------------------------------------------------------------------#
  
  H <- H0 <- matrix(0, nHealthZones, p)
  
  Hposition <- Locations[, 7]
  
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
	  