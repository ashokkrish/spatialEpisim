# # source("R/rasterStack.R")
# 
# nrows <- 71
# ncols <- 50
# states_total <- 6                # Total number of states: S, V, E, I, R, D
# states_observable <- 2           # Number of observable states: I and D
# p <- ncols*nrows                 # Dimentionality of the state space
# 
# rho <- 0.8
# sigma <- 1
# 
# Locations <- read.csv("observeddata/Ebola_Health_Zones_LatLon.csv", header = T)
# #Locations <- Locations[order(Locations$Hposition),]
# q <- dim(Locations)[1] # Number of health zones in North Kivu and Ituri provinces of DRC

# # rs <- createRasterStack("Democratic Republic of Congo", rasterAgg = 10, isCropped = T, level1Names = c("Ituri", "Nord-Kivu"))
# # rasterStack <- rs$rasterStack
#
# # indices <- 1:p
# # cellCenterLat <- matrix(0,p,1)
# # cellCenterLong <- cellCenterLat
# #
# # for (i in 1:nrows){
# #   for (j in 1:ncols){
# #     pos = ((i-1)*ncols) + j
# #     cellCenterLat[pos] <- ymax(rasterStack) - (i-0.5)*yres(rasterStack)
# #     cellCenterLong[pos] <- xmin(rasterStack) + (j-0.5)*xres(rasterStack)
# #   }
# # }
# #
# # centers <- data.frame(Index = indices, CenterLatitude = cellCenterLat, CenterLongitude = cellCenterLong)

# QHt <- matrix(0, p, q)
# 
# for (n in 1:q){
#   HZRow <- Locations[n,5]
#   HZCol <- Locations[n,6]
#   for (i in 1:nrows){
#     for (j in 1:ncols){
#       pos <- ((i-1)*ncols)+j
#       d <- sqrt((HZRow - i)^2 + (HZCol - j)^2)
#       QHt[pos,n] <- (sigma^2)*(1+(d/rho))*exp(-d/rho)
#     }
#   }
# }
# 
# QFull <- cbind(rbind(QHt, matrix(0,p,q)),rbind(matrix(0,p,q),QHt))
# QHt <- QFull