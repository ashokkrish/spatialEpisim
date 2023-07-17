# #memory.limit()
# 
# # install.packages("lattice", dependencies = T)
# # install.packages("Matrix", dependencies = T)
# # install.packages("ggplot2", dependencies = T)
# 
# library(Matrix)
# # library(lattice)
# # library(ggplot2)
# 
# getwd() 
# 
# #setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# 
# # Avoid "magic constants"
# nrows <- 48 #125
# ncols <- 34 #72
# states_total <- 6                # Total number of states: S, V, E, I, R, D
# states_observable <- 2           # Number of observable states: I and D
# p <- ncols*nrows                 # Dimensionality of the state space
# 
# Qvariance <- 1
# Qrho <- 0.8
# 
# ptm <- proc.time()
# 
# alpha <- matrix(rep(1:p, p), p, p)
# JJ <- (alpha - 1L) %% nrows + 1L
# II <- ((alpha - JJ)/ncols) + 1L
# LL <- t(JJ)
# KK <- t(II)
# d <- sqrt((LL - JJ)^2 + (KK - II)^2)
# Q2 <- Qvariance*(Qrho^d)
# 
# print(proc.time() - ptm)
# 
# #all.equal(Q, Q2) # If you'd like to compare versions 1 and 2
# 
# #print(paste("Dimension of the Q2 matrix:")); print(dim(Q2))
# 
# #levelplot(Q2, col.regions= colorRampPalette(c("red","green","blue")))
# 
# #-------------------------------------------#
# # Create a block-diagonal covariance matrix #
# #-------------------------------------------#
# 
# Qf.OSI <- bdiag(Q2, Q2)
# 
# # Qf.OSI <- as.matrix(bdiag(Q2,Q2)) #as.matrix is required for the levelplot() function to work
#  
# print(proc.time() - ptm)
#  
# print(paste("Dimension of the forecast error covariance matrix, Qf.OSI:")); print(dim(Qf.OSI))
