#rm(list = ls())

nrows <- 5
ncols <- 5
states_total <- 6
states_observable <- 2
p <- nrows * ncols

Qvariance <- 1
Qrho <- 0.8

alpha <- matrix(rep(1:p, p), nrow = p, ncol = p)
JJ <- (alpha - 1) %% nrows + 1
II <- floor((alpha - JJ) / ncols) + 1
LL <- t(JJ)
KK <- t(II)
d <- sqrt((LL - JJ)^2 + (KK - II)^2)
# Q <- Qvariance * (Qrho^d)
Q <- Qvariance*(1 + (d/Qrho))*exp(-d/Qrho)

print(dim(Q))
Q[1:5, 1:5]

x <- 1:ncol(Q)
y <- 1:nrow(Q)

# Create a grid of x and y values
X <- matrix(x, nrow = nrow(Q), ncol = ncol(Q), byrow = TRUE)
Y <- matrix(y, nrow = nrow(Q), ncol = ncol(Q), byrow = FALSE)

library(plot3D)
persp3D(x = X, y = Y, z = Q, theta = 90, expand = 0.5,
        xlab = "Columns", ylab = "Rows", scale = FALSE, 
        colkey = list(side = 1))

# # Plot the 3D surface
# library(rgl)
# persp3d(X, Y, Q, col = "lightgreen", xlab = "Columns", ylab = "Rows", zlab = "Values",
#         ticktype = "detailed", xlim = c(0, 100), ylim = c(0, 100))

# Block diagonalization if there are multiple observable states
QFull <- Q

Q0 <- matrix(0, p, p)

Qtop <- cbind(QFull, Q0)
#print(dim(Qtop))

Qbottom <- cbind(Q0, QFull)
#print(dim(Qbottom))

#QFull <- rbind(Qtop, Qbottom)

print(dim(QFull))

x <- 1:ncol(QFull)
y <- 1:nrow(QFull)

# Create a grid of x and y values
X <- matrix(x, nrow = nrow(QFull), ncol = ncol(QFull), byrow = TRUE)
Y <- matrix(y, nrow = nrow(QFull), ncol = ncol(QFull), byrow = FALSE)

library(plot3D)
persp3D(x = X, y = Y, z = QFull, theta = 90, expand = 0.5,
        xlab = "Columns", ylab = "Rows", scale = FALSE,
        colkey = list(side = 1))

# Plot the 3D surface
library(rgl)
persp3d(X, Y, QFull, col = "lightgreen", xlab = "Columns", ylab = "Rows", zlab = "Values",
        ticktype = "detailed", main = "3D Surface Plot of QFull", xlim = c(0, 5), ylim = c(0, 5))