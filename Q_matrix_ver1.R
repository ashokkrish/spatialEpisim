# memory.limit()

# install.packages("lattice", dependencies = T)
# install.packages("Matrix", dependencies = T)
# install.packages("ggplot2", dependencies = T)

library(Matrix)
# library(lattice)
# library(ggplot2)

getwd() 

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Avoid "magic constants"
nrows <- 48 # 125
ncols <- 34 # 72
states_total <- 6                # Total number of states: S, V, E, I, R, D
states_observable <- 2           # Number of observable states: I and D
p <- ncols*nrows                 # Dimensionality of the state space

#--------------------------------------------------------------#
# Compute Qf.OSI, the "constant" model error covariance matrix #
#--------------------------------------------------------------#

#-----------------------------------------------------------------------------#
# The dimension of Qf.OSI matrix is states_observable*p x states_observable*p #
#-----------------------------------------------------------------------------#

Qvariance <- 1
Qrho <- 0.8

ptm <- proc.time()

Q <- matrix(0, p, p) 

for (rowa in 0:(nrows-1))
{
  for (cola in 0:(ncols-1))
  {
    a = rowa * ncols + cola
    for (rowb in 0:(nrows-1))
    {
      for (colb in 0:(ncols-1))
      {
        b = rowb * ncols + colb
        d = sqrt((rowa - rowb)^2 + (cola - colb)^2)
        Q[a+1, b+1] <- Qvariance * (Qrho^d)
        #print(paste("a = ", a+1, "b = ", b+1, "d = ", d))
      }
    }
  }
}

print(proc.time() - ptm)

print(paste("Dimension of the Q matrix:")); print(dim(Q))

# dn <- (det(Q))^(1/p)
# print(dn) 	  # Determinant of Q is very close to zero

# Is the sum of the eigen values of Q is equal to p? YES!
# sum(eigen(Q)$values)

#-------------------------------------------#
# Create a block-diagonal covariance matrix #
#-------------------------------------------#

Qf.OSI <- bdiag(Q, Q)

# Qf.OSI <- as.matrix(bdiag(Q,Q)) #as.matrix is required for the levelplot() function to work
 
print(proc.time() - ptm)

print(paste("Dimension of the forecast error covariance matrix, Qf.OSI:")); print(dim(Qf.OSI))

# library('plot.matrix')
# plot(Qf.OSI) # Takes a lot of time
 
# The below line of code generates a plot using the 'lattice' package.
# For small values of p uncomment and run to see the plot (don't run the below lines for p = 9000)

  # levelplot(Q, col.regions= colorRampPalette(c("red","green","blue")))
  # levelplot(Qf.OSI, col.regions= colorRampPalette(c("red","green","blue")))

# The below three lines of code generates a plot using the 'ggplot2' package.
# For small values of p uncomment and run to see the plot
 
  # dd <- expand.grid(x = 1:ncol(Qf.OSI), y = 1:nrow(Qf.OSI))
  # dd$col <- unlist(c(Qf.OSI))
  # ggplot(dd, aes(x = x, y = y, fill = factor(col))) + geom_tile()

# If nrows >> ncols Q looks more like an Identity matrix.
# If ncols >> nrows Q looks has nrows number of decaying blocks. 
