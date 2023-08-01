plotHQHt <- function(HQHt) {

  print(paste("Determinant of HQHt: ", det(HQHt)))

  x <- 1:ncol(HQHt)
  y <- 1:nrow(HQHt)
  
  # Create a grid of x and y values
  X <- matrix(x, nrow = nrow(HQHt), ncol = ncol(HQHt), byrow = TRUE)
  Y <- matrix(y, nrow = nrow(HQHt), ncol = ncol(HQHt), byrow = FALSE)
  
  library(plot3D)
  persp3D(x = X, y = Y, z = HQHt, theta = 90, expand = 0.5,
          xlab = "Columns", ylab = "Rows", scale = FALSE,
          colkey = list(side = 1))
  
  # Plot the 3D surface
  library(rgl)
  persp3d(X, Y, HQHt, col = "lightgreen", xlab = "Columns", ylab = "Rows", zlab = "Values",
          ticktype = "detailed", main = "3D Surface Plot of HQHt", xlim = c(0, nrow(HQHt)), ylim = c(0, ncol(HQHt)))
}

#--------------#
# Example Call #
#--------------#

# plotHQHt(HQHt)
# 
# plotHQHt(HQHt_easy)
