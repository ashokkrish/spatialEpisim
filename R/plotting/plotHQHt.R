plotHQHt <- function(HQHt) {
  message(paste("Determinant of HQHt =", det(HQHt)))

  x <- 1:ncol(HQHt)
  y <- 1:nrow(HQHt)

  # Create a grid of x and y values
  X <- matrix(x, nrow = nrow(HQHt), ncol = ncol(HQHt), byrow = TRUE)
  Y <- matrix(y, nrow = nrow(HQHt), ncol = ncol(HQHt), byrow = FALSE)

  plot3D::persp3D(x = X, y = Y, z = HQHt, theta = 90, expand = 0.5,
                  xlab = "Columns", ylab = "Rows", main = "Plot of HQHt", scale = FALSE,
                  colkey = list(side = 1))
}
