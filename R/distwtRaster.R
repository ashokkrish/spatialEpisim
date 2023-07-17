avg_euclidean_distance <- function(p, q, lambda)
{
  exp(-sqrt(sum((p - q)^2))/lambda)
}

wtd_nbrs_sum <- function(input_matrix, radius, lambda)
{
  temp_1 <- matrix(data = 0, nrow = nrow(x = input_matrix), ncol = radius)
  
  temp_2 <- matrix(data = 0, nrow = radius, ncol = ((2 * radius) + ncol(x = input_matrix)))
  
  input_matrix_modified <- rbind(temp_2, cbind(temp_1, input_matrix, temp_1), temp_2)
  
  #print(input_matrix_modified)
  #print(dim(input_matrix_modified))
  
  output_matrix <- matrix(nrow = nrow(x = input_matrix), ncol = ncol(x = input_matrix))
  
  #Generating the weight matrix
  weight_matrix <- matrix(0, nrow = 1 + 2*radius, ncol = 1 + 2*radius)
  
  for(i in seq_len(1 + 2*radius))
  {
    for(j in seq_len(1 + 2*radius))
    {
      weight_matrix[i,j] <- avg_euclidean_distance(c(i,j), c(radius+1, radius+1), lambda)
    }
  }
  
  #print(weight_matrix)
  
  for(i in seq_len(length.out = nrow(x = input_matrix)))
  {
    for(j in seq_len(length.out = ncol(x = input_matrix))) 
    {
      row_min <- (radius + (i - radius))
      row_max <- (radius + (i + radius))
      column_min <- (radius + (j - radius))
      column_max <- (radius + (j + radius))
      neighbours <- input_matrix_modified[(row_min:row_max), (column_min:column_max)]
      weighted_sum <- sum(neighbours * weight_matrix) # casewise multiplication
      output_matrix[i, j] <- weighted_sum
    }
  }
  return(output_matrix)
}