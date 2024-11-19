#' Fill Nearest Non-NA Values
#'
#' This function takes a vector and fills NA values with the nearest non-NA value.
#' It uses a data frame to facilitate filling both upwards and downwards to ensure
#' the closest available value is used to fill each NA.
#'
#' @param x A numeric vector that may contain NA values.
#'
#' @return A numeric vector with NA values filled by the nearest non-NA values.
#'         The original order of the vector is preserved.
fill_nearest <- function(x) {
  if (all(is.na(x))) {stop("At least one value in `x` cannot be missing.")}
  # Find the index of non-NA values
  non_na_idx <- which(!is.na(x))
  
  # Create a vector to store the filled values
  filled_x <- x
  
  # Loop through the indices where there are NA values
  for (i in which(is.na(x))) {
    # Find the index of the nearest non-NA value
    nearest_idx <- non_na_idx[which.min(abs(non_na_idx - i))]
    
    # Fill the NA with the nearest value
    filled_x[i] <- x[nearest_idx]
  }
  
  return(filled_x)
}