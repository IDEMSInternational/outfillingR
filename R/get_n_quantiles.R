#' Get specified number of quantiles from a variable.
#'
#' This function calculates and returns the specified number of quantiles of a numeric vector.
#'
#' @param x A numeric vector.
#' @param n_quantiles The number of quantiles to calculate.
#' @param na.rm A logical value indicating whether NA values should be removed before computation. Defaults to FALSE.
#' @return A named numeric vector of quantiles.
#' @examples
#' data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' get_n_quantiles(data, 4) # Quartiles
#' get_n_quantiles(data, 5) # Quintiles
#' get_n_quantiles(c(1,2,NA,3,4), 4, na.rm = TRUE)
get_n_quantiles <- function(x, n_quantiles, na.rm = FALSE) {
  if (!is.numeric(x)) {
    stop("Input 'x' must be a numeric vector.")
  }
  if (!is.numeric(n_quantiles) || n_quantiles < 1 || floor(n_quantiles) != n_quantiles) {
    stop("n_quantiles must be a positive integer.")
  }
  if (!is.logical(na.rm)) {
    stop("na.rm must be logical.")
  }
  
  probs <- c(seq(1 / n_quantiles, (n_quantiles - 1) / n_quantiles, 1 / n_quantiles), 1)
  quantiles <- quantile(x, probs = probs, na.rm = na.rm)
  return(quantiles)
}