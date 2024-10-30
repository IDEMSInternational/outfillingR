#' Calculate Logistic Model Coefficients (b0, b1)
#'
#' Estimates the coefficients b0 and b1 for a logistic model based on mean rainfall estimate (RFE)
#' and observed conditional probabilities of rain. Returns 0 for both coefficients if the total count of rainy days is below
#' the specified threshold or if there are insufficient data points.
#'
#' @param mean_rfe A numeric vector representing mean RFE values for each bin.
#' @param conditional_prob_rain A numeric vector representing conditional probabilities of rain for each bin.
#' @param bin_counts A numeric vector indicating the count of observations in each RFE bin.
#' @param min_rainy_days_threshold A numeric threshold for the minimum required total count of rainy days across bins.
#' @return A named numeric vector containing `b0` and `b1` logistic model coefficients, or `NA` values if model fitting fails.
calculate_b0_b1 <- function(mean_rfe, conditional_prob_rain, bin_counts, min_rainy_days_threshold) {
  
  # If there are insufficient rainy days across bins, return b0 and b1 as 0
  if (sum(bin_counts) < min_rainy_days_threshold) {
    return(c(b0 = 0, b1 = 0))
  }
  
  # If there are fewer than 2 data points, return NA for b0 and b1
  if (length(mean_rfe) < 2) {
    return(c(b0 = NA, b1 = NA))
  }
  
  tryCatch({
    # Fit logistic model using non-linear least squares
    fit <- nls(conditional_prob_rain ~ logistic_function(mean_rfe, b0, b1),
               start = list(b0 = 0, b1 = 0), control = list(maxiter = 10000))
    
    # Extract coefficients b0 and b1
    coef_fit <- coef(fit)
    b0 <- coef_fit['b0']
    b1 <- coef_fit['b1']
    
    # Set b0 and b1 to NA if b0 is infeasible (e.g., b0 > 1)
    if (b0 > 1) {
      return(c(b0 = NA, b1 = NA))
    }
    
    return(coef_fit)
  }, error = function(e) {
    # Return NA for b0 and b1 if fitting fails
    return(c(b0 = NA, b1 = NA))
  })
}
