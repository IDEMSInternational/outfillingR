#' Calibrate Kappa and Theta via Log-Log Linear Regression
#'
#' This function estimates the parameters kappa (k) and theta for rainfall estimation by performing
#' a linear regression on the log-transformed mean and variance of rainfall. Kappa (k) is derived
#' from the intercept, and theta from the slope of the regression line.
#'
#' @param filtered_probabilities_df A data frame containing columns `Rainfall_Mean` (mean rainfall) and
#' `Rainfall_Std` (standard deviation of rainfall).
#' @return A named list with `k` (kappa) and `theta` values, or `NA` for both if there are fewer than 4 data points.
calibrate_kappa_theta <- function(filtered_probabilities_df) {
  # Retrieve mean and variance
  rainfall_means <- filtered_probabilities_df$Rainfall_Mean
  rainfall_stds <- filtered_probabilities_df$Rainfall_Std
  
  if (any(rainfall_means < 0)){ stop("Rainfall mean values less than 0")}
  if (any(rainfall_stds < 0)){ stop("Rainfall SD values less than 0")}
  
  # Log-transform: log(variance) vs log(mean)
  log_mean_rainfall <- log(rainfall_means)
  log_variance_rainfall <- log(rainfall_stds^2)
  
  # Only perform regression if we have more than 3 data points
  if (length(log_mean_rainfall) > 3) {
    # Linear regression: log(variance) ~ log(mean)
    fit <- lm(log_variance_rainfall ~ log_mean_rainfall)
    
    # Extract slope (theta) and intercept (ln_k)
    theta <- coef(fit)[2]
    ln_k <- coef(fit)[1]
    
    # Calculate kappa (k) from ln(k)
    k <- exp(ln_k)
    
    return(list(k = k, theta = theta))
  } else {
    # Insufficient data points; return NA for k and theta
    return(list(k = NA, theta = NA))
  }
}