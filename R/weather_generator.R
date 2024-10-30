#' Generate Synthetic Rainfall Data
#'
#' This function generates synthetic rainfall data based on historical rainfall 
#' events and specified monthly parameters. It uses a specified statistical 
#' distribution (gamma or lognormal) to model the rainfall amounts.
#'
#' @param datain Either a path to a CSV file containing historical rainfall data 
#'               or a data frame. The data frame must include columns for 
#'               `Station_name`, `date`, `rfe`, `rainfall`, `lon`, and `lat`.
#' @param monthly_params_df A data frame containing monthly parameters for 
#'                          generating rainfall data. It should include columns 
#'                          for `Month`, `b0`, `b1`, `b0_rainyday`, `b1_rainyday`, 
#'                          `b0_dryday`, `b1_dryday`, `a0`, `a1`, `kappa`, 
#'                          `theta`, `p0`, `p0_rainyday`, and `p0_dryday`.
#' @param distribution_flag A string indicating the distribution to use for 
#'                          generating rainfall amounts. Options are 'gamma' 
#'                          (default) or 'lognormal'.
#' @param markovflag A binary indicator (default 1) for whether to use a Markov 
#'                   chain approach in determining the probability of rainfall. 
#'                   If set to 0, the function does not consider previous 
#'                   rainfall when calculating probabilities.
#'
#' @return A data frame containing the generated rainfall data, including columns 
#'         for `Station_name`, `date`, `lon`, `lat`, `rfe`, `original_rainfall`, 
#'         and `generated_rainfall`.
#'
#' @examples
#' # Example usage:
#' monthly_params <- data.frame(Month = 1:12, b0 = runif(12), b1 = runif(12), 
#'                               b0_rainyday = runif(12), b1_rainyday = runif(12),
#'                               b0_dryday = runif(12), b1_dryday = runif(12),
#'                               a0 = runif(12), a1 = runif(12), kappa = runif(12),
#'                               theta = runif(12), p0 = runif(12),
#'                               p0_rainyday = runif(12), p0_dryday = runif(12))
#' historical_data <- data.frame(Station_name = "Station A", date = Sys.Date(),
#'                                rfe = c(0, 1, 0), rainfall = c(0, 5, 0),
#'                                lon = 34.0, lat = -1.0)
#' generated_rainfall <- weather_generator(historical_data, monthly_params)
#' print(generated_rainfall)
#' 
weather_generator <- function(datain, monthly_params_df, distribution_flag = 'gamma', markovflag = 1) {
  # If datain is a file path, read the CSV; otherwise, assume it's a data frame
  df <- if (is.character(datain)) read.csv(datain) else datain
  
  generated_rainfall <- 0  # Initialise the generated rainfall
  generated_data <- list()  # Initialise list to store generated data
  
  # Loop through each row of the data
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    
    station_name <- row$Station_name
    date <- row$date
    rfe <- row$rfe
    original_rainfall <- row$rainfall
    month <- month(as.Date(date))  # Extract month from the date
    
    # Get monthly parameters for the specific month
    monthly_params <- monthly_params_df[monthly_params_df$Month == month, ]
    
    # Extract the parameters for the current month
    b0 <- monthly_params$b0
    b1 <- monthly_params$b1
    b0_rainyday <- monthly_params$b0_rainyday
    b1_rainyday <- monthly_params$b1_rainyday
    b0_dryday <- monthly_params$b0_dryday
    b1_dryday <- monthly_params$b1_dryday
    a0 <- monthly_params$a0
    a1 <- monthly_params$a1
    kappa <- monthly_params$kappa
    theta <- monthly_params$theta
    p0 <- monthly_params$p0
    p0_rainyday <- monthly_params$p0_rainyday
    p0_dryday <- monthly_params$p0_dryday
    
    if (is.na(rfe)) {
      # If rfe is NA, set generated rainfall to NA
      generated_rainfall <- NA
    } else {
      if (rfe < 0) {
        stop(paste("Negative RFE value (", rfe, ") found.", sep = ""))
      } else {
        # Calculate the probability of precipitation
        prob_precipitation <- if (rfe == 0) {
          if (markovflag == 0 || is.na(generated_rainfall)) {
            p0
          } else if (generated_rainfall == 0) {
            p0_dryday
          } else {
            p0_rainyday
          }
        } else {
          if (markovflag == 0 || is.na(generated_rainfall)) {
            1 / (1 + exp(-1 * (b0 + b1 * rfe)))
          } else if (generated_rainfall == 0) {
            1 / (1 + exp(-1 * (b0_dryday + b1_dryday * rfe)))
          } else {
            1 / (1 + exp(-1 * (b0_rainyday + b1_rainyday * rfe)))
          }
        }
      }
      
      # Clamp probability to [0, 1]
      prob_precipitation <- max(0, min(prob_precipitation, 1))
      
      # Determine if rainfall occurred (Bernoulli trial)
      rain_occurred <- rbinom(1, 1, prob_precipitation)
      
      # If rain occurred, calculate the amount
      # If rain occurred, calculate the amount
      if (rain_occurred == 1) {
        if (distribution_flag == 'gamma') {
          if (rfe > 0) {
            variance_rainfall <- kappa * (rfe ^ theta)
            mean_rainfall <- a0 + a1 * rfe
            # Gamma distribution parameters
            shape <- mean_rainfall^2 / variance_rainfall
            scale <- variance_rainfall / mean_rainfall
            
            # Sample from the Gamma distribution
            generated_rainfall <- rgamma(1, shape=shape, scale=scale)
            
          } else {
            variance_rainfall <- kappa * (0.1 ^ theta)
            mean_rainfall <- a0 + a1 * 0.1
            shape <- mean_rainfall^2 / variance_rainfall
            scale <- variance_rainfall / mean_rainfall
            
            generated_rainfall <- rgamma(1, shape=shape, scale=scale)
            #ECB note. In R the generated rainfall seems to blow up sometimes when rfe is low. This is a temporary fix
            
          }
          
        } else if (distribution_flag == 'lognormal') {
          mean_rainfall <- a0 + a1 * rfe
          variance_rainfall <- kappa * (rfe ^ theta)
          
          # Sample from the log-normal distribution
          generated_rainfall <- rlnorm(1, log(mean_rainfall), sqrt(variance_rainfall))
        }
      } else {
        generated_rainfall <- 0
      }
    }
    
    # Store the generated data in the list
    generated_data[[i]] <- list(
      Station_name = station_name,
      date = date,
      lon = row$lon,
      lat = row$lat,
      rfe = rfe,
      original_rainfall = original_rainfall,
      generated_rainfall = generated_rainfall
    )
  }
  
  # Convert the list to a data frame
  generated_df <- do.call(rbind, lapply(generated_data, as.data.frame))

  return(generated_df)
}
