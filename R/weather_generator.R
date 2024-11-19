#' Generate Synthetic Rainfall Data
#'
#' This function generates synthetic rainfall data using historical rainfall 
#' data and monthly parameters. It applies a specified statistical distribution 
#' (gamma or lognormal) to model rainfall amounts, optionally using a Markov 
#' chain to simulate the probability of precipitation based on past conditions.
#'
#' @param data Either a path to a CSV file containing historical rainfall data 
#'             or a data frame.
#' @param station A string specifying the column name in `data` that contains 
#'                the station names.
#' @param date A string specifying the column name in `data` that contains 
#'             the date values.
#' @param rfe A string specifying the column name in `data` that contains 
#'            rainfall estimates (RFE) used as predictors for rainfall generation.
#' @param rainfall A string specifying the column name in `data` that contains 
#'                 the original rainfall values.
#' @param metadata An optional data frame containing additional metadata to merge 
#'                 with the historical data. Default is `NULL`.
#' @param metadata_station A string specifying the column name in the metadata 
#'                         data frame that corresponds to `station`. 
#'                         If `NULL`, it defaults to the value of `station`.
#' @param lon A string specifying the column corresponding to longitude values.
#'            If `metadata` is `NULL`, this column is from `data`, otherwise
#'            this is from `metadata`
#' @param lat A string specifying the column corresponding to latitude values.
#'            If `metadata` is `NULL`, this column is from `data`, otherwise
#'            this is from `metadata`
#' @param monthly_params_df A data frame containing monthly parameters for 
#'                          rainfall generation. This must include columns 
#'                          for `Month`, `b0`, `b1`, `b0_rainyday`, `b1_rainyday`, 
#'                          `b0_dryday`, `b1_dryday`, `a0`, `a1`, `kappa`, 
#'                          `theta`, `p0`, `p0_rainyday`, and `p0_dryday`.
#' @param distribution_flag A string specifying the statistical distribution 
#'                          to use for generating rainfall amounts. Options 
#'                          are `"gamma"` (default) or `"lognormal"`.
#' @param markovflag A logical value indicating whether to use a Markov chain 
#'                   approach for rainfall occurrence. If `TRUE` (default), the 
#'                   probability of rainfall depends on previous conditions.
#'
#' @return A data frame containing the generated synthetic rainfall data with 
#'         the following columns:
#'         - `Station_name`: The station name.
#'         - `date`: The date.
#'         - `lon`: Longitude of the station.
#'         - `lat`: Latitude of the station.
#'         - `rfe`: Rainfall estimates (RFE) used in the generation process.
#'         - `original_rainfall`: The original rainfall value from `data`.
#'         - `generated_rainfall`: The generated synthetic rainfall value.
weather_generator <- function(data,
                              station,
                              date,
                              rfe,
                              rainfall,
                              metadata = NULL,
                              metadata_station = NULL,
                              lon,
                              lat,
                              monthly_params_df,
                              distribution_flag = 'gamma',
                              markovflag = TRUE) {
  
  # If data is a file path, read the CSV; otherwise, assume it's a data frame
  df <- if (is.character(data)) utils::read.csv(data) else data
  
  generated_rainfall <- 0  # Initialise the generated rainfall
  generated_data <- list()  # Initialise list to store generated data
  
  if (!is.null(metadata)) {
    if (is.null(metadata_station)) metadata_station = station
    df <- dplyr::full_join(df, metadata, by = stats::setNames(metadata_station, station))
  }
  
  # Loop through each row of the data
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    
    station_name <- row[[station]]
    date_col <- row[[date]]
    rfe_col <- row[[rfe]]
    original_rainfall <- row[[rainfall]]
    month <- lubridate::month(as.Date(date_col))  # Extract month from the date
    
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
    
    if (is.na(rfe_col)) {
      # If rfe_col is NA, set generated rainfall to NA
      generated_rainfall <- NA
    } else {
      if (rfe_col < 0) {
        stop(paste("Negative RFE value (", rfe_col, ") found.", sep = ""))
      } else {
        # Calculate the probability of precipitation
        prob_precipitation <- if (rfe_col == 0) {
          if (markovflag == FALSE || is.na(generated_rainfall)) {
            p0
          } else if (generated_rainfall == 0) {
            p0_dryday
          } else {
            p0_rainyday
          }
        } else {
          if (markovflag == FALSE || is.na(generated_rainfall)) {
            1 / (1 + exp(-1 * (b0 + b1 * rfe_col)))
          } else if (generated_rainfall == 0) {
            1 / (1 + exp(-1 * (b0_dryday + b1_dryday * rfe_col)))
          } else {
            1 / (1 + exp(-1 * (b0_rainyday + b1_rainyday * rfe_col)))
          }
        }
      }
      
      # Clamp probability to [0, 1]
      prob_precipitation <- max(0, min(prob_precipitation, 1))
      
      # Determine if rainfall occurred (Bernoulli trial)
      rain_occurred <- stats::rbinom(1, 1, prob_precipitation)
      
      # If rain occurred, calculate the amount
      if (rain_occurred == 1) {
        if (distribution_flag == 'gamma') {
          if (rfe_col > 0) {
            variance_rainfall <- kappa * (rfe_col ^ theta)
            mean_rainfall <- a0 + a1 * rfe_col
            # Gamma distribution parameters
            shape <- mean_rainfall^2 / variance_rainfall
            scale <- variance_rainfall / mean_rainfall
            
            # Sample from the Gamma distribution
            generated_rainfall <- stats::rgamma(1, shape=shape, scale=scale)
            
          } else {
            variance_rainfall <- kappa * (0.1 ^ theta)
            mean_rainfall <- a0 + a1 * 0.1
            shape <- mean_rainfall^2 / variance_rainfall
            scale <- variance_rainfall / mean_rainfall
            
            generated_rainfall <- stats::rgamma(1, shape=shape, scale=scale)
            #ECB note. In R the generated rainfall seems to blow up sometimes when rfe_col is low. This is a temporary fix
            
          }
          
        } else if (distribution_flag == 'lognormal') {
          mean_rainfall <- a0 + a1 * rfe_col
          variance_rainfall <- kappa * (rfe_col ^ theta)
          
          # Sample from the log-normal distribution
          generated_rainfall <- stats::rlnorm(1, log(mean_rainfall), sqrt(variance_rainfall))
        }
      } else {
        generated_rainfall <- 0
      }
    }
    
    # Store the generated data in the list
    generated_data[[i]] <- list(
      station_name = station_name,
      date = date_col,
      lon = row[[lon]],
      lat = row[[lat]],
      rfe = rfe_col,
      original_rainfall = original_rainfall,
      generated_rainfall = generated_rainfall
    )
  }
  
  # Convert the list to a data frame
  generated_df <- do.call(rbind, lapply(generated_data, as.data.frame))

  return(generated_df)
}
