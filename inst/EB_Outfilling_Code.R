# Load necessary libraries
library(dplyr)
library(lubridate)
library(dplyr)


# Read the CSV file into a dataframe
select_calibration_data <- function(file_path, rainfall_estimate_column,station_to_exclude) {
  
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  
  
  # Copy data from the selected column to the RFE column
  data <- data %>%
    mutate(rfe = get(rainfall_estimate_column))
  
  #ECB note. Referring to the python code, we are only going to create the calibration for calibration option 2 (i.e. including no data from the station in question). This is to avoid the code becoming too complicated. We can add functionality for the other options later.
  
  
  calibration_data <- data %>%
    filter(Station_name != station_to_exclude)
  
  
  
  write.csv(calibration_data, "calibration_data.csv", row.names = FALSE)
}

#calibration_data$date <- as.Date(calibration_data$date, format = "%Y-%m-%d")

#ECB note. This is the point at which we would loop over all months.
# Extract all January values into a new dataframe
#filtered_df <- calibration_data %>%
#  filter(month(date) == 1)  # 1 corresponds to January

# View the first few rows of the filtered dataframe

# Load necessary libraries


calculate_and_plot_conditional_probabilities <- function(filtered_df, rfe_bin_edges, count_filter = 10) {
  # Initialize lists to store results
  rfe_bin_ranges <- c()
  conditional_probabilities <- c()
  average_rfe_values <- c()
  bin_counts <- c()
  rainfall_means <- c()
  rainfall_stds <- c()
  
  # Lists for rainy and dry day probabilities
  probabilities_rainyday <- c()
  probabilities_dryday <- c()
  
  # Calculate the 'previous_day_rain' column
  filtered_df <- filtered_df %>%
    mutate(previous_day_rain = dplyr::lag(rainfall))
  
  # Calculate p0: Conditional probability of rainfall > 0 when RFE is zero
  zero_rfe_df <- filtered_df %>%
    filter(rfe == 0)
  total_zero_rfe_count <- nrow(zero_rfe_df)
  positive_rainfall_zero_rfe_count <- sum(zero_rfe_df$rainfall > 0, na.rm = TRUE)
  
  p0 <- ifelse(total_zero_rfe_count > 0, positive_rainfall_zero_rfe_count / total_zero_rfe_count, NA)
  
  # Calculate p0_rainyday and p0_dryday
  rainyday_zero_rfe_df <- zero_rfe_df %>%
    filter(previous_day_rain > 0)
  dryday_zero_rfe_df <- zero_rfe_df %>%
    filter(is.na(previous_day_rain) | previous_day_rain == 0)
  
  p0_rainyday <- ifelse(nrow(rainyday_zero_rfe_df) > 0,
                        sum(rainyday_zero_rfe_df$rainfall > 0, na.rm = TRUE) / nrow(rainyday_zero_rfe_df), NA)
  p0_dryday <- ifelse(nrow(dryday_zero_rfe_df) > 0,
                      sum(dryday_zero_rfe_df$rainfall > 0, na.rm = TRUE) / nrow(dryday_zero_rfe_df), NA)
  
  # Iterate over each rfe bin
  for (i in seq_along(rfe_bin_edges)[-length(rfe_bin_edges)]) {
    # Define the range for the current bin
    lower_bound <- rfe_bin_edges[i]
    upper_bound <- rfe_bin_edges[i + 1]
    
    # Filter rows within the current rfe bin
    bin_df <- filtered_df %>%
      filter(rfe >= lower_bound & rfe < upper_bound)
    
    # Calculate the probability of rainfall > 0 in this bin
    total_count <- nrow(bin_df)
    positive_rainfall_count <- sum(bin_df$rainfall > 0, na.rm = TRUE)
    
    probability <- ifelse(total_count > 0, positive_rainfall_count / total_count, NA)
    average_rfe <- ifelse(total_count > 0, mean(bin_df$rfe, na.rm = TRUE), NA)
    rainfall_mean <- ifelse(total_count > 0, mean(bin_df$rainfall[bin_df$rainfall > 0], na.rm = TRUE), NA)
    rainfall_std <- ifelse(total_count > 0, sd(bin_df$rainfall[bin_df$rainfall > 0], na.rm = TRUE), NA)
    
    # Filter the bin data for rainy days and dry days
    rainyday_bin_df <- bin_df %>%
      filter(previous_day_rain > 0)
    dryday_bin_df <- bin_df %>%
      filter(is.na(previous_day_rain) | previous_day_rain == 0)
    
    # Calculate the probability of rainfall for rainy days within this RFE bin
    probability_rainyday <- ifelse(nrow(rainyday_bin_df) > 0,
                                   sum(rainyday_bin_df$rainfall > 0, na.rm = TRUE) / nrow(rainyday_bin_df), NA)
    
    # Calculate the probability of rainfall for dry days within this RFE bin
    probability_dryday <- ifelse(nrow(dryday_bin_df) > 0,
                                 sum(dryday_bin_df$rainfall > 0, na.rm = TRUE) / nrow(dryday_bin_df), NA)
    
    # Store the results
    rfe_bin_ranges <- c(rfe_bin_ranges, paste(lower_bound, upper_bound - 1, sep = "-"))
    conditional_probabilities <- c(conditional_probabilities, probability)
    average_rfe_values <- c(average_rfe_values, average_rfe)
    bin_counts <- c(bin_counts, total_count)
    rainfall_means <- c(rainfall_means, rainfall_mean)
    rainfall_stds <- c(rainfall_stds, rainfall_std)
    probabilities_rainyday <- c(probabilities_rainyday, probability_rainyday)
    probabilities_dryday <- c(probabilities_dryday, probability_dryday)
  }
  
  # Create a DataFrame for the results
  probabilities_df <- data.frame(
    RFE_Bin_Range = rfe_bin_ranges,
    Conditional_Probability = conditional_probabilities,
    Average_RFE = average_rfe_values,
    Rainfall_Mean = rainfall_means,
    Rainfall_Std = rainfall_stds,
    Count = bin_counts,
    Probability_RainyDay = probabilities_rainyday,
    Probability_DryDay = probabilities_dryday
  )
  
  # Filter the DataFrame to include only rows where 'Count' > count_filter
  filtered_probabilities_df <- probabilities_df %>%
    filter(Count > count_filter)
  
  return(list(filtered_probabilities_df = filtered_probabilities_df, p0 = p0, p0_rainyday = p0_rainyday, p0_dryday = p0_dryday))
}


# Define the logistic function
logistic_function <- function(rfe, b0, b1) {
  return(1 / (1 + exp(-(b0 + b1 * rfe))))
}

# Function to calculate b0 and b1
calculate_b0_b1 <- function(mean_rfe, conditional_prob_rain, bin_counts, min_rainy_days_threshold) {
  # If there are insufficient rainy days in any bin, return b0 and b1 as 0
  if (sum(bin_counts) < min_rainy_days_threshold) {
    return(c(b0 = 0, b1 = 0))
  }
  
  # If there are fewer than 2 data points, return NA for b0 and b1
  if (length(mean_rfe) < 2) {
    return(c(b0 = NA, b1 = NA))
  }
  
  tryCatch({
    # Fit the logistic model using non-linear least squares
    fit <- nls(conditional_prob_rain ~ logistic_function(mean_rfe, b0, b1),
               start = list(b0 = 0, b1 = 0), control = list(maxiter = 10000))
    
    # Extract the coefficients b0 and b1
    coef_fit <- coef(fit)
    b0 <- coef_fit['b0']
    b1 <- coef_fit['b1']
    
    # Set b0 and b1 to NA if b0 > 1 (infeasible)
    if (b0 > 1) {
      return(c(b0 = NA, b1 = NA))
    }
    
    return(coef_fit)
  }, error = function(e) {
    # If fitting fails, return NA for b0 and b1
    return(c(b0 = NA, b1 = NA))
  })
}



# Function to perform linear regression in log-log space and estimate kappa and theta
calibrate_kappa_theta <- function(filtered_probabilities_df) {
  # Get the mean rainfall and the variance (rainfall_std^2)
  rainfall_means <- filtered_probabilities_df$Rainfall_Mean
  rainfall_stds <- filtered_probabilities_df$Rainfall_Std
  
  # Log-log transform: log(variance) vs log(mean)
  log_mean_rainfall <- log(rainfall_means)
  log_variance_rainfall <- log(rainfall_stds^2)
  
  # Only perform regression if we have more than 3 data points
  if (length(log_mean_rainfall) > 3) {
    # Perform linear regression: log(variance) ~ log(mean)
    fit <- lm(log_variance_rainfall ~ log_mean_rainfall)
    
    # Extract slope (theta) and intercept (ln_k)
    theta <- coef(fit)[2]
    ln_k <- coef(fit)[1]
    
    # Calculate k (kappa) from ln(k)
    k <- exp(ln_k)
    
    # Return k and theta
    return(list(k = k, theta = theta))
  } else {
    # If not enough data points, return NA for k and theta
    return(list(k = NA, theta = NA))
  }
}

extract_rows_by_date_range_across_years <- function(df, start_date, end_date) {
  # Ensure 'date' column is in Date format
  df$date <- as.Date(df$date, format = "%Y-%m-%d")
  
  # Remove rows where 'date' could not be parsed
  df <- df[!is.na(df$date), ]
  
  # Extract month and day from the date
  df$month_day <- format(df$date, "%m-%d")
  
  # Convert start_date and end_date to "MM-DD" format
  start_date <- format(as.Date(paste0("2021-", start_date)), "%m-%d")  # 2021 used as reference year
  end_date <- format(as.Date(paste0("2021-", end_date)), "%m-%d")      # 2021 used as reference year
  
  # Filter rows based on the date range across all years
  if (start_date <= end_date) {
    mask <- (df$month_day >= start_date) & (df$month_day <= end_date)
  } else {  # Handle the case where the range wraps around the end of the year
    mask <- (df$month_day >= start_date) | (df$month_day <= end_date)
  }
  
  # Filter the dataframe
  filtered_df <- df[mask, ]
  
  # Drop the 'month_day' column as it's no longer needed
  filtered_df$month_day <- NULL
  
  return(filtered_df)
}



compute_monthly_parameters <- function(filein, custom_bins = c(1, 3, 5, 10, 15), count_filter = 10, min_rainy_days_threshold = 10) {
  # Load the CSV data
  df <- read.csv(filein)
  
  # Initialize lists to store results
  months <- c()
  b0_values <- c()
  b1_values <- c()
  b0_rainyday_values <- c()
  b1_rainyday_values <- c()
  b0_dryday_values <- c()
  b1_dryday_values <- c()
  a0_values <- c()
  a1_values <- c()
  kappa_values <- c()
  theta_values <- c()
  p0_dryday_values <- c()
  p0_rainyday_values <- c()
  p0_values_orig <- c()
  
  # Loop through each month (January to December)
  for (month in 1:12) {
    # Get the correct number of days in the current month (for any reference year, e.g., 2021)
    last_day <- as.numeric(format(as.Date(paste0("2021-", month, "-01")) + months(1) - days(1), "%d"))
    
    # Format the start and end dates for the current month
    start_date <- sprintf("%02d-01", month)
    end_date <- sprintf("%02d-%02d", month, last_day)
    
    # Extract data for the current month
    filtered_df <- extract_rows_by_date_range_across_years(df, start_date, end_date)
    filtered_df <- tibble::tibble(filtered_df)
    # Calculate conditional probabilities and other statistics
    result_df <- calculate_and_plot_conditional_probabilities(filtered_df, custom_bins, count_filter)
    #print(month)
    #print(result_df)
    months <- c(months, month)
    if (nrow(result_df$filtered_probabilities_df) > 3){
      # Initialize variables
      b0 <- NA
      b1 <- NA
      b0_rainyday <- NA
      b1_rainyday <- NA
      b0_dryday <- NA
      b1_dryday <- NA
      a0 <- NA
      a1 <- NA
      kappa <- NA
      theta <- NA
      p0 <- NA
      p0_rainyday <- NA
      p0_dryday <- NA
      
      p0 <- result_df$p0
      p0_rainyday <- result_df$p0_rainyday
      p0_dryday <- result_df$p0_dryday
      
      # Calculate the linear model parameters (b0, b1) if data is sufficient
      
      xvals <- result_df$filtered_probabilities_df$Average_RFE
      yvals <- result_df$filtered_probabilities_df$Rainfall_Mean
      
      # Perform linear regression for a0 and a1
      lm_model <- lm(yvals ~ xvals)
      a0 <- max(0, coef(lm_model)[1])
      a1 <- coef(lm_model)[2]
      
      # Calculate b0 and b1
      #result_df$mean_rfe <- result_df$filtered_probabilities_df$Average_RFE
      #result_df$conditional_prob_rain <- result_df$filtered_probabilities_df$Conditional_Probability
      #result_df$rainy_days <- result_df$filtered_probabilities_df$Count
      #b0_b1 <- calculate_b0_b1(result_df, 100)
      #b0 <- b0_b1[1]
      #b1 <- b0_b1[2]
      
      rainyday_rfe <- result_df$filtered_probabilities_df$Average_RFE
      rainyday_prob <- result_df$filtered_probabilities_df$Probability_RainyDay
      rainyday_counts <- result_df$filtered_probabilities_df$Count
      
      # Calculate b0 and b1 for rainy days
      b0_b1_rainy <- calculate_b0_b1(rainyday_rfe, rainyday_prob, rainyday_counts, min_rainy_days_threshold)
      b0_rainyday <- b0_b1_rainy[1]
      b1_rainyday <- b0_b1_rainy[2]
      #This is now working. Apply to dry days
      
      # Calculate b0 and b1 for dry days
      dryday_rfe <- result_df$filtered_probabilities_df$Average_RFE
      dryday_prob <- result_df$filtered_probabilities_df$Probability_DryDay
      dryday_counts <- result_df$filtered_probabilities_df$Count
      
      b0_b1_dry <- calculate_b0_b1(dryday_rfe, dryday_prob, dryday_counts, min_rainy_days_threshold)
      b0_dryday <- b0_b1_dry[1]
      b1_dryday <- b0_b1_dry[2]
      
      # Calculate b0 and b1 for all days
      allday_rfe <- result_df$filtered_probabilities_df$Average_RFE
      allday_prob <- result_df$filtered_probabilities_df$Conditional_Probability
      allday_counts <- result_df$filtered_probabilities_df$Count
      
      b0_b1_all <- calculate_b0_b1(allday_rfe, allday_prob, allday_counts, min_rainy_days_threshold)
      b0 <- c(b0_b1_all[1])
      b1 <- c(b0_b1_all[2])
      
      
      
      
      # Handle NaN cases for rainy and dry days
      #if (is.na(b0_dryday) || is.na(b0_rainyday) || is.na(b1_dryday) || is.na(b1_rainyday)) {
      #  b0_rainyday <- b0
      #  b1_rainyday <- b1
      #  b0_dryday <- b0
      #  b1_dryday <- b1
      #}
      
      # Compute kappa and theta
      kappa_theta <- calibrate_kappa_theta(result_df$filtered_probabilities_df)
      kappa <- kappa_theta$k
      theta <- kappa_theta$theta
      
      
      
      # Store the results
      
      cat("Data for month:", month, "\n")
      b0_values <- c(b0_values, b0)
      b1_values <- c(b1_values, b1)
      a0_values <- c(a0_values, a0)
      a1_values <- c(a1_values, a1)
      b0_rainyday_values <- c(b0_rainyday_values, b0_rainyday)
      b1_rainyday_values <- c(b1_rainyday_values, b1_rainyday)
      b0_dryday_values <- c(b0_dryday_values, b0_dryday)
      b1_dryday_values <- c(b1_dryday_values, b1_dryday)
      
      kappa_values <- c(kappa_values, kappa)
      theta_values <- c(theta_values, theta)
      p0_values_orig <- c(p0_values_orig, p0)
      p0_rainyday_values <- c(p0_rainyday_values, p0_rainyday)
      p0_dryday_values <- c(p0_dryday_values, p0_dryday)
    }
    
    
    else {
      # Print a message and assign NA values for the month
      cat("No data for month:", month, "\n")
      b0_values <- c(b0_values, NA)
      b1_values <- c(b1_values, NA)
      a0_values <- c(a0_values, NA)
      a1_values <- c(a1_values, NA)
      b0_rainyday_values <- c(b0_rainyday_values, NA)
      b1_rainyday_values <- c(b1_rainyday_values, NA)
      b0_dryday_values <- c(b0_dryday_values, NA)
      b1_dryday_values <- c(b1_dryday_values, NA)
      kappa_values <- c(kappa_values, NA)
      theta_values <- c(theta_values, NA)
      p0_values_orig <- c(p0_values_orig, NA)
      p0_rainyday_values <- c(p0_rainyday_values, NA)
      p0_dryday_values <- c(p0_dryday_values, NA)
    }
  }
  
  # Create a dataframe with the results
  results_df <- data.frame(
    Month = months,
    b0 = b0_values,
    b1 = b1_values,
    a0 = a0_values,
    a1 = a1_values,
    b0_rainyday = b0_rainyday_values,
    b1_rainyday = b1_rainyday_values,
    b0_dryday = b0_dryday_values,
    b1_dryday = b1_dryday_values,
    kappa = kappa_values,
    theta = theta_values,
    p0 = p0_values_orig,
    p0_rainyday = p0_rainyday_values,
    p0_dryday = p0_dryday_values
  )
  
  return(results_df)
}

fill_nearest <- function(x) {
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

weather_generator <- function(datain, monthly_params_df, distribution_flag = 'gamma', markovflag = 1) {
  set.seed(1)
  
  # Load necessary libraries
  if (!require(lubridate)) install.packages("lubridate")
  if (!require(MASS)) install.packages("MASS")
  library(lubridate)
  
  
  # If datain is a file path, read the CSV, otherwise assume it's a dataframe
  
  if (is.character(datain)) {
    df <- read.csv(datain)
  } else {
    df <- datain
  }
  
  generated_rainfall <- 0  # Initialising the generated rainfall
  generated_data <- list()  # Initializing list to store generated data
  
  # Loop through each row of the data
  for (i in 1:nrow(df)) {
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
      # Calculate the probability of precipitation
      if (rfe == 0) {
        if (markovflag == 0 || is.na(generated_rainfall)) {
          prob_precipitation <- p0
        } else if (generated_rainfall == 0) {
          prob_precipitation <- p0_dryday
        } else {
          prob_precipitation <- p0_rainyday
        }
      } else if (rfe > 0) {
        if (markovflag == 0 || is.na(generated_rainfall)) {
          prob_precipitation <- 1 / (1 + exp(-1 * (b0 + b1 * rfe)))
        } else if (generated_rainfall == 0) {
          prob_precipitation <- 1 / (1 + exp(-1 * (b0_dryday + b1_dryday * rfe)))
        } else {
          prob_precipitation <- 1 / (1 + exp(-1 * (b0_rainyday + b1_rainyday * rfe)))
        }
      } else {
        stop(paste("Negative RFE value (", rfe, ") found.", sep = ""))
      }
      
      # Clamp probability to [0, 1]
      prob_precipitation <- max(0, min(prob_precipitation, 1))
      
      # Determine if rainfall occurred (Bernoulli trial)
      rain_occurred <- rbinom(1, 1, prob_precipitation)
      
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
      generated_rainfall <- round(generated_rainfall, 1)
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
  
  
  # Convert the list to a dataframe
  generated_df <- do.call(rbind, lapply(generated_data, as.data.frame))
  
  return(generated_df)
}




file_path <- "C:/Users/lclem/Downloads/merged_allfields (1).csv"
station_to_exclude <- "PETAUKE MET"
rainfall_estimate_column <- "chirps"

select_calibration_data(file_path, rainfall_estimate_column, station_to_exclude)

custom_bins <- c(1, 3, 5, 10, 15, 20)  # Custom RFE bins
count_filter <- 10                   # Minimum count of values in each bin to include in the calculations
min_rainy_days_threshold <- 50
filein <- 'calibration_data.csv'
# Call the function to compute monthly parameters
#dry_season_params <- compute_seasonal_parameters_no_markov(filein, '04-01','09-30',c(0.5,2,4,6,8,10,20), count_filter, min_rainy_days_threshold)
dry_season_params <- list(
  b0=-0.2,
  b1=0.05,
  a0=0.1,
  a1=1,
  b0_dryday=-0.2,
  b0_rainyday=-0.2,
  b1_dryday=0.05,
  b1_rainyday=0.05,
  kappa=2,
  theta=2,
  p0=0.001,
  p0_rainyday=0.001,
  p0_dryday=0.001
)

monthly_parameters <- compute_monthly_parameters(filein, custom_bins, count_filter, min_rainy_days_threshold)

target_months <- 5:9 #User defined dry season

# Replace the values in monthly_parameters for the target months with dry_season_params values
monthly_parameters[target_months, ] <- list(
  Month = target_months,
  b0 = dry_season_params$b0,
  b1 = dry_season_params$b1,
  a0 = dry_season_params$a0,
  a1 = dry_season_params$a1,
  b0_rainyday = dry_season_params$b0_rainyday,
  b1_rainyday = dry_season_params$b1_rainyday,
  b0_dryday = dry_season_params$b0_dryday,
  b1_dryday = dry_season_params$b1_dryday,
  kappa = dry_season_params$kappa,
  theta = dry_season_params$theta,
  p0 = dry_season_params$p0,
  p0_rainyday = dry_season_params$p0_rainyday,
  p0_dryday = dry_season_params$p0_dryday
)

# Check the updated monthly_parameters to ensure values were correctly substituted

datain <- read.csv("C:/Users/lclem/Downloads/merged_allfields (1).csv")
generated_weather <- weather_generator(
  datain = datain,
  monthly_params_df = monthly_parameters,
  distribution_flag = 'gamma',  # Choose 'gamma' or 'lognormal'
  markovflag = 1  # Use Markov chain (1 for yes, 0 for no)
)
write.csv(generated_weather,'r_test_rparams.csv')


# View the generated data
#print(generated_weather)
