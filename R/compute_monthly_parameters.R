#' Compute Monthly Parameters
#'
#' Calculates monthly parameters based on rainfall data from a CSV file.
#'
#' @param filein A string representing the input CSV file path.
#' @param custom_bins A numeric vector specifying custom bins for RFE.
#' @param count_filter A numeric threshold for filtering counts.
#' @param min_rainy_days_threshold A numeric threshold for minimum rainy days.
#' @return A data frame with the computed monthly parameters.
compute_monthly_parameters <- function(filein, custom_bins = c(1, 3, 5, 10, 15), count_filter = 10, min_rainy_days_threshold = 10) {
  # Load the CSV data
  df <- read.csv(filein)
  
  # Initialise lists to store results
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
    } else {
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