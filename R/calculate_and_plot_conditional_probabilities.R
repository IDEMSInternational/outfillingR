#' Calculate and Plot Conditional Probabilities
#'
#' This function calculates conditional probabilities of rainfall based on
#' ranges of rainfall estimates (RFE) and generates statistical summaries for
#' each bin, including probability of rainy and dry days. Results are filtered
#' to exclude bins with a low number of observations.
#'
#' @param filtered_df A data frame containing rainfall data with columns "rainfall" and "rfe".
#' @param rfe_bin_edges A numeric vector defining the edges of RFE bins.
#' @param count_filter A numeric value specifying the minimum count required for a bin to be included in the results (default is 10).
#' @return A list containing:
#'   - `filtered_probabilities_df`: A data frame with conditional probabilities, mean and std of rainfall, and rainy/dry day probabilities by RFE bin.
#'   - `p0`: Probability of rainfall > 0 when RFE is zero.
#'   - `p0_rainyday`: Probability of rainfall > 0 for rainy days when RFE is zero.
#'   - `p0_dryday`: Probability of rainfall > 0 for dry days when RFE is zero.
#'   
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
  filtered_df <- filtered_df %>% dplyr::mutate(previous_day_rain = dplyr::lag(rainfall))

  # Calculate p0: Probability of rainfall > 0 when RFE is zero
  zero_rfe_df <- filtered_df %>% dplyr::filter(rfe == 0)
  
  total_zero_rfe_count <- nrow(zero_rfe_df)
  positive_rainfall_zero_rfe_count <- sum(zero_rfe_df$rainfall > 0, na.rm = TRUE)
  
  p0 <- ifelse(total_zero_rfe_count > 0, positive_rainfall_zero_rfe_count / total_zero_rfe_count, NA)
  
  # Calculate p0_rainyday and p0_dryday
  rainyday_zero_rfe_df <- zero_rfe_df %>% dplyr::filter(previous_day_rain > 0)
  dryday_zero_rfe_df <- zero_rfe_df %>% dplyr::filter(is.na(previous_day_rain) | previous_day_rain == 0)
  
  p0_rainyday <- ifelse(nrow(rainyday_zero_rfe_df) > 0, sum(rainyday_zero_rfe_df$rainfall > 0, na.rm = TRUE) / nrow(rainyday_zero_rfe_df), NA)
  p0_dryday <- ifelse(nrow(dryday_zero_rfe_df) > 0, sum(dryday_zero_rfe_df$rainfall > 0, na.rm = TRUE) / nrow(dryday_zero_rfe_df), NA)
  
  # Iterate over each rfe bin
  for (i in seq_along(rfe_bin_edges)[-length(rfe_bin_edges)]) {
    lower_bound <- rfe_bin_edges[i]
    upper_bound <- rfe_bin_edges[i + 1]
    
    # Filter rows within the current rfe bin
    bin_df <- filtered_df %>% dplyr::filter(rfe >= lower_bound & rfe < upper_bound)
    
    # Calculate the probability of rainfall > 0 in this bin
    total_count <- nrow(bin_df)
    positive_rainfall_count <- sum(bin_df$rainfall > 0, na.rm = TRUE)
    
    probability <- ifelse(total_count > 0, positive_rainfall_count / total_count, NA)
    average_rfe <- ifelse(total_count > 0, mean(bin_df$rfe, na.rm = TRUE), NA)
    rainfall_mean <- ifelse(total_count > 0, mean(bin_df$rainfall[bin_df$rainfall > 0], na.rm = TRUE), NA)
    rainfall_std <- ifelse(total_count > 0, sd(bin_df$rainfall[bin_df$rainfall > 0], na.rm = TRUE), NA)
    
    # Filter the bin data for rainy days and dry days
    rainyday_bin_df <- bin_df %>% dplyr::filter(previous_day_rain > 0)
    dryday_bin_df <- bin_df %>% dplyr::filter(is.na(previous_day_rain) | previous_day_rain == 0)
    
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
    dplyr::filter(Count > count_filter)
  
  return(list(filtered_probabilities_df = filtered_probabilities_df, p0 = p0, p0_rainyday = p0_rainyday, p0_dryday = p0_dryday))
}
