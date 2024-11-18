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
  
  # Initialise lists to store results
  rfe_bin_ranges <- conditional_probabilities <- average_rfe_values <- bin_counts <- 
    rainfall_means <- rainfall_stds <- probabilities_rainyday <- probabilities_dryday <- vector("list", length(rfe_bin_edges) - 1)
  
  # Calculate the 'previous_day_rain' column
  filtered_df <- filtered_df %>%
    dplyr::mutate(previous_day_rain = lag(rainfall))
  
  # Calculate p0: Probability of rainfall > 0 when RFE is zero
  zero_rfe_df <- filtered_df %>% dplyr::filter(rfe == 0)
  p0 <- mean(zero_rfe_df$rainfall > 0, na.rm = TRUE)
  
  # Calculate p0_rainyday and p0_dryday
  zero_rfe_df$previous_day_rain <- as.numeric(zero_rfe_df$previous_day_rain)
  p0_rainyday <- mean(zero_rfe_df$rainfall[zero_rfe_df$previous_day_rain > 0] > 0, na.rm = TRUE)
  p0_dryday <- mean(zero_rfe_df$rainfall[is.na(zero_rfe_df$previous_day_rain) | zero_rfe_df$previous_day_rain == 0] > 0, na.rm = TRUE)
  
  # Iterate over each rfe bin
  for (i in seq_len(length(rfe_bin_edges) - 1)) {
    lower_bound <- rfe_bin_edges[i]
    upper_bound <- rfe_bin_edges[i + 1]
    
    # Filter rows within the current rfe bin
    bin_df <- filtered_df %>% dplyr::filter(rfe >= lower_bound & rfe < upper_bound)
    total_count <- nrow(bin_df)
    
    # Calculate conditional probabilities and statistical summaries
    conditional_probabilities[[i]] <- mean(bin_df$rainfall > 0, na.rm = TRUE)
    average_rfe_values[[i]] <- mean(bin_df$rfe, na.rm = TRUE)
    rainfall_means[[i]] <- mean(bin_df$rainfall[bin_df$rainfall > 0], na.rm = TRUE)
    rainfall_stds[[i]] <- sd(bin_df$rainfall[bin_df$rainfall > 0], na.rm = TRUE)
    bin_counts[[i]] <- total_count
    
    # Calculate conditional probabilities for rainy and dry days within this RFE bin
    bin_df$previous_day_rain <- as.numeric(bin_df$previous_day_rain)
    probabilities_rainyday[[i]] <- mean(bin_df$rainfall[bin_df$previous_day_rain > 0] > 0, na.rm = TRUE)
    probabilities_dryday[[i]] <- mean(bin_df$rainfall[is.na(bin_df$previous_day_rain) | bin_df$previous_day_rain == 0] > 0, na.rm = TRUE)
    
    # Define bin range as a string
    rfe_bin_ranges[[i]] <- paste(lower_bound, upper_bound - 1, sep = "-")
  }
  
  # Combine results into a data frame
  probabilities_df <- data.frame(
    RFE_Bin_Range = unlist(rfe_bin_ranges),
    Conditional_Probability = unlist(conditional_probabilities),
    Average_RFE = unlist(average_rfe_values),
    Rainfall_Mean = unlist(rainfall_means),
    Rainfall_Std = unlist(rainfall_stds),
    Count = unlist(bin_counts),
    Probability_RainyDay = unlist(probabilities_rainyday),
    Probability_DryDay = unlist(probabilities_dryday)
  )
  
  # Filter the DataFrame to include only rows where 'Count' > count_filter
  filtered_probabilities_df <- probabilities_df %>%
    dplyr::filter(Count > count_filter)
  
  return(list(filtered_probabilities_df = filtered_probabilities_df, p0 = p0, p0_rainyday = p0_rainyday, p0_dryday = p0_dryday))
}
