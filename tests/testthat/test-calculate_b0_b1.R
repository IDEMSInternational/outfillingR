test_that("calculate_b0_b1 returns (0, 0) when rainy days are below threshold", {
  # Mock data
  mean_rfe <- c(1, 2, 3)
  conditional_prob_rain <- c(0.2, 0.5, 0.8)
  bin_counts <- c(1, 1, 1)  # Total rainy days = 3
  threshold <- 5  # Threshold not met
  
  # Call the function
  result <- calculate_b0_b1(mean_rfe, conditional_prob_rain, bin_counts, threshold)
  
  # Check result
  expect_equal(result, c(b0 = 0, b1 = 0))
})

test_that("calculate_b0_b1 returns NA when there are insufficient data points", {
  # Mock data
  mean_rfe <- c(1)
  conditional_prob_rain <- c(0.5)
  bin_counts <- c(10)  # Sufficient count, but only one data point
  
  # Call the function
  result <- calculate_b0_b1(mean_rfe, conditional_prob_rain, bin_counts, 5)
  
  # Check result
  expect_equal(result, c(b0 = NA, b1 = NA))
})

test_that("calculate_b0_b1 returns NA if model fitting fails", {
  # Mock data with illogical conditional probabilities
  mean_rfe <- c(1, 2, 3)
  conditional_prob_rain <- c(1.2, -0.1, 0.5)  # Invalid probabilities
  bin_counts <- c(10, 10, 10)
  threshold <- 5
  
  # Call the function
  result <- calculate_b0_b1(mean_rfe, conditional_prob_rain, bin_counts, threshold)
  
  # Check result
  expect_equal(result, c(b0 = NA, b1 = NA))
})

test_that("calculate_b0_b1 handles edge cases", {
  # Edge case: All conditional probabilities are 0
  mean_rfe <- c(1, 2, 3)
  conditional_prob_rain <- c(0, 0, 0)
  bin_counts <- c(10, 10, 10)
  threshold <- 5
  
  # Call the function
  result <- calculate_b0_b1(mean_rfe, conditional_prob_rain, bin_counts, threshold)
  expect_equal(result, c(b0 = NA, b1 = NA))
  
  # Edge case: All conditional probabilities are 1
  conditional_prob_rain <- c(1, 1, 1)
  result <- calculate_b0_b1(mean_rfe, conditional_prob_rain, bin_counts, threshold)
  expect_equal(result, c(b0 = NA, b1 = NA))
})