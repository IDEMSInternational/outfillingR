test_that("calculate_and_plot_conditional_probabilities returns correct structure", {
  # Mock data
  filtered_df <- data.frame(
    rainfall = c(0, 5, 10, 0, 20, 0, 0, 30),
    rfe = c(0, 1, 1, 2, 2, 3, 3, 3)
  )
  rfe_bin_edges <- c(0, 1, 2, 3, 4)
  
  # Call the function
  result <- calculate_and_plot_conditional_probabilities(filtered_df, rfe_bin_edges, count_filter = 1)
  
  # Check the structure of the returned list
  expect_named(result, c("filtered_probabilities_df", "p0", "p0_rainyday", "p0_dryday"))
  expect_true(is.data.frame(result$filtered_probabilities_df))
  
  # Check that p0, p0_rainyday, and p0_dryday are numeric
  expect_true(is.numeric(result$p0))
  expect_true(is.numeric(result$p0_rainyday))
  expect_true(is.numeric(result$p0_dryday))
})

test_that("calculate_and_plot_conditional_probabilities calculates p0 correctly", {
  # Mock data
  filtered_df <- data.frame(
    rainfall = c(0, 5, 0, 0, 0),
    rfe = c(0, 0, 0, 1, 2)
  )
  rfe_bin_edges <- c(0, 1, 2, 3)
  
  # Call the function
  result <- calculate_and_plot_conditional_probabilities(filtered_df, rfe_bin_edges)
  
  # Check p0 (probability of rainfall > 0 when rfe == 0)
  expect_equal(result$p0, 1/3, tolerance = 1e-8)
})

test_that("calculate_and_plot_conditional_probabilities calculates conditional probabilities for bins", {
  # Mock data
  filtered_df <- data.frame(
    rainfall = c(0, 10, 5, 0, 20, 0, 30, 0),
    rfe = c(0, 1, 1, 2, 2, 3, 3, 3)
  )
  rfe_bin_edges <- c(0, 1, 2, 3, 4)
  
  # Call the function
  result <- calculate_and_plot_conditional_probabilities(filtered_df, rfe_bin_edges, count_filter = 1)
  filtered_probabilities_df <- result$filtered_probabilities_df
  
  # Check the bin counts and conditional probabilities
  expect_true(all(filtered_probabilities_df$Count > 1))
  expect_equal(filtered_probabilities_df$Conditional_Probability[2], 0.5, tolerance = 1e-8)  # For RFE 1-2 bin
})

test_that("calculate_and_plot_conditional_probabilities excludes bins with low counts", {
  # Mock data
  filtered_df <- data.frame(
    rainfall = c(0, 5, 0, 0, 20, 0, 0, 30),
    rfe = c(0, 1, 1, 2, 2, 3, 3, 3)
  )
  rfe_bin_edges <- c(0, 1, 2, 3, 4)
  
  # Call the function with a high count filter
  result <- calculate_and_plot_conditional_probabilities(filtered_df, rfe_bin_edges, count_filter = 5)
  
  # Check that no bins pass the filter
  expect_equal(nrow(result$filtered_probabilities_df), 0)
})

test_that("calculate_and_plot_conditional_probabilities handles edge cases", {
  # Edge case: Empty data frame
  filtered_df <- data.frame(rainfall = numeric(0), rfe = numeric(0))
  rfe_bin_edges <- c(0, 1, 2, 3)
  
  # Call the function
  result <- calculate_and_plot_conditional_probabilities(filtered_df, rfe_bin_edges)

  # Check result
  expect_true(is.data.frame(result$filtered_probabilities_df))
  expect_equal(nrow(result$filtered_probabilities_df), 0)
  expect_equal(result$p0, NaN)
  expect_equal(result$p0_rainyday, NaN)
  expect_equal(result$p0_dryday, NaN)
  
  # Edge case: Single row
  filtered_df <- data.frame(rainfall = c(5), rfe = c(0))
  result <- calculate_and_plot_conditional_probabilities(filtered_df, rfe_bin_edges)
  expect_equal(result$p0, 1)
})
