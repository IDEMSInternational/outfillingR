test_that("calibrate_kappa_theta returns k and theta for valid data", {
  # Mock data with valid mean and std
  filtered_probabilities_df <- data.frame(
    Rainfall_Mean = c(1, 2, 3, 4, 5),
    Rainfall_Std = c(0.5, 1, 1.5, 2, 2.5)
  )
  
  # Call the function
  result <- calibrate_kappa_theta(filtered_probabilities_df)
  
  # Check result structure
  expect_named(result, c("k", "theta"))
  
  # Check values are numeric and not NA
  expect_true(is.numeric(result$k))
  expect_true(is.numeric(result$theta))
  expect_false(is.na(result$k))
  expect_false(is.na(result$theta))
})

test_that("calibrate_kappa_theta returns NA for insufficient data", {
  # Mock data with fewer than 4 rows
  filtered_probabilities_df <- data.frame(
    Rainfall_Mean = c(1, 2, 3),
    Rainfall_Std = c(0.5, 1, 1.5)
  )
  
  # Call the function
  result <- calibrate_kappa_theta(filtered_probabilities_df)
  
  # Check result
  expect_equal(result, list(k = NA, theta = NA))
})

test_that("calibrate_kappa_theta handles edge cases with zero or negative values", {
  # Mock data with zero and negative rainfall means (invalid for log transformation)
  filtered_probabilities_df <- data.frame(
    Rainfall_Mean = c(1, -2, 3, 0, 5),
    Rainfall_Std = c(0.5, 1, 1.5, 2, 2.5)
  )
  
  # Call the function
  expect_error(calibrate_kappa_theta(filtered_probabilities_df), "Rainfall mean values less than 0")
})

test_that("calibrate_kappa_theta handles large datasets", {
  # Generate a large dataset
  set.seed(123)
  rainfall_means <- runif(1000, min = 1, max = 100)
  rainfall_stds <- rainfall_means / 2 + rnorm(1000, sd = 5)
  
  filtered_probabilities_df <- data.frame(
    Rainfall_Mean = rainfall_means,
    Rainfall_Std = abs(rainfall_stds)  # Ensure std is non-negative
  )
  
  # Call the function
  result <- calibrate_kappa_theta(filtered_probabilities_df)
  
  # Check result structure
  expect_named(result, c("k", "theta"))
  expect_true(!is.na(result$k))
  expect_true(!is.na(result$theta))
})

test_that("calibrate_kappa_theta preserves numeric type", {
  # Mock data with valid inputs
  filtered_probabilities_df <- data.frame(
    Rainfall_Mean = c(10, 20, 30, 40, 50),
    Rainfall_Std = c(5, 7, 10, 12, 15)
  )
  
  # Call the function
  result <- calibrate_kappa_theta(filtered_probabilities_df)
  
  # Ensure k and theta are numeric
  expect_true(is.numeric(result$k))
  expect_true(is.numeric(result$theta))
})
