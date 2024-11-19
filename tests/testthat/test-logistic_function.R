test_that("logistic_function returns correct probabilities for scalar inputs", {
  # Test with scalar inputs
  result <- logistic_function(rfe = 0, b0 = 0, b1 = 1)
  
  # Expected result for rfe = 0, b0 = 0, b1 = 1
  expected <- 0.5
  expect_equal(result, expected, tolerance = 1e-8)
  
  # Test with different parameters
  result <- logistic_function(rfe = 1, b0 = -1, b1 = 2)
  expected <- 1 / (1 + exp(-(-1 + 2 * 1)))
  expect_equal(result, expected, tolerance = 1e-8)
})

test_that("logistic_function handles vector inputs correctly", {
  # Test with a vector of rfe values
  rfe_values <- c(-1, 0, 1)
  b0 <- 0
  b1 <- 1
  result <- logistic_function(rfe = rfe_values, b0 = b0, b1 = b1)
  
  # Expected result
  expected <- 1 / (1 + exp(-(b0 + b1 * rfe_values)))
  expect_equal(result, expected, tolerance = 1e-8)
})

test_that("logistic_function handles edge cases", {
  # Large positive rfe values
  result <- logistic_function(rfe = 100, b0 = 0, b1 = 1)
  expect_equal(result, 1, tolerance = 1e-8)
  
  # Large negative rfe values
  result <- logistic_function(rfe = -100, b0 = 0, b1 = 1)
  expect_equal(result, 0, tolerance = 1e-8)
})

test_that("logistic_function returns values between 0 and 1", {
  # Generate random inputs
  rfe_values <- runif(100, min = -100, max = 100)
  result <- logistic_function(rfe = rfe_values, b0 = 0, b1 = 1)
  
  # Check bounds
  expect_true(all(result >= 0 & result <= 1))
})

test_that("logistic_function throws an error for invalid inputs", {
  # Non-numeric rfe
  expect_error(logistic_function(rfe = "invalid", b0 = 0, b1 = 1), "non-numeric argument")
  
  # Non-numeric b0 or b1
  expect_error(logistic_function(rfe = 1, b0 = "invalid", b1 = 1), "non-numeric argument")
  expect_error(logistic_function(rfe = 1, b0 = 0, b1 = "invalid"), "non-numeric argument")
})
