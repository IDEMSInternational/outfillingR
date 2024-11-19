library(testthat)

test_that("fill_nearest correctly fills NA values with nearest non-NA values", {
  # Test with simple vector
  x <- c(1, NA, NA, 4, NA, 6)
  result <- fill_nearest(x)
  expected <- c(1, 1, 4, 4, 4, 6)
  expect_equal(result, expected)
  
  # Test with a vector where NA is at the beginning
  x <- c(NA, 2, NA, 4)
  result <- fill_nearest(x)
  expected <- c(2, 2, 2, 4)
  expect_equal(result, expected)
  
  # Test with a vector where NA is at the end
  x <- c(1, 3, NA, NA)
  result <- fill_nearest(x)
  expected <- c(1, 3, 3, 3)
  expect_equal(result, expected)
  
  # Test with consecutive NA values
  x <- c(1, NA, NA, NA, 5)
  result <- fill_nearest(x)
  expected <- c(1, 1, 1, 5, 5)
  expect_equal(result, expected)
})

test_that("fill_nearest works with no NA values", {
  # Test with a vector that has no NAs
  x <- c(1, 2, 3, 4, 5)
  result <- fill_nearest(x)
  expected <- x
  expect_equal(result, expected)
})

test_that("fill_nearest handles vectors with all NAs", {
  # Test with a vector that is entirely NA
  x <- c(NA, NA, NA)
  expect_error(fill_nearest(x),
                "At least one value in `x` cannot be missing.")
})

test_that("fill_nearest handles edge cases", {
  # Test with a single value
  x <- c(5)
  result <- fill_nearest(x)
  expected <- c(5)
  expect_equal(result, expected)
  
  # Test with alternating values and NAs
  x <- c(1, NA, 3, NA, 5)
  result <- fill_nearest(x)
  expected <- c(1, 1, 3, 3, 5)
  expect_equal(result, expected)
})

test_that("fill_nearest preserves numeric type", {
  # Test with numeric input
  x <- c(1.5, NA, 3.2, NA)
  result <- fill_nearest(x)
  expected <- c(1.5, 1.5, 3.2, 3.2)
  expect_equal(result, expected)
  expect_true(is.numeric(result))
})