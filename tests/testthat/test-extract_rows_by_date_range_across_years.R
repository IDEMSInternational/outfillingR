test_that("extract_rows_by_date_range_across_years handles normal ranges within a single year", {
  # Mock data
  df <- data.frame(
    date = as.Date(c("2021-01-01", "2021-02-15", "2021-03-10", "2021-04-25", "2021-12-31")),
    value = c(1, 2, 3, 4, 5)
  )
  
  # Call the function with a normal range
  result <- extract_rows_by_date_range_across_years(df, date = "date", start_date = "02-01", end_date = "03-31")
  
  # Expected rows
  expected <- (df[2:3, ])
  
  rownames(expected) <- c("1", "2")
  rownames(result) <- c("1", "2")
  
  # Check the result
  expect_equal(result, expected)
})

test_that("extract_rows_by_date_range_across_years handles ranges crossing the year boundary", {
  # Mock data
  df <- data.frame(
    date = as.Date(c("2021-01-01", "2021-12-15", "2021-12-31", "2021-11-30", "2021-07-15")),
    value = c(1, 2, 3, 4, 5)
  )
  
  # Call the function with a range crossing the year boundary
  result <- extract_rows_by_date_range_across_years(df, date = "date", start_date = "12-01", end_date = "01-15")
  
  # Expected rows
  expected <- df[c(1, 2, 3), ]
  
  # Check the result
  expect_equal(result, expected)
})

test_that("extract_rows_by_date_range_across_years handles empty data frames gracefully", {
  # Mock empty data frame
  df <- data.frame(date = as.Date(character()), value = numeric())
  
  # Call the function
  result <- extract_rows_by_date_range_across_years(df, date = "date", start_date = "02-01", end_date = "03-31")
  
  # Check the result
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), ncol(df))
})

test_that("extract_rows_by_date_range_across_years works with leap years", {
  # Mock data with a leap year
  df <- data.frame(
    date = as.Date(c("2020-02-28", "2020-02-29", "2020-03-01", "2021-02-28", "2021-03-01")),
    value = c(1, 2, 3, 4, 5)
  )
  
  # Call the function for a range covering February
  result <- extract_rows_by_date_range_across_years(df, date = "date", start_date = "02-28", end_date = "03-01")
  
  # Expected rows
  expected <- df[c(1, 2, 3, 4, 5), ]
  
  # Check the result
  expect_equal(result, expected)
})

test_that("extract_rows_by_date_range_across_years handles full-year range", {
  # Mock data
  df <- data.frame(
    date = as.Date(c("2021-01-01", "2021-06-15", "2021-12-31")),
    value = c(1, 2, 3)
  )
  
  # Call the function with the full-year range
  result <- extract_rows_by_date_range_across_years(df, date = "date", start_date = "01-01", end_date = "12-31")
  
  # Check the result matches the input
  expect_equal(result, df)
})
