# Mock data
data <- data.frame(
  station = c("Station1", "Station2", "Station3", "Station1"),
  rainfall = c(10, 20, 30, 40),
  other_column = c("A", "B", "C", "D")
)

# Call the function
result <- select_calibration_data(
  data = data,
  station = "station",
  rainfall_estimate_column = "rainfall",
  station_to_exclude = "Station1"
)

test_that("select_calibration_data works with a data frame", {
  # Check output structure
  expect_true(is.data.frame(result))
  expect_true("rfe" %in% colnames(result))
  
  # Check station exclusion
  expect_false(any(result$station == "Station1"))
  
  # Check rfe column values
  expect_equal(result$rfe, c(20, 30))
})

test_that("select_calibration_data handles missing columns", {
  # Mock data
  data <- data.frame(
    station = c("Station1", "Station2", "Station3"),
    other_column = c("X", "Y", "Z")
  )
  
  # Call the function with a non-existent rainfall column
  expect_error(
    select_calibration_data(
      data = data,
      station = "station",
      rainfall_estimate_column = "rainfall",
      station_to_exclude = "Station2"
    )
  )
})
