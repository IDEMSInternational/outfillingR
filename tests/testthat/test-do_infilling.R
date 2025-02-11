data("zambia_data")
zambia_data <- zambia_data %>%
  dplyr::filter(station %in% c("PETAUKE MET", "MSEKERA AGROMET", "MFUWE MET")) %>%
  dplyr::filter(date <= "2013-12-31")

test_that("do_infilling performs infilling with valid data and parameters", {
  # Mock monthly parameters
  monthly_params <- data.frame(
    Month = 1:12,
    b0 = rep(0.5, 12),
    b1 = rep(0.1, 12),
    b0_rainyday = rep(0.5, 12),
    b1_rainyday = rep(0.1, 12),
    b0_dryday = rep(0.3, 12),
    b1_dryday = rep(0.05, 12),
    a0 = rep(0.2, 12),
    a1 = rep(0.1, 12),
    kappa = rep(1, 12),
    theta = rep(0.5, 12),
    p0 = rep(0.2, 12),
    p0_rainyday = rep(0.3, 12),
    p0_dryday = rep(0.1, 12)
  )
  
  # Call the function
  result <- do_infilling(
    data = zambia_data,
    station = "station",
    date = "date",
    rainfall = "rainfall",
    rfe = "rfe",
    lon = "lon",
    lat = "lat",
    #station_to_exclude = "PETAUKE MET",
    rainfall_estimate_column = "chirps"
  )
  
  # Check result structure
  expect_true(is.data.frame(result)) 
})

test_that("do_infilling handles missing values correctly", {
  # Mock data with missing values
  historical_data <- zambia_data
  historical_data$rfe <- NA
  historical_data$rainfall <- NA
  
  # Call the function
  result <- do_infilling(
    data = historical_data,
    station = "station",
    date = "date",
    rainfall = "rainfall",
    rfe = "rfe",
    lon = "lon",
    lat = "lat",
    rainfall_estimate_column = "chirps",
    custom_bins = c(0.5, 1.5),
    count_filter = 1,
    min_rainy_days_threshold = 1,
    return_type = "data.frame"
  )
  
  # Check that it returns a data frame
  expect_true(is.data.frame(result))
})
