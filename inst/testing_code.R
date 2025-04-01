#devtools::install_github("IDEMSInternational/outfillingR")
#library(outfillingR)


# calibration_data <- select_calibration_data(merge1,
#                                             station = "station",
#                                             rainfall_estimate_column = "chirps_rain")


# Get data frame "merge1"
devtools::load_all()
merge1 <- readRDS("c:/users/lclem/Downloads/merge1.rds")
# outfilling
do_infilling(
  data = merge1,
  station = "station",
  date = "date",
  rainfall = "rainfall",
  rfe = "rfe",
  lon = "longitude",
  lat = "latitude",
  #station_to_exclude = "Station A",
  rainfall_estimate_column = "chirps_rain",
  autobins = TRUE,
  by_month = TRUE,
  n_bins = 7,
  custom_bins = c(1, 5, 10, 15, 20),
  count_filter = 5,
  min_rainy_days_threshold = 30,
  target_months = c(5, 6, 7, 8, 9),
  distribution_flag = "gamma",
  markovflag = TRUE
)

# outfilling
do_infilling(
  data = merge1,
  station = "station",
  date = "date",
  rainfall = "rainfall",
  rfe = "rfe",
  lon = "longitude",
  lat = "latitude",
  #station_to_exclude = "Station A",
  rainfall_estimate_column = "chirps_rain",
  autobins = TRUE,
  by_month = FALSE,
  n_bins = 15,
  custom_bins = c(1, 5, 10, 15, 20),
  count_filter = 5,
  min_rainy_days_threshold = 30,
  target_months = c(5, 6, 7, 8, 9),
  distribution_flag = "gamma",
  markovflag = TRUE
)

