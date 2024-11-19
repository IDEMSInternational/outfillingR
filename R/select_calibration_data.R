#' Select Calibration Data
#'
#' This function reads a CSV file containing rainfall data, selects the relevant
#' column for rainfall estimates, and excludes data from a specified station for calibration.
#' It outputs a CSV file with the filtered calibration data.
#'
#' @param data A string specifying the file path to the CSV file containing rainfall data.
#' @param station A string specifying the column name in `data` that contains 
#'                the station names.
#' @param rainfall_estimate_column A string representing the column name with rainfall estimates to be used in calibration.
#' @param station_to_exclude A string representing the name of the station to exclude from the calibration data.
#' @param save A logical column indicating whether to save the resulting data frame or not. Default `FALSE`.
#' @return A data frame with the calibration data (without rows from the excluded station).
#' @export
#' 
#' @examples
#' # Select calibration data from "rainfall_data.csv" excluding "Station_A" 
#' # with rainfall estimates from the column "Rainfall_Estimate"
#' # calibration_data <- select_calibration_data("rainfall_data.csv", "Rainfall_Estimate", "Station_A")
select_calibration_data <- function(data, station, rainfall_estimate_column, station_to_exclude, save = FALSE) {
  
  # If data is a file path, read the CSV; otherwise, assume it's a data frame
  df <- if (is.character(data)) read.csv(data, stringsAsFactors = FALSE) else data
  
  # Read CSV and select relevant column, excluding specified station
  calibration_data <- df %>%
    dplyr::mutate(rfe = .data[[rainfall_estimate_column]]) %>%
    dplyr::filter(station != station_to_exclude)
  
  if (save) write.csv(calibration_data, "calibration_data.csv")
  
  return(calibration_data)
}