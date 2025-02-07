#' Select Calibration Data
#'
#' This function reads a CSV file containing rainfall data, selects the relevant
#' column for rainfall estimates, and excludes data from a specified station for calibration.
#' It outputs a CSV file with the filtered calibration data.
#'
#' @param data Either a path to a CSV file containing historical rainfall data 
#'             or a data frame.
#' @param station A string specifying the column name in `data` that contains 
#'                the station names.
#' @param rainfall_estimate_column A string representing the column name with rainfall estimates to be used in calibration.
#' @param stations_to_include A string representing the name of the station to exclude from the calibration data (Optional).
#' @param save A logical column indicating whether to save the resulting data frame or not. Default `FALSE`.
#' @return A data frame with the calibration data (without rows from the excluded station).
select_calibration_data <- function(data, station, rainfall_estimate_column, stations_to_include = NULL, save = FALSE) {
  
  # If data is a file path, read the CSV; otherwise, assume it's a data frame
  df <- if (is.character(data)) utils::read.csv(data, stringsAsFactors = FALSE) else data
  
  # Read CSV and select relevant column, excluding specified station
  calibration_data <- df %>% dplyr::mutate(rfe = .data[[rainfall_estimate_column]])
  
  if (!is.null(stations_to_include)){
    calibration_data <- calibration_data %>% dplyr::filter(.data[[station]] %in% stations_to_include)
  }
  
  if (save) utils::write.csv(calibration_data, "calibration_data.csv")
  
  return(calibration_data)
}
