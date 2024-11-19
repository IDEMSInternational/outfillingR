#' Perform Rainfall Data Infilling
#'
#' This function infills missing or incomplete rainfall data based on specified
#' parameters, statistical distributions, and historical calibration data. It
#' uses monthly parameters, supports a Markov chain approach, and allows for 
#' user-defined dry seasons and custom bins for rainfall estimates.
#'
#' @param data Either a path to a CSV file containing historical rainfall data 
#'             or a data frame.
#' @param station A string specifying the column name in `data` that contains 
#'                the station names.
#' @param date A string specifying the column name in `data` that contains 
#'             the date values.
#' @param rfe A string specifying the column name in `data` that contains 
#'            rainfall estimates (RFE) used as predictors for rainfall generation.
#' @param rainfall A string specifying the column name in `data` that contains 
#'                 the original rainfall values.
#' @param metadata An optional data frame containing additional metadata to merge 
#'                 with the historical data. Default is `NULL`.
#' @param metadata_station A string specifying the column name in the metadata 
#'                         data frame that corresponds to `station`. 
#'                         If `NULL`, it defaults to the value of `station`.
#' @param lon A string specifying the column corresponding to longitude values.
#'            If `metadata` is `NULL`, this column is from `data`, otherwise
#'            this is from `metadata`
#' @param lat A string specifying the column corresponding to latitude values.
#'            If `metadata` is `NULL`, this column is from `data`, otherwise
#'            this is from `metadata`
#' @param station_to_exclude A string specifying the station to exclude from 
#'                           calibration data.
#' @param rainfall_estimate_column A string specifying the column name in `data` 
#'                                 that contains rainfall estimates used for 
#'                                 calibration.
#' @param custom_bins A numeric vector specifying custom bins for RFE values. 
#'                    Default is `c(1, 3, 5, 10, 15, 20)`.
#' @param count_filter A numeric threshold specifying the minimum number of values 
#'                     required in each bin to include in calculations. Default is 
#'                     `10`.
#' @param min_rainy_days_threshold A numeric threshold for the minimum number of 
#'                                 rainy days required for inclusion in calculations. 
#'                                 Default is `50`.
#' @param target_months A numeric vector specifying the months (as integers) to 
#'                      apply dry season parameters. Default is `5:9` (May to 
#'                      September).
#' @param distribution_flag A character string specifying the statistical distribution 
#'                          to use for rainfall generation. Options are `"gamma"` 
#'                          (default) or `"lognormal"`.
#' @param markovflag A logical value indicating whether to use a Markov chain 
#'                   approach for rainfall occurrence. Default is `TRUE`.
#' @param b0 A numeric value for the intercept in the dry season model. Default is 
#'           `-0.2`.
#' @param b1 A numeric value for the slope in the dry season model. Default is `0.05`.
#' @param a0 A numeric value for the intercept in the rainfall distribution model. 
#'           Default is `0.1`.
#' @param a1 A numeric value for the slope in the rainfall distribution model. 
#'           Default is `1`.
#' @param b0_dryday A numeric value for the intercept for dry days. Default is 
#'                  `-0.2`.
#' @param b0_rainyday A numeric value for the intercept for rainy days. Default is 
#'                    `-0.2`.
#' @param b1_dryday A numeric value for the slope for dry days. Default is `0.05`.
#' @param b1_rainyday A numeric value for the slope for rainy days. Default is `0.05`.
#' @param kappa A numeric value specifying the scaling factor for the rainfall 
#'              distribution variance. Default is `2`.
#' @param theta A numeric value specifying the exponent for the rainfall 
#'              distribution variance. Default is `2`.
#' @param p0 A numeric value for the baseline probability of precipitation. Default 
#'           is `0.001`.
#' @param p0_rainyday A numeric value for the probability of precipitation following 
#'                    a rainy day. Default is `0.001`.
#' @param p0_dryday A numeric value for the probability of precipitation following 
#'                  a dry day. Default is `0.001`.
#'
#' @export
#' @return A data frame containing the infilled rainfall data, including columns 
#'         for station name, date, RFE, original rainfall, and generated rainfall.
#'
#' @examples
#' # Example data
#' historical_data <- data.frame(
#'   Station_name = c("Station A", "Station B"),
#'   date = as.Date(c("2023-01-01", "2023-01-02")),
#'   rainfall = c(5, 10),
#'   rfe = c(0.5, 1.2),
#'   lon = c(34.5, 34.6),
#'   lat = c(-1.5, -1.6)
#' )
#'
#' # Perform infilling
#' infilled_data <- do_infilling(
#'   data = historical_data,
#'   station = "Station_name",
#'   date = "date",
#'   rainfall = "rainfall",
#'   rfe = "rfe",
#'   lon = "lon",
#'   lat = "lat",
#'   station_to_exclude = "Station A",
#'   rainfall_estimate_column = "rfe"
#' )
#'
#' print(infilled_data)
#'
#' @seealso \code{\link{weather_generator}}, \code{\link{compute_monthly_parameters}}
do_infilling <- function(data,
                         station,
                         date,
                         rainfall,
                         rfe,
                         metadata = NULL,
                         metadata_station = NULL,
                         lon,
                         lat,
                         station_to_exclude, rainfall_estimate_column,
                         custom_bins = c(1, 3, 5, 10, 15, 20),
                         count_filter = 10,
                         min_rainy_days_threshold = 50,
                         target_months = 5:9, #User defined dry season
                         distribution_flag = c("gamma", "lognormal"),
                         markovflag = TRUE,
                         b0=-0.2,
                         b1=0.05,
                         a0=0.1,
                         a1=1,
                         b0_dryday=-0.2,
                         b0_rainyday=-0.2,
                         b1_dryday=0.05,
                         b1_rainyday=0.05,
                         kappa=2,
                         theta=2,
                         p0=0.001,
                         p0_rainyday=0.001,
                         p0_dryday=0.001
                         
){
  distribution_flag <- match.arg(distribution_flag)
  calibration_data <- select_calibration_data(data, station = station, rainfall_estimate_column = rainfall_estimate_column, station_to_exclude = station_to_exclude)
  
  # Call the function to compute monthly parameters
  dry_season_params <- list(
    b0 = b0,
    b1 = b1,
    a0 = a0,
    a1 = a1,
    b0_dryday = b0_dryday,
    b0_rainyday = b0_rainyday,
    b1_dryday = b1_dryday,
    b1_rainyday = b1_rainyday,
    kappa = kappa,
    theta = theta,
    p0 = p0,
    p0_rainyday = p0_rainyday,
    p0_dryday = p0_dryday
  )
  
  monthly_parameters <- compute_monthly_parameters(data = calibration_data,
                                                   custom_bins = custom_bins,
                                                   count_filter = count_filter,
                                                   min_rainy_days_threshold = min_rainy_days_threshold)
  
  # Replace the values in monthly_parameters for the target months with dry_season_params values
  monthly_parameters[target_months, ] <- list(
    Month = target_months,
    b0 = dry_season_params$b0,
    b1 = dry_season_params$b1,
    a0 = dry_season_params$a0,
    a1 = dry_season_params$a1,
    b0_rainyday = dry_season_params$b0_rainyday,
    b1_rainyday = dry_season_params$b1_rainyday,
    b0_dryday = dry_season_params$b0_dryday,
    b1_dryday = dry_season_params$b1_dryday,
    kappa = dry_season_params$kappa,
    theta = dry_season_params$theta,
    p0 = dry_season_params$p0,
    p0_rainyday = dry_season_params$p0_rainyday,
    p0_dryday = dry_season_params$p0_dryday
  )
  
  # Check the updated monthly_parameters to ensure values were correctly substituted
  generated_weather <- weather_generator(
    data = data,
    station = station,
    date = date,
    rfe = rfe,
    rainfall = rainfall,
    metadata = metadata,
    metadata_station = metadata_station,
    lon = lon,
    lat = lat,
    monthly_params_df = monthly_parameters,
    distribution_flag = 'gamma',  # Choose 'gamma' or 'lognormal'
    markovflag = markovflag
  )
  return(generated_weather)
}