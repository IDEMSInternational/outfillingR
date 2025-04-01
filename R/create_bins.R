#' Create Bins for Rainfall Columns
#'
#' Computes bin edges for rainfall using quantiles, either across all data or
#' grouped by month.
#'
#' @param data A data frame containing at least a date column and a rainfall column.
#' @param date A vector of dates (same length as `rainfall` or a column in `data`).
#' @param rainfall A numeric vector of rainfall values (same length as `date`) or a column in `data`.
#' @param by_month Logical. If `TRUE`, compute bins separately for each calendar month.
#' @param n_bins Integer. The number of bins to create using quantile-based binning.
#'
#' @return If `by_month = TRUE`, returns a data frame with one row per month and a column `quantiles` 
#'         containing the bin edges as a list-column. If `by_month = FALSE`, returns a numeric vector 
#'         of quantile-based bin edges.
create_bins <- function(data, date, rainfall = NULL, by_month = TRUE, n_bins = 5){
  if (by_month){
    all_bin_edges <- data %>%
      dplyr::mutate(month = lubridate::month(date)) %>%
      dplyr::group_by(month) %>%
      dplyr::reframe(quantiles = get_n_quantiles(rainfall, n_bins, na.rm = TRUE))
  } else {
    all_bin_edges <- data %>%
      dplyr::reframe(quantiles = get_n_quantiles(rainfall, n_bins, na.rm = TRUE)) %>%
      dplyr::pull(quantiles)
  }
  return(all_bin_edges)
}