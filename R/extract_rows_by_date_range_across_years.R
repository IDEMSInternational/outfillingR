#' Extract Rows by Date Range Across Multiple Years
#'
#' This function filters rows in a data frame based on a specified date range, ignoring the year
#' component and treating the date range as recurring annually. Useful for selecting seasonal data.
#'
#' @param df A data frame with a column named `date`, which should be of class `Date` or convertible to `Date`.
#' @param start_date A string in "MM-DD" format specifying the start of the date range.
#' @param end_date A string in "MM-DD" format specifying the end of the date range.
#' @return A filtered data frame containing rows where the `date` falls within the specified date range, across all years.
extract_rows_by_date_range_across_years <- function(df, start_date, end_date) {
  # Use 2021 as reference year to handle the range
  # Convert start_date and end_date to "MM-DD" format
  start_date <- format(as.Date(paste0("2021-", start_date)), "%m-%d")  # 2021 used as a reference year
  end_date <- format(as.Date(paste0("2021-", end_date)), "%m-%d")
  
  df %>%
    dplyr::mutate(
      date = as.Date(date, format = "%Y-%m-%d"),
      month_day = format(date, "%m-%d")
    ) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::filter(
      if (start_date <= end_date) {
        dplyr::between(month_day, start_date, end_date)
      } else {
        month_day >= start_date | month_day <= end_date
      }
    ) %>%
    dplyr::select(-month_day)
}
