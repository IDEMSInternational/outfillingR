% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_monthly_parameters.R
\name{compute_monthly_parameters}
\alias{compute_monthly_parameters}
\title{Compute Monthly Parameters from Rainfall Data}
\usage{
compute_monthly_parameters(
  data,
  date,
  bin_edges = c(1, 3, 5, 10, 15),
  count_filter = 10,
  min_rainy_days_threshold = 10
)
}
\arguments{
\item{data}{A data frame or a file path to a CSV file containing daily rainfall data.
Must include at least a date column and a rainfall column.}

\item{date}{A string giving the name of the column in \code{data} that contains the date values.}

\item{bin_edges}{Either a data frame giving the custom bins by month, otherwise a numeric vector that specifies the RFE bin edges.}

\item{count_filter}{Numeric. The minimum number of values in each bin for it to be used in the regression calculations.}

\item{min_rainy_days_threshold}{Numeric. The minimum number of rainy days required to fit the probability models for rainy and dry days.}
}
\value{
A data frame with one row per month, containing the following columns:
\describe{
\item{Month}{Integer month (1 = January, ..., 12 = December).}
\item{b0, b1}{Intercept and slope from a linear model of conditional probability vs. RFE.}
\item{a0, a1}{Intercept and slope from a linear model of mean rainfall vs. RFE.}
\item{b0_rainyday, b1_rainyday}{Intercept and slope for the probability of a rainy day vs. RFE.}
\item{b0_dryday, b1_dryday}{Intercept and slope for the probability of a dry day vs. RFE.}
\item{kappa, theta}{Parameters from a calibrated distribution of rainfall occurrence.}
\item{p0}{Baseline probability of rainfall.}
\item{p0_rainyday}{Baseline probability of rainfall on a rainy day.}
\item{p0_dryday}{Baseline probability of rainfall on a dry day.}
}
}
\description{
This function computes a range of statistical parameters for each calendar month
based on daily rainfall data. It can work with either a CSV file or a data frame,
and uses rainfall frequency estimates (RFE) binned over custom or automatically
generated intervals. For each month, it estimates linear model coefficients
relating RFE to various conditional probabilities, mean rainfall, and other
statistical quantities.
}
\details{
This function is designed to support rainfall modeling and simulation by
generating month-specific statistics. It uses helper functions such as
\code{extract_rows_by_date_range_across_years()}, \code{calculate_and_plot_conditional_probabilities()},
\code{calculate_b0_b1()}, and \code{calibrate_kappa_theta()} which must be defined in the environment.
}
