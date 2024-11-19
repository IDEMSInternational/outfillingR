
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/IDEMSInternational/outfillingR/workflows/R-CMD-check/badge.svg)](https://github.com/IDEMSInternational/outfillingR/actions)
[![Codecov test
coverage](https://codecov.io/gh/IDEMSInternational/outfillingR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/IDEMSInternational/outfillingR?branch=main)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![CRAN
status](https://www.r-pkg.org/badges/version/outfillingR)](https://CRAN.R-project.org/package=outfillingR)
[![license](https://img.shields.io/badge/license-LGPL%20(%3E=%203)-lightgrey.svg)](https://www.gnu.org/licenses/lgpl-3.0.en.html)
<!-- badges: end -->

## Overview

`outfillingR` is an R package designed to address missing data through
infilling and outfilling methods. The package is particularly suited for
environmental and meteorological data, including rainfall datasets, and
allows users to incorporate statistical distributions, custom bins, and
Markov chain techniques into their workflows.

## Installation

Install the development version from GitHub with:

``` r
#install.packages("devtools")
devtools::install_github("IDEMSInternational/outfillingR")
```

## Examples

Here is an example of infilling data.

``` r
# Load the outfillingR library
library(outfillingR)
```

``` r
# Load example dataset included with the package
data("zambia_data") 

# Explore the dataset
head(zambia_data)
```

| station     | date       |   lon |    lat | rainfall | rfe |   chirps | imerg_cal | imerg_uncal |       era5 |  uwnd_925 |   uwnd_600 | vwnd_925 |   vwnd_600 | tamsat |
|:------------|:-----------|------:|-------:|---------:|----:|---------:|----------:|------------:|-----------:|----------:|-----------:|---------:|-----------:|-------:|
| PETAUKE MET | 1983-01-01 | 31.28 | -14.25 |      0.0 |  NA |  0.00000 |        NA |          NA |  0.2717972 | -1.949852 | -1.9164579 | 4.089509 |  0.2734017 |     NA |
| PETAUKE MET | 1983-01-02 | 31.28 | -14.25 |      0.0 |  NA |  0.00000 |        NA |          NA |  0.1244545 | -4.332736 | -2.2785558 | 2.865819 |  0.2813057 |     NA |
| PETAUKE MET | 1983-01-03 | 31.28 | -14.25 |      0.0 |  NA |  0.00000 |        NA |          NA |  0.4241467 | -2.962837 | -1.7329807 | 2.163186 | -0.8702773 |     NA |
| PETAUKE MET | 1983-01-04 | 31.28 | -14.25 |      0.6 |  NA | 18.64820 |        NA |          NA |  2.5868416 | -5.279587 | -1.2310308 | 3.091859 | -1.0639247 |     NA |
| PETAUKE MET | 1983-01-05 | 31.28 | -14.25 |      0.0 |  NA |  0.00000 |        NA |          NA |  0.4715919 | -5.617349 | -1.1299206 | 4.285424 | -0.7625155 |     NA |
| PETAUKE MET | 1983-01-06 | 31.28 | -14.25 |      1.2 |  NA | 29.31855 |        NA |          NA | 24.7664450 | -7.546356 | -0.3533143 | 4.400442 | -0.1528946 |     NA |

``` r
# Perform infilling using default parameters
infilled_data <- do_infilling(
  data = zambia_data,
  station = "Station_name",
  date = "date",
  rainfall = "rainfall",
  rfe = "rfe",
  lon = "lon",
  lat = "lat",
  station_to_exclude = "Station A",
  rainfall_estimate_column = "rfe",
  custom_bins = c(1, 5, 10, 15, 20),
  count_filter = 5,
  min_rainy_days_threshold = 30,
  target_months = c(5, 6, 7, 8, 9),
  distribution_flag = "gamma",
  markovflag = TRUE
)

# View the results
head(infilled_data)
```

    #> Data for month: 1 
    #> Data for month: 2 
    #> Data for month: 3 
    #> Data for month: 4 
    #> No data for month: 5 
    #> No data for month: 6 
    #> No data for month: 7 
    #> No data for month: 8 
    #> No data for month: 9 
    #> Data for month: 10 
    #> Data for month: 11 
    #> Data for month: 12

| station     | date       |   lon |    lat | rainfall | rfe |   chirps | imerg_cal | imerg_uncal |       era5 |  uwnd_925 |   uwnd_600 | vwnd_925 |   vwnd_600 | tamsat |
|:------------|:-----------|------:|-------:|---------:|----:|---------:|----------:|------------:|-----------:|----------:|-----------:|---------:|-----------:|-------:|
| PETAUKE MET | 1983-01-01 | 31.28 | -14.25 |      0.0 |  NA |  0.00000 |        NA |          NA |  0.2717972 | -1.949852 | -1.9164579 | 4.089509 |  0.2734017 |     NA |
| PETAUKE MET | 1983-01-02 | 31.28 | -14.25 |      0.0 |  NA |  0.00000 |        NA |          NA |  0.1244545 | -4.332736 | -2.2785558 | 2.865819 |  0.2813057 |     NA |
| PETAUKE MET | 1983-01-03 | 31.28 | -14.25 |      0.0 |  NA |  0.00000 |        NA |          NA |  0.4241467 | -2.962837 | -1.7329807 | 2.163186 | -0.8702773 |     NA |
| PETAUKE MET | 1983-01-04 | 31.28 | -14.25 |      0.6 |  NA | 18.64820 |        NA |          NA |  2.5868416 | -5.279587 | -1.2310308 | 3.091859 | -1.0639247 |     NA |
| PETAUKE MET | 1983-01-05 | 31.28 | -14.25 |      0.0 |  NA |  0.00000 |        NA |          NA |  0.4715919 | -5.617349 | -1.1299206 | 4.285424 | -0.7625155 |     NA |
| PETAUKE MET | 1983-01-06 | 31.28 | -14.25 |      1.2 |  NA | 29.31855 |        NA |          NA | 24.7664450 | -7.546356 | -0.3533143 | 4.400442 | -0.1528946 |     NA |

## Contributing

We welcome contributions! If you’d like to contribute, please submit a
pull request or file an issue on the GitHub repository.
