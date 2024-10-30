#' Logistic Function
#'
#' Computes the logistic function for a given RFE value and parameters.
#' This function can be used to estimate conditional probabilities using a logistic model.
#'
#' @param rfe A numeric value or vector representing the rainfall estimate (RFE).
#' @param b0 A numeric value representing the intercept of the logistic model.
#' @param b1 A numeric value representing the slope of the logistic model.
#' @return A numeric value or vector giving the probability output from the logistic function.
#' 
logistic_function <- function(rfe, b0, b1) {
  1 / (1 + exp(-(b0 + b1 * rfe)))
}
