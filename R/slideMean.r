#' @title Sliding mean
#'
#' @description Given a vector containing numeric data, this function returns a vector of smoothed data by computing 
#' mean values along a sliding window  
#'
#'
#' @param x A vector containing data to smooth
#'
#' @param TStep A numeric value corresponding to the length of the sliding window (number of values)
#'
#' @return A vector containing mean smoothed data 
#'
#' @authors Quentin Petitjean, Vincent Calcagno
#'
#'
#' @examples
#'
#' # TODO
#'

slideMean <- function(x, TStep){stats::filter(x, rep(1 / TStep, TStep), method = "convolution", sides = 2)}
