#' @title Sliding window.
#' 
#' @description Given a vector containing numeric data, this function returns a vector of smoothed data 
#' by performing the computation specified by the 'statistic' argument over the input vector moving through centered sliding window.
#'
#' @param x A vector containing data to smooth.
#'
#' @param Tstep A numeric value corresponding to the length of the sliding window (i.e., the number of values).
#' 
#' @param statistic A character string indicating the computation to preform over the sliding window. The following arguments are currently supported: 
#' \itemize{
#'          \item{'sum' = sum the values over each sliding window.}
#'          \item{'mean' = average the values over each sliding window.}
#'          \item{'median' = compute the median over each sliding window.}
#'          \item{'var' = compute the variance over each sliding window.}
#'          \item{'sd' = compute the standard deviation over each sliding window.}
#'          \item{'circular.var' = compute the variance of circular data (angular values expressed in radians) over each sliding window.}
#'          }
#' 
#' @param na.rm A logical value (i.e., TRUE or FALSE) indicating whether NA values should be stripped before the computation proceeds (default = TRUE). 
#' In case na.rm = TRUE and the selected statistic argument is 'median', the function use the '+Big_alternate' method to compute the median while ignoring NA values (see the help page of the \code{\link[stats]{runmed}} function for more information).
#'
#' @return A vector of the same length than the input data (x) which contains the smoothed data.
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link[stats]{runmed}}, \code{\link[stats]{filter}}
#'
#' @examples
#' 
#' set.seed(2023) 
#' # compute smoothed mean over a sliding window of 15 time unit in length
#' slidWindow(x = rnorm(150), Tstep = 15, statistic = 'mean',  na.rm = TRUE)
#' 
#' # compute smoothed variance over a sliding window of 15 time unit in length
#' slidWindow(x = rnorm(150), Tstep = 15, statistic = 'var',  na.rm = TRUE)
#' 
#' @export

slidWindow <-
  function (x,
            Tstep,
            statistic = c("sum", "mean", "median", "var", "sd", "circular.var"),
            na.rm = TRUE) {
    if (length(statistic) > 1) {
      stop(
        "[statistic] argument is unspecified, choose either 'sum', 'mean', 'median', 'var', 'sd' or 'circular.var'"
      )
    }
    if (!statistic %in% c("sum", "mean", "median", "var", "sd", "circular.var")) {
      stop(
        "[statistic] argument is wrong or misspelled, choose either 'sum', 'mean', 'median', 'var', 'sd' or 'circular.var'"
      )
    }
    if(isTRUE(na.rm) & statistic == "sum" | isTRUE(na.rm) & statistic == "mean"){
      naLoc <- ifelse(is.na(x), 1, 0)
      x <- ifelse(is.na(x), 0, x)
    }
    if (statistic == "sum") {
      smoothed <- stats::filter(
        x,
        filter = rep(1, Tstep),
        method = "convolution",
        circular = F,
        sides = 2
      )
    }
    if (statistic == "mean") {
      smoothed <-
        stats::filter(
          x,
          filter = rep(1/Tstep, Tstep),
          method = "convolution",
          circular = F, 
          sides = 2
        )
      if(isTRUE(na.rm)){
        smoothedNaCount <- stats::filter(naLoc, rep(1, Tstep), sides=2)
        smoothed <- smoothed * (Tstep / (Tstep - smoothedNaCount))
      }
    }
    if (statistic == "median") {
      if(isTRUE(na.rm)){
        smoothed <- stats::runmed(x, Tstep, endrule = "keep", na.action = "+Big_alternate")
      } else{
        smoothedNaCount <- stats::filter(ifelse(is.na(x), 1, 0), rep(1, Tstep), sides=2)
        smoothed <- stats::runmed(x, Tstep, endrule = "keep")
        smoothed[which(smoothedNaCount == 1)] <- NA
      }
      smoothed[c(seq(floor(Tstep/2)), seq(from = length(smoothed), to = length(smoothed)-floor(Tstep/2)+1))] <- NA
    }
    if (statistic == "var" | statistic == "sd") {
      MovSum_x <- MoveR::slidWindow(x = x, Tstep = Tstep, statistic = "sum", na.rm = na.rm)
      MovSum_x2 <- MoveR::slidWindow(x = x^2, Tstep = Tstep, statistic = "sum", na.rm = na.rm)
      if(isTRUE(na.rm)){
        smoothedNaCount <- stats::filter(ifelse(is.na(x), 1, 0), rep(1, Tstep), sides=2)
        Tstep <- rep(Tstep, length(x)) - smoothedNaCount
      }
      smoothed <- ((MovSum_x2 / Tstep) - (MovSum_x / Tstep)^2) * (Tstep / (Tstep - 1))
      if (statistic == "sd") {
        smoothed <- sqrt(smoothed)
      }
    }
    if (statistic == "circular.var") {
      mean_cos <- MoveR::slidWindow(x = cos(x), Tstep = Tstep, statistic = "mean", na.rm = na.rm)
      mean_sin <- MoveR::slidWindow(x = sin(x), Tstep = Tstep, statistic = "mean", na.rm = na.rm)
      smoothed <- 1 - sqrt(mean_cos^2 + mean_sin^2)
    }
    return(c(smoothed))
  }
