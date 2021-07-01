#' @title Compute corrected rediscretized sinuosity index 
#'
#'
#' @description Given a data frames containing tracking informations for a given fragment, 
#' this function compute turning angle of individual and returns a vector containing the value of 
#' turning angle along this fragment
#' 
#'
#' @param df A data frame containing x, y coordinates columns named "x.pos", "y.pos" for a given fragment
#' 
#' @param unit A character string indicating whether the function should returns turning angle in radians or degrees
#'
#' @return this function returns a vector containing the value of turning angle computed along a given fragment
#'
#'
#' @authors Quentin Petitjean
#'
#'
#' @examples
#'
#' #TODO
#'
#' @export

turnAngle <- function(df, unit = c("radians", "degrees")) {
  turnAngle <- vector()
  turnAngle <-  append(turnAngle, NA, after = 0)
  for (j in seq(nrow(df))) {
    if (j < nrow(df) - 1) {
      angle1 <-
        atan2(df[j, ]$x.pos - df[j + 1, ]$x.pos,
              df[j + 1, ]$y.pos - df[j, ]$y.pos)
      angle2 <-
        atan2(df[j + 1,]$x.pos - df[j + 2,]$x.pos,
              df[j + 2,]$y.pos - df[j + 1,]$y.pos)
      turnAngle_temp <- angle2 - angle1
      if (turnAngle_temp > pi) {
        turnAngle_temp[turnAngle_temp < 0] = 2 * pi
      } else if (turnAngle_temp <= -pi) {
        turnAngle_temp[turnAngle_temp > 0] = 2 * pi
      }
      turnAngle <- c(turnAngle, turnAngle_temp)
      
    } else {
      next()
    }
  }
  turnAngle <-  append(turnAngle, NA, after = length(turnAngle))
  if (unit == "radians") {
    turnAngle <- turnAngle
  } else if (unit == "degrees") {
    turnAngle <- turnAngle * 180 / pi
  }if (length(unit) > 1 | !unit == "degrees" | !unit == "radians") {
    stop("Unit argument is not specified or misspelled, choose either radians or degrees")
  }
  return(turnAngle)
}