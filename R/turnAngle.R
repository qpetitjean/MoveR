#' @title Compute turning angle
#'
#'
#' @description Given a data frames containing tracking information for a particle, 
#' this function compute returns a vector containing the value of turning angle along the considered fragment
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
      #if (turnAngle_temp > pi) {
      # turnAngle_temp[turnAngle_temp < 0] = 2 * pi
      #} else if (turnAngle_temp <= -pi) {
      # turnAngle_temp[turnAngle_temp > 0] = 2 * pi
      #}
      turnAngle <- c(turnAngle, turnAngle_temp)
      
    } else {
      next()
    }
  }
  turnAngle <-  append(turnAngle, NA, after = length(turnAngle))
  if (length(unit) > 1) {
    stop("Unit argument is not specified, choose either radians or degrees")
  } else if (!unit == "degrees" & !unit == "radians"){
    stop("Unit argument seems misspelled, choose either radians or degrees")
  } else if (unit == "radians") {
    turnAngle <- turnAngle
  } else if (unit == "degrees") {
    turnAngle <- turnAngle * 180 / pi
  }
  return(turnAngle)
}
