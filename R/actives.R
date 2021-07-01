#' @title Determine when individual is active or not
#'
#'
#' @description Given a data frames containing tracking informations for a given fragment including speed, 
#' this function return a vector containing True or False when individual is active or not respectively
#'
#' @param df A data frame containing x, y coordinates and speed columns named "x.pos", "y.pos", "speed" for a fragment
#' 
#' @param minSpeed A numeric value expressed in the same unit than speed, corresponding to the treschold above which individual 
#' is considered as active 
#'
#' @return this function returns a vector containing True or False when individual is active or not respectively
#'
#'
#' @authors Quentin Petitjean
#'
#'
#'
#' @examples
#'
#' #TODO
#'
#' @export

actives <- function (df, minSpeed = NULL) {
  if (is.null(minSpeed)) {
    stop(
      "minSpeed argument is missing:
    impossible to determine whether individual is active or not without minimum speed treshold"
    )
  } else if (!is.null(minSpeed)) {
    actives <- df$speed  > minSpeed
  }
  return(actives)
}