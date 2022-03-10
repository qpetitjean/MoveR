#' @title Compute speed 
#'
#'
#' @description Given a data frames containing tracking informations for a given fragment, 
#' this function scale fragment path and returns a vector containing the value of 
#' speed along this fragment
#' 
#'
#' @param df A data frame containing at x and y coordinates columns named "x.pos", "y.pos", the df should also contains
#' a time column (e.g., frame, second) for a fragment
#' 
#' @param scale A ratio corresponding to the scaling factor to be applied to the trajectory coordinates 
#' (e.g., size in cm / size in pixels; see trajr::TrajScale())
#' 
#' @param unit The unit expected after scaling (e.g., "cm", "m", ...)
#' 
#' @param TimeCol A character string corresponding to the name of the column containing Time information (e.g., "frame")
#'
#' @return this function returns a vector containing the value of speed along a given fragment
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

speed <- function(df, scale = NULL, unit = NULL, TimeCol = NULL) {
  if(is.null(unit)) { 
    warning("the unit of the trajectory path after scaling is missing, default is pixels")
    unit = "pixels"
  }
  if(is.null(scale)) { 
    ("the scaling factor to be applied to the trajectory coordinates is missing, default is 1/1")
    scale = 1/1
  }
  if (is.null(TimeCol)) {
    stop(
      "TimeCol argument is missing: the name of the column carying time information is needed to compute speed"
    )}
  
  trj <-
    trajr::TrajFromCoords(df[, c("x.pos", "y.pos", TimeCol)],
                          spatialUnits = "pixels",
                          timeCol = 3)
  trj <- trajr::TrajScale(trj, scale, unit)
  trjderiv <- trajr::TrajDerivatives(trj)
  speed <-  append(trjderiv[[1]], NA, after=0) 
  return(speed)
}