#' @title Compute speed 
#'
#'
#' @description Given a data frames containing tracking informations for a given fragment, 
#' this function scale fragment path and returns a vector containing the value of 
#' speed along this fragment
#' 
#'
#' @param df A data frame containing at x, y coordinates and time columns named "x.pos", "y.pos", "frame" for a fragment
#' 
#' @param scale A ratio corresponding to the scaling factor to be applied to the trajectory coordinates 
#' (e.g., size in cm / size in pixels; see trajr::TrajScale())
#' 
#' @param unit The unit expected after scaling (e.g., "cm", "m", ...)
#' 
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

speed <- function(df, scale = NULL, unit = NULL) {
  if(is.null(unit)) { 
    warning("the unit of the trajectory path after scaling is missing, default is pixels")
    unit = "pixels"
  }
  if(is.null(scale)) { 
    ("the scaling factor to be applied to the trajectory coordinates is missing, default is 1/1")
    scale = 1/1
  }
  trj <-
    trajr::TrajFromCoords(dplyr::select(df, c("x.pos", "y.pos", "frame")),
                          spatialUnits = "pixels",
                          timeCol = 3)
  trj <- trajr::TrajScale(trj, scale, unit)
  trjderiv <- trajr::TrajDerivatives(trj)
  speed <-  append(trjderiv[[1]], NA, after=0) 
  return(speed)
}
