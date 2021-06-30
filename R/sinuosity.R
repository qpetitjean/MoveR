#' @title Compute corrected rediscretized sinuosity index 
#'
#'
#' @description Given a data frames containing tracking informations for a given fragment, 
#' this function rediscretize fragment path and returns a vector containing the value of 
#' sinuosity along this fragment
#' 
#'
#' @param df A data frame containing at x, y coordinates and time columns named "x.pos", "y.pos", "frame" for a fragment
#' 
#' @param scale A ratio corresponding to the scaling factor to be applied to the trajectory coordinates 
#' (e.g., size in cm / size in pixels; see trajr::TrajScale())
#' 
#' @param unit The unit expected after scaling (e.g., "cm", "m", ...)
#' 
#' @param segL A numeric value expressed in the unit specified by user and corresponding 
#' to the length of the resampling step needed to discretize the input trajectory 
#' see trajr::TrajRediscretize())
#' 
#'
#' @return this function returns a vector containing the value of sinuosity for a given fragment
#'
#'
#' @authors Quentin Petitjean, Vincent Calcagno
#'
#'
#'
#' @examples
#'
#' #TODO
#'
#' @export

sinuosity <- function(df, scale = NULL, unit = NULL, segL) {
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
  if (!inherits(try(trajr::TrajRediscretize(trj, segL), silent = T)
                , "try-error")) {
    discretized <- trajr::TrajRediscretize(trj, segL)
    sinuosity <- trajr::TrajSinuosity2(discretized)
  } else if (inherits(try(trajr::TrajRediscretize(df, segL), silent = T)
                      , "try-error")) {
    sinuosity <- NA
    warning("The fragment is shorter than segL, sinuosity returned NA, segL might be to high")
  }
  return(sinuosity)
}