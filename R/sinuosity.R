#' @title Compute corrected rediscretized sinuosity index
#'
#'
#' @description Given a data frames containing tracking informations for a given fragment,
#' this function rediscretize fragment path and returns a vector containing the value of
#' sinuosity along this fragment
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
#' @param segL A numeric value expressed in the unit specified by user and corresponding
#' to the length of the resampling step needed to discretize the input trajectory (optional)
#' see trajr::TrajRediscretize()) 
#'
#' @param TimeCol A character string corresponding to the name of the column containing Time information (e.g., "frame")
#'
#' @return this function returns a vector containing the value of sinuosity for a given fragment
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

sinuosity <- function(df,
                      scale = NULL,
                      unit = NULL,
                      segL = NULL,
                      TimeCol = NULL) {
  if (is.null(unit)) {
    warning("the unit of the trajectory path after scaling is missing, default is pixels")
    unit = "pixels"
  }
  if (is.null(TimeCol)) {
    stop(
      "TimeCol argument is missing: the name of the column carying time information is needed to compute sinuosity"
    )}
  if (is.null(scale)) {
    (
      "the scaling factor to be applied to the trajectory coordinates is missing, default is 1/1"
    )
    scale = 1 / 1
  }
  trj <-
    trajr::TrajFromCoords(df[, c("x.pos", "y.pos", TimeCol)],
                          spatialUnits = "pixels",
                          timeCol = 3)
  trj <- trajr::TrajScale(trj, scale, unit)
  if (is.null(segL)) {
    sinuosityRes <- trajr::TrajSinuosity2(trj)
  } else if (!is.null(segL)) {
    if (inherits(try(trajr::TrajRediscretize(trj, segL), silent = TRUE)
                 , "try-error")) {
      sinuosityRes <- NA
      warning("The fragment is shorter than segL, sinuosity returned NA, segL might be to high")
    } else if (!inherits(try(trajr::TrajRediscretize(trj, segL), silent = TRUE)
                         , "try-error")) {
      discretized <- trajr::TrajRediscretize(trj, segL)
      sinuosityRes <- trajr::TrajSinuosity2(trj)
    }
  }
  return(sinuosityRes)
}