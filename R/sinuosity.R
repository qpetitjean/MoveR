#' @title Compute corrected rediscretized sinuosity index for a given tracklet.
#'
#' @description Given a data frames containing tracking information for a given tracklet,
#' this function rediscretize the tracklet and returns a vector containing the value of
#' sinuosity along the trajectory.
#'
#'
#' @param df  A data frame containing x and y coordinates in columns named "x.pos", "y.pos" for a given tracklet, as well as
#' a column containing time information, whatever the unit, over the tracklet.
#'
#' @param scale A ratio corresponding to the scaling factor which should be applied to the trajectory coordinates.
#' (e.g., size in cm / size in pixels; see \code{\link[trajr]{TrajScale}}.
#'
#' @param segL A numeric value expressed in the unit specified by user and corresponding
#' to the length of the resampling step needed to discretize the input trajectory (optional)
#' see \code{\link[trajr]{TrajRediscretize}}.
#'
#' @param timeCol A character string corresponding to the name of the column containing Time information (e.g., "frame").
#'
#' @param compass.direction A value used to specify the compass direction (in radians). If not NULL, turning angles are calculated for a directed walk, otherwise, a random walk is assumed (default = NULL).
#'
#' @return This function returns a value of sinuosity for a given tracklet according to \code{\link[trajr]{TrajSinuosity2}} from the \code{\link[trajr]{trajr}} package.
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link[trajr]{TrajSinuosity2}}
#'
#' @references Benhamou, S. (2004). How to reliably estimate the tortuosity of an animal's path. Journal of Theoretical Biology, 229(2), 209-220. doi:10.1016/j.jtbi.2004.03.016.
#'
#' @examples
#'
#' set.seed(2023)
#' # generate a dummy tracklet
#' ## start to specify some parameters to generate the tracklet
#' TrackL <-
#'   100 # the length of the tracklet or a sequence to randomly sample tracklet's length
#' 
#' TrackDatTemp <-
#'   trajr::TrajGenerate(sample(TrackL, 1), random = TRUE, fps = 1)
#' TrackDat <-
#'   data.frame(
#'     x.pos = TrackDatTemp[["x"]] - min(TrackDatTemp[["x"]]),
#'     y.pos = TrackDatTemp[["y"]] - min(TrackDatTemp[["y"]]),
#'     frame = TrackDatTemp[["time"]]
#'   )
#' 
#' # compute the sinuosity of the particle' trajectory
#' MoveR::sinuosity(TrackDat, scale = 1, timeCol = "frame")
#'
#' @export

sinuosity <- function(df,
                      scale = NULL,
                      segL = NULL,
                      timeCol = NULL,
                      compass = NULL) {
  if (is.null(MoveR::listGet(df, "x.pos"))) {
    stop(
      "x.pos column is missing or might be misspelled: x coordinates are needed to compute euclidian distance"
    )
  }
  if (is.null(MoveR::listGet(df, "y.pos"))) {
    stop(
      "x.pos column is missing or might be misspelled: x coordinates are needed to compute euclidian distance"
    )
  }
  if (is.null(scale)) {
    warning(
      "the scaling factor to be applied to the trajectory coordinates is missing, default is 1/1"
    )
    scale = 1 / 1
  }
  if (is.null(timeCol)) {
    stop(
      "timeCol argument is missing: the name of the column carying time information is needed to compute speed"
    )
  }
  if (is.null(MoveR::listGet(df, timeCol))) {
    stop(
      "timeCol argument is misspelled or is absent from the input df: the name of the column carying time information is needed to compute speed"
    )
  }
  
  trj <-
    trajr::TrajFromCoords(df[, c("x.pos", "y.pos", timeCol)],
                          timeCol = 3, 
                          spatialUnits = "NA",
                          timeUnits = "NA")
  trj <- trajr::TrajScale(trj, scale, units = "NA")
  if (is.null(segL)) {
    sinuosityRes <-
      trajr::TrajSinuosity2(trj, compass.direction = compass)
  } else if (!is.null(segL)) {
    if (inherits(try(trajr::TrajRediscretize(trj, segL), silent = TRUE)
                 , "try-error")) {
      sinuosityRes <- NA
      warning("The tracklet is shorter than segL, sinuosity returned NA, segL might be to high")
    } else if (!inherits(try(trajr::TrajRediscretize(trj, segL), silent = TRUE)
                         , "try-error")) {
      discretized <- trajr::TrajRediscretize(trj, segL)
      sinuosityRes <-
        trajr::TrajSinuosity2(trj, compass.direction = compass)
    }
  }
  return(sinuosityRes)
}
