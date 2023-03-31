#' @title Compute particle's speed over a trajectory.
#'
#' @description Given a data frames containing tracking information for a given tracklet,
#' this function scale tracklet path and returns a vector containing the value of
#' speed along this tracklet.
#'
#'
#' @param df A data frame containing x and y coordinates in columns named "x.pos", "y.pos" for a given tracklet, as well as
#' a column containing time information, whatever the unit, over the trajectory.
#'
#' @param scale A ratio corresponding to the scaling factor to be applied to the trajectory coordinates
#' (e.g., size in cm / size in pixels; see \code{\link[trajr]{TrajScale}}.
#'
#' @param TimeCol A character string corresponding to the name of the column containing Time information (e.g., "frame").
#'
#' @return This function returns a vector containing the values of speed over a trajectory.
#'
#'
#' @author Quentin PETITJEAN
#' 
#' @seealso \code{\link[trajr]{TrajDerivatives}}
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
#' # compute the speed of the particle along its trajectory,
#' # expressing the speed as pixels/frame
#' TrackDat[["speedFrame"]] <-
#'   MoveR::speed(TrackDat, scale = 1, TimeCol = "frame")
#' 
#' # to compute the speed according to another time unit, a new column containing the new timeline is needed
#' # here we consider that the frame rate is 25 frame per second
#' TrackDat[["second"]] <- TrackDat[["frame"]] / 25
#' 
#' # then compute the speed of the particle over its trajectory according to the new time unit
#' TrackDat[["speedSec"]] <-
#'   MoveR::speed(TrackDat, scale = 1, TimeCol = "second")
#' 
#' str(TrackDat)
#' 
#' # it is also possible to resample the tracklet before computing speed, here every 10 time unit (i.e., frame)
#' sampledFragDat <-
#'   MoveR::resamplTracklets(TrackDat, TimeCol = "frame",  Tstep = 10)
#' 
#' # and then compute the speed of the particle over its trajectory
#' sampledFragDat[["speed"]] <-
#'   MoveR::speed(sampledFragDat, scale = 1, TimeCol = "frame")
#' 
#' str(sampledFragDat) 
#'
#' @export

speed <- function(df,
                  scale = NULL,
                  TimeCol = NULL) {
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
  if (is.null(TimeCol)) {
    stop(
      "TimeCol argument is missing: the name of the column carying time information is needed to compute speed"
    )
  }
  if (is.null(MoveR::listGet(df, TimeCol))) {
    stop(
      "TimeCol argument is misspelled or is absent from the input df: the name of the column carying time information is needed to compute speed"
    )
  }
  
  trj <-
    trajr::TrajFromCoords(df[, c("x.pos", "y.pos", TimeCol)],
                          timeCol = 3, 
                          spatialUnits = "NA",
                          timeUnits = "NA")
  trj <- trajr::TrajScale(trj, scale, units = "NA")
  trjderiv <- trajr::TrajDerivatives(trj)
  speedRes <-  base::append(trjderiv[[1]], NA, after = 0)
  return(speedRes)
}
