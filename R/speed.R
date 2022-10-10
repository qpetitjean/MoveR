#' @title Compute particle speed over a trajectory
#'
#'
#' @description Given a data frames containing tracking informations for a given fragment,
#' this function scale fragment path and returns a vector containing the value of
#' speed along this fragment.
#'
#'
#' @param df A data frame containing x and y coordinates in columns named "x.pos", "y.pos" for a given fragment, as well as
#' a column containing time information, whatever the unit, over the fragment.
#'
#' @param scale A ratio corresponding to the scaling factor to be applied to the trajectory coordinates
#' (e.g., size in cm / size in pixels; see trajr::TrajScale()).
#'
#' @param unit The unit expected after scaling (e.g., "cm", "m", ...).
#'
#' @param TimeCol A character string corresponding to the name of the column containing Time information (e.g., "frame").
#'
#' @return This function returns a vector containing the value of speed along a given fragment.
#'
#'
#' @authors Quentin PETITJEAN
#'
#' @examples
#'
#'# generate a dummy fragment
#'## start to specify some parameters to generate the fragment
#'FragL <- 100 # the length of the fragment or a sequence to randomly sample fragment length
#'
#'fragDatTemp <- trajr::TrajGenerate(sample(FragL, 1), random = TRUE, fps = 1)
#'fragDat <- data.frame(
#'  x.pos = fragDatTemp[["x"]] - min(fragDatTemp[["x"]]),
#'  y.pos = fragDatTemp[["y"]] - min(fragDatTemp[["y"]] ),
#'  frame = fragDatTemp[["time"]]
#')
#'
#'# compute the speed of the particle along its trajectory, here we consider that the space unit is the pixels,
#'# expressing the speed as pixels/frame
#'fragDat[["speedFrame"]] <- speed(fragDat, scale = 1, TimeCol = "frame", unit = "pixels")
#'
#'# to compute the speed according to another time unit, a new column containing the new timeline is needed
#'
#'fragDat[["second"]] <- fragDat[["frame"]]/25 # here we consider that the frame rate is 25 frame per second
#'
#'# then compute the speed of the particle along its trajectory according to the new time unit
#'
#'fragDat[["speedSec"]] <- speed(fragDat, scale = 1/1, TimeCol = "second", unit = "pixels")
#'
#'str(fragDat)
#'
#'# it is also possible to resample the fragment before computing speed, here every 10 time unit (i.e., frame)
#'
#'sampledFragDat <- resampleFrags(fragDat, TimeCol = "frame",  Tstep = 10)
#'
#'# and then compute the speed of the particle along its trajectory
#'
#'sampledFragDat[["speed"]] <- speed(sampledFragDat, scale = 1/1, TimeCol = "frame", unit = "pixels")
#'
#'str(sampledFragDat)
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