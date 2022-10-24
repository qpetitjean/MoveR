#' @title Compute corrected rediscretized sinuosity index for a given fragment.
#'
#' @description Given a data frames containing tracking informations for a given fragment,
#' this function rediscretize fragment path and returns a vector containing the value of
#' sinuosity along this fragment.
#'
#'
#' @param df  A data frame containing x and y coordinates in columns named "x.pos", "y.pos" for a given fragment, as well as
#' a column containing time information, whatever the unit, over the fragment.
#'
#' @param scale A ratio corresponding to the scaling factor to be applied to the trajectory coordinates.
#' (e.g., size in cm / size in pixels; see \code{\link[trajr]{TrajScale}}.
#'
#' @param segL A numeric value expressed in the unit specified by user and corresponding
#' to the length of the resampling step needed to discretize the input trajectory (optional)
#' see \code{\link[trajr]{TrajRediscretize}}.
#'
#' @param TimeCol A character string corresponding to the name of the column containing Time information (e.g., "frame").
#'
#' @param compass.direction A value used to specify the compass direction (in radians). If not NULL, turning angles are calculated for a directed walk, otherwise, a random walk is assumed (default = NULL).
#'
#' @return This function returns a value of sinuosity for a given fragment according to TrajSinuosity2 function from trajR package.
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link[trajr]{TrajSinuosity2}}
#'
#' @references Benhamou, S. (2004). How to reliably estimate the tortuosity of an animal's path. Journal of Theoretical Biology, 229(2), 209-220. doi:10.1016/j.jtbi.2004.03.016.
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
#' y.pos = fragDatTemp[["y"]] - min(fragDatTemp[["y"]] ),
#'  frame = fragDatTemp[["time"]]
#')
#'
#'# compute the sinuosity of the particle' trajectory, here we consider that the space unit is the pixels
#'sinuosity(fragDat, scale = 1, TimeCol = "frame", unit = "pixels")
#'
#' @export

sinuosity <- function(df,
                      scale = NULL,
                      segL = NULL,
                      TimeCol = NULL,
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
  if (is.null(segL)) {
    sinuosityRes <-
      trajr::TrajSinuosity2(trj, compass.direction = compass)
  } else if (!is.null(segL)) {
    if (inherits(try(trajr::TrajRediscretize(trj, segL), silent = TRUE)
                 , "try-error")) {
      sinuosityRes <- NA
      warning("The fragment is shorter than segL, sinuosity returned NA, segL might be to high")
    } else if (!inherits(try(trajr::TrajRediscretize(trj, segL), silent = TRUE)
                         , "try-error")) {
      discretized <- trajr::TrajRediscretize(trj, segL)
      sinuosityRes <-
        trajr::TrajSinuosity2(trj, compass.direction = compass)
    }
  }
  return(sinuosityRes)
}