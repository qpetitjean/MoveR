#' @title Compute turning angle.
#'
#' @description Given a data frames containing tracking information for a particle,
#' this function returns a vector containing the value of turning angle along the considered fragment.
#'
#'
#' @param df A data frame containing x, y coordinates columns named "x.pos", "y.pos" for a given fragment.
#'
#' @param TimeCol A character string corresponding to the name of the column containing Time information (e.g., "frame").
#'
#' @param unit A character string indicating whether the function should returns turning angle in radians or degrees.
#'
#' @param compass A value used to specify the compass direction (in radians). If not NULL, turning angles are calculated for a directed walk, otherwise, a random walk is assumed (default = NULL).
#'
#'
#' @return This function returns a vector containing the value of turning angle computed along a given fragment.
#'
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link[trajr]{TrajAngles}}
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
#'# compute the turning angle over the particle' trajectory
#'turnAngle(fragDat, TimeCol = "frame", unit = "radians")
#'
#' @export

turnAngle <-
  function(df,
           TimeCol = NULL,
           unit = c("radians", "degrees"),
           compass = NULL,
           scale = NULL) {
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
    turnAngle <-
      trajr::TrajAngles(trj, compass.direction = compass, lag = 1)
    turnAngle <- base::append(turnAngle, NA, after = 0)
    turnAngle <-
      base::append(turnAngle, NA, after = length(turnAngle))
    
    if (length(unit) > 1) {
      warning("Unit argument is not specified, default value is radians")
      turnAngle <- turnAngle
    } else if (!unit == "degrees" & !unit == "radians") {
      stop("Unit argument seems misspelled, choose either radians or degrees")
    } else if (unit == "radians") {
      turnAngle <- turnAngle
    } else if (unit == "degrees") {
      turnAngle <- turnAngle * 180 / pi
    }
    return(turnAngle)
  }