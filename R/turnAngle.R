#' @title Compute particle's turning angle over a trajectory.
#'
#' @description Given a data frames containing tracking information for a particle,
#' this function returns a vector containing the value of turning angle along the considered tracklet.
#'
#'
#' @param df A data frame containing x, y coordinates columns named "x.pos", "y.pos" for a given tracklet.
#'
#' @param timeCol A character string corresponding to the name of the column containing Time information (default = 'frame').
#'
#' @param unit A character string indicating whether the function should returns turning angle in radians or degrees (default = "radians").
#'
#' @param compass A value used to specify the compass direction (in radians). If not NULL, turning angles are calculated for a directed walk, otherwise, a random walk is assumed (default = NULL).
#'
#' @param scale A ratio corresponding to the scaling factor to be applied to the trajectory coordinates
#' (e.g., size in cm / size in pixels; see \code{\link[trajr]{TrajScale}, default = 1}.
#'
#' @return This function returns a vector containing the values of turning angle over a trajectory.
#'
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link[trajr]{TrajAngles}}
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
#' # compute the turning angle over the particle's trajectory
#' MoveR::turnAngle(TrackDat,
#'                  timeCol = "frame",
#'                  unit = "radians",
#'                  scale = 1)
#'
#' @export

turnAngle <-
  function(df,
           timeCol = 'frame',
           unit = c("radians", "degrees"),
           compass = NULL,
           scale = 1) {

    error <- .errorCheck(df = df, x.pos = "x.pos", y.pos = "y.pos" , timeCol = timeCol)
    if(!is.null(error)){
      stop(error)
    }
    
    trj <-
      trajr::TrajFromCoords(df[, c("x.pos", "y.pos", timeCol)],
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
      unit <- "radians"
      turnAngle <- turnAngle
    } else if (!unit == "degrees" & !unit == "radians") {
      stop("[unit] argument seems misspelled, choose either radians or degrees")
    } else if (unit == "radians") {
      turnAngle <- turnAngle
    } else if (unit == "degrees") {
      turnAngle <- turnAngle * 180 / pi
    }
    return(turnAngle)
  }
