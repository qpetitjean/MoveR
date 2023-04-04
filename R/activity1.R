#' @title Determine active or inactive states according to the speed of a particles along its trajectory.
#'
#' @description Given a data frames containing tracking information for a given tracklet including speed, 
#' this function return a vector containing character strings indicating whether the particle is "active" or "inactive".
#'
#' @param df A data frame containing x and y coordinates in columns named "x.pos", "y.pos" for a given tracklet, as well as 
#' a column containing the speed of the particle, whatever the unit, over the tracklet.
#' 
#' @param speedCol A character string corresponding to the name of the column containing the speed of the particles over the tracklet, whatever the unit.
#' 
#' @param minSpeed A numeric value expressed in the same unit than speed, corresponding to the threshold above which the particle 
#' is considered as active.
#'
#' @return This function returns a vector containing character strings indicating whether the particle is "active" or "inactive".
#'
#' @author Quentin PETITJEAN
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
#' TrackDat <- data.frame(
#'   x.pos = TrackDatTemp[["x"]] - min(TrackDatTemp[["x"]]),
#'   y.pos = TrackDatTemp[["y"]] - min(TrackDatTemp[["y"]]),
#'   frame = TrackDatTemp[["time"]]
#' )
#' 
#' # compute the speed of the particle along its trajectory
#' TrackDat[["speed"]] <-
#'   MoveR::speed(TrackDat, scale = 1, timeCol = "frame")
#' 
#' # we can then define the speed treshold above which the particle is considered actives using quantiles
#' # here we use the 0.025 quantile to find the minimum speed treshold
#' hist(log10(TrackDat[["speed"]][-is.na(TrackDat[["speed"]])]))
#' tresh <-
#'   quantile(log10(TrackDat[["speed"]][-is.na(TrackDat[["speed"]])]), 0.025)
#' abline(v = tresh)
#' 
#' TrackDat[["activity1"]] <-
#'   MoveR::activity1(TrackDat, speedCol = "speed", minSpeed = 10 ^ tresh)
#' 
#' # draw the particle' trajectory and spot the inactive moments using red dots
#' MoveR::drawTracklets(list(TrackDat),
#'                  add2It = list(points(
#'                    TrackDat[["x.pos"]][which(TrackDat[["activity1"]] == "inactive")],
#'                    TrackDat[["y.pos"]][which(TrackDat[["activity1"]] == "inactive")],
#'                    col = "red",
#'                    pch = 19,
#'                    cex = 1.5
#'                  )))
#'
#' @export

activity1 <- function (df, speedCol = NULL, minSpeed = NULL) {
  if(is.null(MoveR::listGet(df, "x.pos"))){
    stop(
      "x.pos column is missing or might be misspelled: x coordinates are needed to compute euclidian distance"
    )
  }
  if(is.null(MoveR::listGet(df, "y.pos"))){
    stop(
      "x.pos column is missing or might be misspelled: x coordinates are needed to compute euclidian distance"
    )
  }
  if (is.null(speedCol)) {
    stop(
      "speedCol argument is missing: the name of the column carying speed of the particle along its trajectory is needed to determine activity state"
    )}
  if(is.null(MoveR::listGet(df, speedCol))){
    stop(
      "speedCol argument is misspelled or is absent from the input df: the name of the column carying speed of the particle along its trajectory is needed to determine activity state"
    )}
  if (is.null(minSpeed)) {
    stop(
      "minSpeed argument is missing: impossible to determine whether the particle is active or not without a minimum speed treshold"
    )
  } else if (!is.null(minSpeed)) {
    activeRes <- rep(NA, nrow(df))
    activeRes[which(df[[speedCol]] > minSpeed)] <- "active"
    activeRes[which(df[[speedCol]] < minSpeed)] <- "inactive"
  }
  return(activeRes)
}
