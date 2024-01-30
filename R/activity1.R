#' @title Determine active or inactive states according to the speed of a particles along its trajectory.
#'
#' @description Given a data frames containing tracking information for a given tracklet including speed, 
#' this function return a vector containing numeric values indicating whether the particle is "active" (1) or "inactive" (0).
#'
#' @param df A data frame containing x and y coordinates in columns named "x.pos", "y.pos" for a given tracklet, as well as 
#' a column containing the speed of the particle, whatever the unit, over the tracklet.
#' 
#' @param speedCol A character string corresponding to the name of the column containing the speed of the particles over the tracklet, whatever the unit.
#' 
#' @param minSpeed A numeric value expressed in the same unit than speed, corresponding to the threshold above which the particle 
#' is considered as active.
#'
#' @return This function returns a vector containing numeric values indicating whether the particle is "active" (1) or "inactive" (0).
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
#'   MoveR::speed(TrackDat)
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
#' MoveR::drawTracklets(MoveR::trackletsClass(list(TrackDat)),
#'                  add2It = list(points(
#'                    TrackDat[["x.pos"]][which(TrackDat[["activity1"]] == 0)],
#'                    TrackDat[["y.pos"]][which(TrackDat[["activity1"]] ==  0)],
#'                    col = "red",
#'                    pch = 19,
#'                    cex = 1.5
#'                  )))
#'
#' @export

activity1 <- function (df,
                       speedCol = NULL,
                       minSpeed = NULL) {
  
  error <- .errorCheck(df = df, x.pos = "x.pos", y.pos = "y.pos", speedCol = speedCol, minSpeed = minSpeed)
  if(!is.null(error)){
    stop(error)
  }

  if (!is.null(minSpeed)) {
    activeRes <- rep(NA, nrow(df))
    activeRes[which(df[[speedCol]] > minSpeed)] <- 1
    activeRes[which(df[[speedCol]] < minSpeed)] <- 0
  }
  return(activeRes)
}
