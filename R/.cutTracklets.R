#' @title Cut tracklets.
#'
#' @description Given a list of data frames containing tracking information for each tracklet the function
#' returns the tracklet part(s) that meet the condition specified in the customFunc.
#'
#' @param trackDat A list of data frames containing tracking information for each tracklet or a data frame containing tracking information for one given tracklet.
#'
#' @param customFunc A function used to cut/subset the tracklets.
#'
#' @return An object of class "tracklets" containing a list of tracklets with tracking informations. The returned tracklets are shortened according to
#' the conditions specified by the customFunc argument; the function is not exported.
#'
#'
#' @author Quentin PETITJEAN
#'
#' @examples
#'
#' set.seed(2023)
#' # generate some dummy tracklets
#' ## start to specify some parameters to generate tracklets
#' TrackN <- 20 # the number of tracklet to simulate
#' TrackL <- 1:1000 # the length of the tracklets or a sequence to randomly sample tracklet length
#' 
#' TrackList <- MoveR::trackletsClass(stats::setNames(lapply(lapply(seq(TrackN), function(i)
#'   trajr::TrajGenerate(sample(TrackL, 1), random = TRUE, fps = 1)), function(j)
#'     data.frame(
#'       x.pos = j$x - min(j$x),
#'       y.pos = j$y - min(j$y),
#'       frame = j$time
#'     )), seq(TrackN)))
#'
#' # example 1 :
#' ## cut the tracklets by keeping only the part drawn during the first 25 seconds of the video
#' ### create a time sequence from 0 to 25 seconds (25 seconds * 25 frames)
#' TimeSeq <- seq(0, 25 * 1, by = 1) 
#' ### cut the tracklets
#' trackDatSub <- .cutTracklets(TrackList,
#'                       customFunc = function(x)
#'                       x[["frame"]] %in% TimeSeq)
#' 
#' layout(matrix(c(1,2),2,2, byrow=TRUE), widths=c(1,1))
#' par(mar=c(c(5, 4, 4, 0.5) + 0.1))
#' ### draw the tracklets before the cut
#' MoveR::drawTracklets(TrackList,
#'                  cex.leg = 0.8,
#'                  main = "")
#' 
#' ### draw the tracklets after the cut (keep only the first 25 seconds of the video)
#' MoveR::drawTracklets(trackDatSub,
#'                  cex.leg = 0.8,
#'                  main = "")
#'

.cutTracklets <- function(trackDat, customFunc) {
  if (class(trackDat) == "data.frame") {
    trackDat <- list(trackDat)
  }
  if (is.null(names(trackDat))) {
    names(trackDat) <- seq(length(trackDat))
  }
  # identify part of tracklets detected in the selected Time interval
  When <-
    lapply(trackDat, customFunc)
  # identify which tracklets are detected in the selected Time interval
  Who <-
    which(unlist(lapply(When, function(y)
      TRUE %in% y)) == TRUE)
  # isolate part of detected tracklets included in time interval
  WhoWhen <-
    lapply(When[c(names(Who))], function (z)
      which(z == TRUE))
  # store the selected tracklets part in a list
  output <- stats::setNames(lapply(names(WhoWhen), function(w)
    trackDat[[w]][c(WhoWhen[[w]]),]), names(WhoWhen))
  output <- trackletsClass(output)
  
  if(inherits(trackDat, "tracklets")){
    storedInfo <- MoveR::getInfo(trackDat)
    output <-  MoveR::setInfo(output, storedInfo[[1]],  storedInfo[[2]],  storedInfo[[3]])
  }

  return(output)
}
