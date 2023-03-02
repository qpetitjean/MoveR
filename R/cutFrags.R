#' @title Cut tracklets.
#'
#' @description Given a list of data frames containing tracking information for each tracklet the function
#' returns the tracklet part(s) that meet the condition specified in the customFunc.
#'
#' @param trackDat A list of data frames containing tracking information for each tracklet or a data frame containing tracking information for one given tracklet.
#'
#' @param customFunc A function used to cut/subset the tracklets.
#'
#' @return A list of data frames containing subsetted tracking information for each tracklet according to
#' the conditions specified by the customFunc argument.
#'
#'
#' @author Quentin PETITJEAN
#'
#' @examples
#'
#' # Download the first dataset from the sample data repository
#' Path2Data <- MoveR::dlSampleDat(dataSet = 1, tracker = "TRex")
#' Path2Data
#'
#' # Import the list containing the 9 vectors classically used for further computation
#' Data <- MoveR::readTrex(Path2Data[[1]])
#'
#' # convert it to a list of tracklets
#' trackDat <- MoveR::convert2frags(Data[1:7], by = "identity")
#'
#' # example 1 :
#' ## cut the tracklets by keeping only the part drawn during the first 25 seconds of the video
#' ### create a time sequence from 0 to 25 seconds (25 seconds * 25 frames)
#' TimeSeq <- seq(0, 25 * 25, by = 1) 
#' ### cut the tracklets
#' trackDatSub <- MoveR::cutFrags(trackDat,
#'                       customFunc = function(x)
#'                       x[["frame"]] %in% TimeSeq)
#'
#' par(mfrow = c(1,2))
#' ### draw the tracklets before the cut
#' dMoveR::rawFrags(trackDat,
#'                  cex.leg = 0.8,
#'                  main = "")
#'
#' ### draw the tracklets after the cut (keep only the first 25 seconds of the video)
#' MoveR::drawFrags(trackDatSub,
#'                  cex.leg = 0.8,
#'                  main = "")
#'
#' # example 2 :
#' ## cut the tracklets to remove parts at the edge of the arena
#' ### load a reference dataset (A matrix or dataframe or path to a file (either .txt or .csv) containing a distance matrix to any object or
#' ### the location of one or several areas of interest (here we have created a distance map using ImageJ)
#' refDat <- as.matrix(read.delim(
#'                     Path2Data[[2]],
#'                     dec = "."
#'                    ))
#'   
#' ###  retrieve the value of the edge limit (1) and of the center limit (254) to plot them
#' arenaEdge <- data.frame(which(refDat == 1, arr.ind=T))
#' arenaCenter <- data.frame(which(refDat == 254, arr.ind=T))
#'
#' ### then specify that all values above 254 are considered as the center
#' refDat[which(as.numeric(refDat) > 254)] <- "center"
#' 
#' ### rather all values above 0 and below or equal to 254 are considered as within the border
#' refDat[which(as.numeric(refDat) >= 0 & as.numeric(refDat) <= 254)] <- "edge"
#'
#' ### retrieve the area where the particles are located over their whole trajectory using the locaPos function
#' trackDat <- MoveR::analyseFrags(trackDat,
#'                                 customFunc = list(
#'                                 Position = function(x)
#'                                 MoveR::locaPos(refDat, x)
#'                                 ))
#'
#' ### cut the tracklets to remove every part that are on the edge of the arena
#' trackDatSub2 <- MoveR::cutFrags(trackDat,
#'                                 customFunc = function(x)
#'                                 x[["Position"]] != "edge")
#'
#' par(mfrow = c(1,2))
#' ### draw the tracklets before the cut:
#' ### tracklet parts that are on the edge of the arena colored in red and those at the center in black
#' MoveR::drawFrags(
#'                  trackDat,
#'                  add2It = list(
#'                                points(x = arenaEdge[, 2], y = arenaEdge[, 1], cex = 0.01),
#'                                points(x = arenaCenter[, 2], y = arenaCenter[, 1], cex = 0.01)
#'                                ),
#'                  colId = "Position"
#'                  )
#'                  
#' ### draw the tracklets after the cut (remove parts of the tracklet that are in the edge)
#' MoveR::drawFrags(
#'                  trackDatSub2,
#'                  add2It = list(
#'                                points(x = arenaEdge[, 2], y = arenaEdge[, 1], cex = 0.01),
#'                                points(x = arenaCenter[, 2], y = arenaCenter[, 1], cex = 0.01)
#'                                ),
#'                  colId = "Position"
#'                  )
#'
#' @export

cutFrags <- function(trackDat, customFunc) {
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
  return(output)
}
