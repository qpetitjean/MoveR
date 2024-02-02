#' @title flip y coordinates.
#'
#' @description Given a vector containing the y coordinates of detected particles and image height,
#' this function returns flipped y coordinates.
#'
#' @param YCoords A vector containing y coordinates for some particles.
#' 
#' @param imgHeight A numeric value expressed in pixels indicating the length of the Y axis (i.e., the height of the image or video).
#'
#'
#' @return A vector of the same length than the input data (YCoords) containing flipped Y coordinates.
#'
#' @author Quentin PETITJEAN
#'
#' @examples
#'
#' set.seed(2023)
#' # generate some dummy tracklets
#' ## start to specify some parameters to generate tracklets
#' TrackN <- 1 # the number of tracklet to simulate
#' TrackL <-
#'   10000 # the length of the tracklets or a sequence to randomly sample tracklet length
#' id <- 0
#' TrackList <-  MoveR::trackletsClass(stats::setNames(lapply(lapply(seq(TrackN), function(i)
#'   trajr::TrajGenerate(sample(TrackL, 1), random = TRUE, fps = 1)), function(j) {
#'     id <<- id + 1
#'     data.frame(
#'       x.pos = j$x - min(j$x),
#'       y.pos = j$y - min(j$y),
#'       frame = j$time,
#'       identity = paste("Tracklet", id, sep = "_")
#'    )
#'   }), seq(TrackN)))
#' 
#' # convert the list of tracklets to a simple list of variables
#' trackDatList <- MoveR::convert2List(TrackList)
#' 
#' # find the maximum value on the y axis (corresponding to image Height)
#' imgHeight <- max(trackDatList[["y.pos"]], na.rm = T)
#' 
#' # use flipYCoords to flip y coords
#' trackDatList[["y.pos"]] <-
#'   MoveR::flipYCoords(trackDatList[["y.pos"]], imgHeight = imgHeight)
#'   
#' # rename the identity of the flipped tracklet by adding "bis"
#' trackDatList[["identity"]] <-
#'  gsub("$", "Bis", trackDatList[["identity"]])
#' 
#' # convert the new dataset to a list of tracklet
#'  trackDatList <- trackDatList[-length(trackDatList)]
#'  trackDatflipped <-
#'    MoveR::convert2Tracklets(trackDatList, by = "identity")
#' 
#' # draw the result
#' # here we can see that both trajectory are flipped, with the original trajectory
#' # drawed in black and the flipped one in red
#' MoveR::drawTracklets(MoveR::trackletsClass(c(TrackList, trackDatflipped)),
#'                      colId = "identity")
#'
#' @export

flipYCoords <- function(YCoords, imgHeight = NULL) {
  error <- .errorCheck(imgHeight = imgHeight)
  if(!is.null(error)){
    stop(error)
  }
  
  flipY <- imgHeight - YCoords
  return(flipY)
}
