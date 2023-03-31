#' @title Mirror y coordinates.
#'
#' @description Given a vector containing the y coordinates of detected particles and image height,
#' this function returns mirrored y coordinates.
#'
#' @param YCoords A vector containing y coordinates for some particles.
#' 
#' @param imgHeight A numeric value expressed in pixels, the length of Y axis
#' corresponding to the height of the image or video resolution (optional, only used when flipY = TRUE).
#'
#'
#' @return A vector containing mirrored Y coordinates.
#'
#' @author Quentin PETITJEAN
#'
#' @examples
#'
#'# generate some dummy tracklets
#' ## start to specify some parameters to generate tracklets
#'Fragn <- 1 # the number of tracklet to simulate
#'FragL <- 10 # the length of the tracklets or a sequence to randomly sample tracklet length
#'
#'fragsList <- stats::setNames(lapply(lapply(seq(Fragn), function(i)
#'  trajr::TrajGenerate(sample(FragL, 1), random = TRUE, fps = 1)), function(j)
#'    data.frame(
#'      x.pos = j$x - min(j$x),
#'      y.pos = j$y - min(j$y),
#'      frame = j$time
#'    )), seq(Fragn))
#' 
# convert fraglist to a simple list to extract image resolution and ease computation on y coordinates
#' trackDatList <- convert2List(fragsList)
#' 
# find the maximum value on the y axis (corresponding to image Height)
#' imgHeight <- max(trackDatList[["y.pos"]], na.rm = T)
#' 
#' # use flipYCoords to mirror y coords
#' trackDatList[["y.pos"]] <-
#'   flipYCoords(trackDatList[["y.pos"]], imgHeight = imgHeight)
#' 
#' # convert the new dataset to a list of tracklet
#' fragsListMirrored <- convert2Tracklets(trackDatList, by = "frags_id")
#' 
#' # draw the result
#' # here we can see that both trajectory are mirrored, with the original trajectory 
#' # drawed in black and the mirrored one in red
#' drawTracklets(
#'   setNames(c(fragsList,fragsListMirrored), c(1:2)),
#'   imgRes = c(max(trackDatList[["x.pos"]], na.rm = T), imgHeight), 
#'   colFrag = T)
#'
#' @export

flipYCoords <- function(YCoords, imgHeight = NA) {
  if(is.na(imgHeight)){ 
    stop("imgHeight argument is missing, the height of the image resolution is needed to mirror y coordinates")
    }
  flipY <- imgHeight - YCoords
  return(flipY)
}
