#' @title Mirror y coordinates.
#'
#' @description Given a vector containing the y coordinates of detected particles and image height,
#' this function returns mirrored y coordinates.
#'
#' @param YCoords A vector containing y coordinates for some particles.
#' 
#' @param imgHeight A numeric value expressed in pixels, the length of Y axis
#' corresponding to the height of the image or video resolution (optional, only used when mirrorY = TRUE).
#'
#'
#' @return A vector containing mirrored Y coordinates.
#'
#' @author Quentin PETITJEAN
#'
#' @examples
#'
#'# generate some dummy fragments
#' ## start to specify some parameters to generate fragments
#'Fragn <- 1 # the number of fragment to simulate
#'FragL <- 10 # the length of the fragments or a sequence to randomly sample fragment length
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
#' trackDatList <- convert2list(fragsList)
#' 
# find the maximum value on the y axis (corresponding to image Height)
#' imgHeight <- max(trackDatList[["y.pos"]], na.rm = T)
#' 
#' # use mirrorYFunc to mirror y coords
#' trackDatList[["y.pos"]] <-
#'   mirrorYFunc(trackDatList[["y.pos"]], imgHeight = imgHeight)
#' 
#' # convert the new dataset to a list of fragment
#' fragsListMirrored <- convert2frags(trackDatList, by = "trackId")
#' 
#' # draw the result
#' # here we can see that both trajectory are mirrored, with the original trajectory 
#' # drawed in black and the mirrored one in red
#' drawFrags(
#'   setNames(c(fragsList,fragsListMirrored), c(1:2)),
#'   imgRes = c(max(trackDatList[["x.pos"]], na.rm = T), imgHeight), 
#'   colFrag = T)
#'
#' @export

mirrorYFunc <- function(YCoords, imgHeight = NA) {
  if(is.na(imgHeight)){ 
    stop("imgHeight argument is missing, the height of the image resolution is needed to mirror y coordinates")
    }
  mirrorY <- imgHeight - YCoords
  return(mirrorY)
}
