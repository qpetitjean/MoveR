#' @title Convert a list of tracklets to list of variables.
#'
#' @description Given a list containing 1 or more data frames corresponding to the data for each tracklets
#' the function reduce the list by concatenating tracklets' data based on the variables present within each tracklet data frame 
#' and add tracklet identity as a new variable.
#'
#'
#' @param trackDat A list of data frame containing tracking information for each tracklet (e.g., x.pos, y.pos, frame).
#'
#' @return A list of vectors corresponding to the variable retrieved from the tracklets.
#'
#' @author Quentin PETITJEAN
#' 
#' @seealso \code{\link{convert2Tracklets}}
#'
#' @examples
#'
#' #'# generate some dummy tracklets
#' ## start to specify some parameters to generate tracklets
#'Fragn <- 10 # the number of tracklet to simulate
#'FragL <- 1:1000 # the length of the tracklets or a sequence to randomly sample tracklet length
#'
#'fragsList <- stats::setNames(lapply(lapply(seq(Fragn), function(i)
#'  trajr::TrajGenerate(sample(FragL, 1), random = TRUE, fps = 1)), function(j)
#'    data.frame(
#'      x.pos = j$x - min(j$x),
#'      y.pos = j$y - min(j$y),
#'      frame = j$time
#'    )), seq(Fragn))
#'
#'# convert frag list to a simple list to extract image resolution for generated tracklets
#' trackDatList <- convert2List(fragsList)
#'
#' @export

convert2List <- function(trackDat) {
  # convert the tracklets' list to a data frame and add the tracklet id
  trackDatdf <- do.call("rbind", trackDat)
  trackDatdf[["fragsId"]] <-
    gsub("[.][0-9]*", "", rownames(trackDatdf))
  # then transform the data frame to a list
  trackDatList <- as.list(trackDatdf)
  return(trackDatList)
} 
