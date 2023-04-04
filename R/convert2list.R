#' @title Convert a list of tracklets to list of variables.
#'
#' @description Given a list containing 1 or more data frames containing the data for each tracklets
#' the function reduce the list by concatenating the tracklets data frame into a list based on the variables 
#' included within each tracklet data frame. I also add the identity of the tracklets as a new variable named "trackletId".
#'
#' @param trackDat A list of data frame containing tracking information for each tracklet (e.g., x.pos, y.pos, frame).
#'
#' @return A list of vectors corresponding to the variables retrieved from the tracklets.
#'
#' @author Quentin PETITJEAN
#' 
#' @seealso \code{\link{convert2Tracklets}}
#'
#' @examples
#'
#' set.seed(2023)
#' # generate some dummy tracklets
#' ## start to specify some parameters to generate tracklets
#' TrackN <- 3 # the number of tracklet to simulate
#' TrackL <-
#'   1:1000 # the length of the tracklets or a sequence to randomly sample tracklet length
#' id <- 0
#' TrackList <- stats::setNames(lapply(lapply(seq(TrackN), function(i)
#'   trajr::TrajGenerate(sample(TrackL, 1), random = TRUE, fps = 1)), function(j) {
#'     id <<- id + 1
#'     data.frame(
#'       x.pos = j$x - min(j$x),
#'       y.pos = j$y - min(j$y),
#'       frame = j$time,
#'      identity = paste("Tracklet", id, sep = "_")
#'     )
#'   }), seq(TrackN))
#' 
#' # convert the list of tracklets to a simple list of variables
#'  trackDatList <- MoveR::convert2List(TrackList)
#'  str(trackDatList)
#'
#' @export

convert2List <- function(trackDat) {
  # convert the tracklets' list to a data frame and add the tracklet id
  trackDatdf <- do.call("rbind", trackDat)
  trackDatdf[["trackletId"]] <-
    gsub("[.][0-9]*", "", rownames(trackDatdf))
  # then transform the data frame to a list
  trackDatList <- as.list(trackDatdf)
  return(trackDatList)
} 