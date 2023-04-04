#' @title Convert a list of variables to list of tracklets.
#'
#' @description Given a list containing vectors corresponding to the various variable of tracking data 
#' the function returns a list of data frames corresponding to the data for each tracklet based on tracklets identity.
#'
#' @param trackDatList A list of vector corresponding to the variable characterizing the tracking data.
#'
#' @param by A character vector identifying the tracklets to join by.
#'
#' @return A list of data frames corresponding the tracking data for each tracklets.
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link{convert2List}}
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
#'  # convert the list of tracklets to a simple list of variables
#'  trackDatList <- MoveR::convert2List(TrackList)
#'  
#'  # convert back the list of variables to a list of tracklet based on tracklets identity
#'  trackDat <- MoveR::convert2Tracklets(trackDatList, by = "trackletId")
#'  str(trackDat) # display only the three first elements (dataframes) of the list
#'
#' @export

convert2Tracklets <- function(trackDatList, by = NULL) {
  # convert trackDatList to a dataframe
  if (is.null(by)) {
    stop("by argument is missing: impossible to join tracklets without an identifier")
  }
  if (inherits(try(as.data.frame(trackDatList), silent = T)
               , "try-error")) {
    diff <-
      unlist(lapply(trackDatList, length))[!duplicated(unlist(lapply(trackDatList, length)))]
    sim <-
      unlist(lapply(trackDatList, length))[duplicated(unlist(lapply(trackDatList, length)))]
    diff <- diff[!diff %in% sim]
    stop(
      "The input contains lists with different length: consider removing ",
      deparse(names(diff))
    )
  } else {
    trackDatDf <- as.data.frame(trackDatList)
  }
  # convert the dataframe trackDatDf to a list of tracklets
  trackDat <-
    split(trackDatDf, listGet(trackDatDf, by))
  
  return(trackDat)
}
