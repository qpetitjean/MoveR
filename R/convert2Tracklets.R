#' @title Convert a list of variables to list of tracklets.
#'
#' @description Given a list containing vectors corresponding to the various variable of tracking data 
#' the function returns a list of data frames corresponding to the data for each tracklet based on tracklets identity.
#'
#' @param trackDatList A list of vector corresponding to the variable of tracking data.
#'
#' @param by A character vector identifying tracklets to join by.
#'
#' @return A list of data frames corresponding the the tracking data for each tracklets.
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link{convert2List}}
#'
#' @examples
#'
#'# generate some dummy tracklets
#'## start to specify some parameters to generate tracklets
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
#'# convert fragList to a simple list based on the variables present within each tracklets' data frame
#'trackDatList <- convert2List(fragsList)
#'
#'# convert the list of variable to a liost of tracklet based on tracklets identity
#'trackDat <- convert2Tracklets(trackDatList, by = "fragsId")
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
