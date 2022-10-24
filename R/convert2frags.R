#' @title Convert a list of variables to list of fragments.
#'
#' @description Given a list containing vectors corresponding to the various variable of tracking data 
#' the function returns a list of data frames corresponding to the data for each fragment based on fragments identity.
#'
#' @param trackDatList A list of vector corresponding to the variable of tracking data.
#'
#' @param by A character vector identifying fragments to join by.
#'
#' @return A list of data frames corresponding the the tracking data for each fragments.
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link{convert2list}}
#'
#' @examples
#'
#'# generate some dummy fragments
#'## start to specify some parameters to generate fragments
#'Fragn <- 10 # the number of fragment to simulate
#'FragL <- 1:1000 # the length of the fragments or a sequence to randomly sample fragment length
#'
#'fragsList <- stats::setNames(lapply(lapply(seq(Fragn), function(i)
#'  trajr::TrajGenerate(sample(FragL, 1), random = TRUE, fps = 1)), function(j)
#'    data.frame(
#'      x.pos = j$x - min(j$x),
#'      y.pos = j$y - min(j$y),
#'      frame = j$time
#'    )), seq(Fragn))
#'
#'# convert fragList to a simple list based on the variables present within each fragments' data frame
#'trackDatList <- convert2list(fragsList)
#'
#'# convert the list of variable to a liost of fragment based on fragments identity
#'trackDat <- convert2frags(trackDatList, by = "fragsId")
#'
#' @export

convert2frags <- function(trackDatList, by = NULL) {
  # convert trackDatList to a dataframe
  if (is.null(by)) {
    stop("by argument is missing: impossible to join fragments without an identifier")
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
  # convert the dataframe trackDatDf to a list of fragments
  trackDat <-
    split(trackDatDf, listGet(trackDatDf, by))
  
  return(trackDat)
}
