#' @title Convert a list of fragments to list of variables
#'
#' @description Given a list containing 1 or more data frames corresponding to the data for each fragments
#' the function reduce the list by concatenating fragments' data based on the variables present within each fragment data frame 
#' and add fragment identity as a new variable.
#'
#'
#' @param trackDat A list of data frame containing tracking information for each fragment (e.g., x.pos, y.pos, frame).
#'
#' @return A list of vectors corresponding to the variable retrieved from the fragments.
#'
#' @authors Quentin PETITJEAN
#' 
#' @seealso \code{\link{convert2frags}}
#'
#' @examples
#'
#' #'# generate some dummy fragments
#' ## start to specify some parameters to generate fragments
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
#'# convert frag list to a simple list to extract image resolution for generated fragments
#' trackDatList <- convert2list(fragsList)
#'
#' @export

convert2list <- function(trackDat) {
  # convert the fragments' list to a data frame and add the fragment id
  trackDatdf <- do.call("rbind", trackDat)
  trackDatdf[["fragsId"]] <-
    gsub("[.][0-9]*", "", rownames(trackDatdf))
  # then transform the data frame to a list
  trackDatList <- as.list(trackDatdf)
  return(trackDatList)
} 