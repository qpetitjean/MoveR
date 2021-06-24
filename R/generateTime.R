#' @title Generate time
#'
#' @description Given a vector containing the number of individuals detected in each frame (i.e., ntarget),
#' this function reconstruct times serie to recover actual time for replicated frame in the dataset
#'
#'
#' @param trackingData A list containing raw tracking datas 
#'
#'
#' @return A vector containing a sequence of frame number corresponding to the time serie
#'
#' @authors Quentin Petitjean, Vincent Calcagno
#'
#'
#'
#' @examples
#'
#' # TODO
#'
#' @export

generateTime <- function(trackingData) {
  ### simple approach using ntargets
  allTimes <- c()
  frame_out <- 0
  for (t in trackingData$ntargets) {
    allTimes <- c(allTimes, rep(frame_out, times = t))
    frame_out <-  frame_out + 1
  }
  return(allTimes)
}

