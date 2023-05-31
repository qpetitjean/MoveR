#' @title Create and test a custom filter on tracklets.
#'
#' @description Given a list of data frames containing tracking information for each tracklet, 
#' this function returns a list of vector containing the result of a user defined condition test for each tracklet.
#'
#' @param trackDat A list of data frames containing tracking information for each tracklet (e.g., x.pos, y.pos, frame).
#'
#' @param toFilter An element of the tracklet's data frame (a column name) from which condition will be verified.
#' 
#' @param customFunc A custom function containing condition(s) to be applied to toFilter argument.
#'
#' @return A list of vector of the same length than the tracklets list containing the result of a user specified condition test for each tracklet.
#'
#' @author Quentin PETITJEAN
#' 
#' @seealso \code{\link{filterTracklets}}
#'
#' @examples
#' \dontrun{
#' 
#' # Download the first dataset from the sample data repository
#' Path2Data <- MoveR::DLsampleData(dataSet = 1, tracker = "TRex")
#' Path2Data
#'
#' # Import the list containing the 9 vectors classically used for further computation
#' Data <- MoveR::readTrex(Path2Data[[1]])
#'
#' # convert it to a list of tracklets
#' trackDat <- MoveR::convert2Tracklets(Data[1:7], by = "identity")
#'
#' # example 1: test for the presence of infinite value in x.pos, if infinite values are detected, the result is TRUE 
#' CondTest1 <- MoveR::filterFunc(trackDat, toFilter = "x.pos", customFunc = function(x) is.infinite(x))
#' str(CondTest1)
#' 
#' # example 2: test for the length of the particles, if particles size is ranging between 1 and 20 pixels, the result is TRUE 
#' CondTest2 <- MoveR::filterFunc(trackDat, toFilter = "maj.ax", customFunc = function(x) x >= 1 & x <= 20)
#' str(CondTest1)
#'
#' }
#' @export

filterFunc <- function(trackDat,
                      toFilter = NULL,
                      customFunc = NULL) {
  filter <- list()
  if (!is.null(customFunc)) {
    if (!is.null(toFilter)) {
      for (i in seq(length(trackDat))) {
        selCol <- listGet(trackDat[[i]], toFilter)
        if (is.null(selCol)) {
          stop(
            "toFilter argument is not found in the provided dataset, toFilter might be misspelled"
          )
        } else {
          filter[[i]] <-
            ifelse(customFunc(selCol), TRUE, FALSE)
        }
      }
    } else {
      stop(
        "toFilter argument is missing, impossible to test condition specified in customFunc"
      )
    }
  } else {
    stop("customFunc argument is missing, no condition to test")
  }
  names(filter) <- names(trackDat)
  return(filter)
}
