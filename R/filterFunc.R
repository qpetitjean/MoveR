#' @title Create and test a custom filter on tracking fragments.
#'
#' @description Given a list of data frames containing tracking information for each fragment, 
#' this function returns a list of vector containing the result of a user defined condition test for each fragment.
#'
#' @param trackDat A list of data frames containing tracking information for each fragment (e.g., x.pos, y.pos, frame).
#'
#' @param toFilter An element of the fragment's data frame (a column name) from which condition will be verified.
#' 
#' @param customFunc A custom function containing condition(s) to be applied to toFilter argument.
#'
#' @return A list of vector of the same length than the fragments list containing
#'  the result of a user specified condition test for each fragment.
#'
#' @author Quentin PETITJEAN
#' 
#' @seealso \code{\link{filterFrags}}
#'
#' @examples
#' 
#'# load the sample data
#'Data <-
#'  readTrex(
#'    system.file("sampleData/sample_1/TREXOutput", package = "MoveR"),
#'    mirrorY = T,
#'    imgHeight = 2160,
#'    rawDat = F
#'  )
#'# convert it to a list of fragments
#'trackDat <- convert2frags(Data[1:7], by = "identity")
#'
#'## Exemple 1: test for the presence of infinite value in x.pos,
#'   # if infinite values are detected, the result is TRUE 
#'  
#' filterFunc(trackDat, toFilter = "x.pos", customFunc = function(x) is.infinite(x))
#'
#'## Exemple 2: test for the length of the individuals, 
#' # if individual size is ranging between 1 and 20 pixels, the result is TRUE 
#'  
#' filterFunc(trackDat, toFilter = "maj.ax", customFunc = function(x) x >= 1 & x <= 20)
#'
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
