#' @title create and test custom filter for tracking data
#'
#' @description Given a list of data frame containing tracking informations for each fragment, 
#' this function returns a list of vector containing the result of a user defined condition test for each fragment 
#'
#' @param trackDat a list of data frame containing tracking infomations for each fragment 
#' (e.g., maj.ax, angle, min.ax, x.pos, y.pos, ...)
#'
#' @param toFilter an element of the fragment's data frame from which condition will be verified
#' 
#' @param customFunc a custom function containing condition(s) to be applied to toFilter argument
#'
#' @return a list of vector containing the result of a user defined condition test for each fragment 
#'
#' @authors Quentin Petitjean, Vincent Calcagno
#'
#' @examples
#'
#'#' # Exemple 1: test for the presence of infinite value in x.pos, the result is TRUE 
#' if infinite values are detected
#'  
#' filterFunc(trackDat, toFilter = "x.pos", customFunc = function(x) is.infinite(x))
#' 
#'
#' # Exemple 2: perform a condition test on the length of the individuals, the result is TRUE 
#' if individual size is ranging between 1 and 20 pixels
#'  
#' filterFunc(trackDat, toFilter = "maj.ax", customFunc = function(x) x >= 1 & x <= 20)
#'
#'
#' @export

filterFunc = function(trackDat,
                      toFilter = NULL,
                      customFunc = NULL) {
  filter <- list()
  if (!is.null(customFunc)) {
    if (!is.null(toFilter)) {
      for (i in seq(length(trackDat))) {
        selCol <- list_get(trackDat[[i]], toFilter)
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
