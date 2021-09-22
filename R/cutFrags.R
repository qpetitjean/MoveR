#' @title CutFrags
#'
#'
#' @description Given a list of data frames containing tracking informations for each fragments the function 
#' returns the fragments part that meet the condition specified in the customFunc 
#'
#'
#' @param trackDat A list of data frame containing tracking informations for each fragment (including a timeline)
#'
#'
#' @param customFunc A function used to cut/subset the fragments 
#'
#'
#' @return This function returns a list of data frames containing subsetted tracking informations for each fragments 
#' that meet the conditions specified by customFunc
#'
#'
#' @authors Quentin Petitjean
#'
#'
#'
#' @examples
#'
#' #TODO
#'
#' @export


cutFrags <- function(trackDat, customFunc) {
  # identify part of fragment detected in the selected Time interval
  When <-
    lapply(finalDatFrags2, customFunc)
  # identify which fragment are detected in the selected Time interval
  Who <-
    which(unlist(lapply(When, function(y)
      TRUE %in% y)) == TRUE)
  # isolate part of detected fragment included in time interval
  WhoWhen <-
    lapply(When[c(names(Who))], function (z)
      which(z == TRUE))
  # store the selected fragment part in a list
  output <- list()
  for (w in names(WhoWhen)) {
    output[[w]] <- trackDat[[w]][c(WhoWhen[[w]]), ]
  }
  return(output)
}