#' @title Fragments analyses
#'
#'
#' @description Given a list of data frames containing tracking informations for each fragment and 
#' a custom function, this function perform the computation and returns the original list 
#' of data frames with the result of the analysis appended 
#' 
#'
#' @param trackDat A list of data frame containing tracking informations for each fragment
#' 
#' @param customFunc A function used to perform the computation along all fragments
#' 
#' @param VarName A character string specifying the name of the computed metric (optional) by default the function
#' retrieve the name of the custom function and consider it as the metric name
#'
#' @return this function returns a new list of dataframe corresponding to each new fragments with the result of customFunc appended
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

analyseFrags <- function(trackDat, customFunc, VarName = NULL) {
  
  # in case VarName is not specified, extract the name of the custom function
  if (is.null(VarName)) {
    VarName <- strsplit(sub("\\(.*", "", deparse(customFunc)), " ")
    if (length(VarName) > 1) {
      VarName = VarName[[2]]
    }
  }
  # loop trough the fragment list and append the result to each list by using the name of the custom function   
  total = length(trackDat)
  pb <-
    progress::progress_bar$new(format = "fragments processing [:bar] :current/:total (:percent)", total = total)
  pb$tick(0)
  Sys.sleep(0.001)
  
  for(i in names(trackDat)){
    if (!inherits(try(customFunc(trackDat[[i]]), silent = T)
                  , "try-error") & !class(customFunc(trackDat[[i]])) == "function") {
    trackDat[[i]][[VarName]] <- customFunc(trackDat[[i]])
    } else if (inherits(try(customFunc(trackDat[[i]]), silent = T)
                        , "try-error") | class(customFunc(trackDat[[i]])) == "function") {
      trackDat[[i]][[VarName]] <- NA
      warning("In fragment ",i , " : analyseFrags returned NA, perhaps check customFunc parameters")
    }
    # progress bar
    pb$tick(1)
    Sys.sleep(1 / 1000)
  }
  return(trackDat)
}