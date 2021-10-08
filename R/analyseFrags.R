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
#' @param customFunc A function or a list of functions used to perform the computation along all fragments
#' NB: in case customFunc is a list of unnamed function it will try to retrieve their names by returning the first character string
#' following function() as name of the results column
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

analyseFrags <- function(trackDat, customFunc) {
  if (is.null(customFunc)) {
    stop("customFunc argument is missing, a customFunc is needed to compute metric")
  }
  # if customFunc is a unnamed list of function, retrieve function names
  if (is.list(customFunc)) {
    if (is.null(names(customFunc))) {
      VarName <-
        lapply(customFunc, function(x)
          strsplit(sub("\\(.*", "", deparse(x)), " ")[[2]])
      names(customFunc) <- unlist(VarName)
    }
    # if customFunc is a function retrieve function names and transformed it to a named list
  } else if (is.function(customFunc)) {
    VarName <- strsplit(sub("\\(.*", "", deparse(customFunc)), " ")[[2]]
    customFunc <- list(customFunc)
    names(customFunc) <- VarName
  }
  
  # loop trough the fragment list and append the result to each list by using the name of the custom function
  total = length(trackDat)
  pb <-
    progress::progress_bar$new(format = "fragments processing [:bar] :current/:total (:percent)", total = total)
  pb$tick(0)
  Sys.sleep(0.001)
  # if trackDat is an unnamed list of fragment use the position of the fragment in the list as fragment name
  if (is.null(names(trackDat))) {
    names(trackDat) <- seq(length(trackDat))
  }
  for (i in names(trackDat)) {
    for (j in names(customFunc)) {
      if (!inherits(try(customFunc[[j]](trackDat[[i]]), silent = T)
                    , "try-error") &
          !class(customFunc[[j]](trackDat[[i]])) == "function") {
        trackDat[[i]][[j]] <- customFunc[[j]](trackDat[[i]])
      } else if (inherits(try(customFunc[[j]](trackDat[[i]]), silent = T)
                          , "try-error") |
                 class(customFunc[[j]](trackDat[[i]])) == "function") {
        trackDat[[i]][[j]] <- NA
        warning(
          "For customFunc ",
          j ,
          " in fragment ",
          i ,
          " : \nanalyseFrags returned NA, perhaps check customFunc argument"
        )
      }
    }
    # progress bar
    pb$tick(1)
    Sys.sleep(1 / 1000)
  }
  
  return(trackDat)
}