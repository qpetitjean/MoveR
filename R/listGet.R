#' @title Get an object within a dataframe, a list or a nested list
#'
#' @description The function looks for an object name within all levels of a list and returns it when found
#'
#'
#' @param myList A dataframe, list or a nested list where to look for 
#'
#' @param toFind A character string corresponding to the name of the object to find
#'
#'
#' @return The object to find or NULL whether the object have been not found
#'
#' @authors Quentin PETITJEAN
#'
#'
#' @examples
#'
#' # create some silly containers where to look for
#'
#' df =  data.frame(values = c(1:4),
#' identity = LETTERS[1:4],
#' other = c("pif", "paf", "pouf","pof"))
#'
#' slist = list(values = c(1:20),
#' identity = LETTERS[1:4],
#' other = c("pif", "paf", "pouf","pof"))
#'
#' nlist = list(
#' values = c(1:20),
#' other = c("pif", "paf", "pouf","pof"),
#' nested = list(
#'  identity = LETTERS[1:4],
#'  values2 = c(1:20),
#'  other2 = c("pif", "paf", "pouf","pof")
#' ))
#' 
#' nnlist = list(
#' values = c(1:20),
#' other = c("pif", "paf", "pouf","pof"),
#' nested = list(
#'  nested2 = list(identity = LETTERS[1:4]),
#'  values2 = c(1:20),
#'  other2 = c("pif", "paf", "pouf","pof")
#' ))
#' 
#' # looking for "identity" containers within each element
#' 
#' listGet(df, "identity")
#' listGet(slist, "identity")
#' listGet(nlist, "identity")
#' listGet(nnlist, "identity")
#'
#' @export

listGet  <- function(myList, toFind) {
  # if my_list is a simple list or a df
  if (toFind %in% names(myList) == TRUE) {
    return(myList[[toFind]])
  }
  # if my_list is a nested list
  else if (any(sapply(myList, class) == "list")) {
    for (i in myList) {
      found <- Recall(i, toFind)
      if (!is.null(found)) {
        return(found)
      } else {
        NULL
      }
    }
  }
}