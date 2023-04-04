#' @title Get an object within a dataframe, a list or a nested list.
#'
#' @description Given a list or dataframe and an object to look for, the function looks for this object name within the data frame or across all levels of a list and returns it. 
#' In case the object is not found, nothing is returned (NULL)
#'
#' @param myList A dataframe, list or a nested list where to look for.
#'
#' @param toFind A character string corresponding to the name of the object to find.
#'
#' @return The object to find or NULL whether the object have been not found.
#'
#' @author Quentin PETITJEAN
#'
#' @examples
#'
#' # create some silly containers where to look for:
#'## a data frame
#' df =  data.frame(values = c(1:4),
#' identity = LETTERS[1:4],
#' other = c("pif", "paf", "pouf","pof"))
#'## a simple list
#' slist = list(values = c(1:20),
#' identity = LETTERS[1:4],
#' other = c("pif", "paf", "pouf","pof"))
#'## a nested list
#' nlist = list(
#' values = c(1:20),
#' other = c("pif", "paf", "pouf","pof"),
#' nested = list(
#'  identity = LETTERS[1:4],
#'  values2 = c(1:20),
#'  other2 = c("pif", "paf", "pouf","pof")
#' ))
#'## a more complicated nested list 
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
#' MoveR::listGet(df, "identity")
#' MoveR::listGet(slist, "identity")
#' MoveR::listGet(nlist, "identity")
#' MoveR::listGet(nnlist, "identity")
#'
#' @export

listGet  <- function(myList, toFind) {
  # if myList is a simple list or a df
  if (toFind %in% names(myList) == TRUE) {
    return(myList[[toFind]])
  }
  # if myList is a nested list
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