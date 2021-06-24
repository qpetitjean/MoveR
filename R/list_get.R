#' @title Get an object within a dataframe, a list or a nested list
#'
#' @description The function looks for an object name within all levels of a list and returns it when found
#'
#'
#' @param my_list A dataframe, list or a nested list where to look for 
#'
#' @param to_find A character string corresponding to the name of the object to find
#'
#'
#' @return The object to find or NULL whether the object have been not found
#'
#' @authors Quentin Petitjean
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
#' list_get(df, "identity")
#' list_get(slist, "identity")
#' list_get(nlist, "identity")
#' list_get(nnlist, "identity")
#'
#' @export

list_get  <- function(my_list, to_find) {
  # if my_list is a simple list or a df
  if (to_find %in% names(my_list) == TRUE) {
    return(my_list[[to_find]])
  }
  # if my_list is a nested list
  else if (any(sapply(my_list, class) == "list")) {
    for (i in my_list) {
      found <- Recall(i, to_find)
      if (!is.null(found)) {
        return(found)
      } else {
        NULL
      }
    }
  }
}
