#' @title Merge the results of several condition tests (i.e., the outputs of filterFunc).
#'
#' @description Given several lists containing the results of a condition test (TRUE or FALSE) as returned by \code{\link{filterFunc}}
#' this function merge the results of the condition tests for further use in \code{\link{filterFrags}}.
#'
#' @param filters A list of list containing the results of a condition test (TRUE or FALSE) as returned by \code{\link{filterFunc}}.
#'
#' @param cond A bloolean (i.e., TRUE or FALSE), specifying when the value does not follow the rules of the condition test.
#'
#' @return A list of vector of the same length than the fragments list containing
#'  the merged result of user specified condition tests for each fragment.
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link{filterFunc}}, \code{\link{filterFrags}}
#'
#' @examples
#'
#' # create 3 filters, i.e., 3 lists containing the results of a condition tests (TRUE or FALSE) as returned by \code{\link{filterFunc}}
#' A = list("1" = c(TRUE, FALSE, FALSE), "2" = c(TRUE, TRUE, FALSE))
#' B = list("1" = c(FALSE, TRUE, FALSE), "2" = c(FALSE, TRUE, FALSE))
#' C = list("1" = c(FALSE, FALSE, TRUE), "2" = c(TRUE, FALSE, FALSE))
#'
#' # merge the filters
#' filterMerge(filters = list(A, B, C), cond = TRUE)
#'
#' @export

filterMerge <- function(filters = list(NULL),
                        cond = TRUE) {
  if (length(filters) == 0) {
    stop("filter argument is NULL, a list of 2 or more filters to merge are needed.")
  }
  Mfilt <-
    stats::setNames(lapply(seq(length(filters[[1]])), function(x) {
      MfiltTemp <-
        rep(ifelse(isTRUE(cond), FALSE, TRUE), length(filters[[1]][[x]]))
      for (i in seq(length(filters))) {
        MfiltTemp[which(filters[[i]][[x]] == ifelse(isTRUE(cond), TRUE, FALSE))] <-
          ifelse(isTRUE(cond), TRUE, FALSE)
      }
      return(MfiltTemp)
    }), names(filters[[1]]))
  return(Mfilt)
}