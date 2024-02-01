#' @title Subsetting varList or tracklets objects.
#'
#' @description use of `[` to subset varList or tracklets objects
#'
#' @return these functions return the subsetted varList or tracklets objects
#'
#' @author Quentin PETITJEAN
#'
#' @keywords internal

`[.varList` <- function(x, i, ..., drop = FALSE) {
  res <- lapply(i, function(idx) x[[idx]])
  if (!is.null(names(x))) {
    names(res) <- names(x)[i]
  }
  class(res) <- "varList"
  # conserve attributes 
  storedInfo <- MoveR::getInfo(x)
  res <- MoveR::setInfo(res, storedInfo[[1]],  storedInfo[[2]],  storedInfo[[3]])
  return(res)
}

`[.tracklets` <- function(x, i, ..., drop = FALSE) {
  res <- lapply(i, function(idx) x[[idx]])
  if (!is.null(names(x))) {
    names(res) <- names(x)[i]
  }
  class(res) <- "tracklets"
  # conserve attributes 
  storedInfo <- MoveR::getInfo(x)
  res <- MoveR::setInfo(res, storedInfo[[1]],  storedInfo[[2]],  storedInfo[[3]])
  return(res)
}
