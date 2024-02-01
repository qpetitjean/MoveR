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
  isCharIndex <- is.character(i)
  res <- lapply(i, function(idx) x[[idx]])
  if (!is.null(names(x))) {
    if (isCharIndex) {
      names(res) <- i
    } else {
      names(res) <- names(x)[i]
    }
  }
  class(res) <- "varList"
  # conserve attributes 
  storedInfo <- MoveR::getInfo(x)
  res <- MoveR::setInfo(res, storedInfo[[1]],  storedInfo[[2]],  storedInfo[[3]])
  return(res)
}

`[.tracklets` <- function(x, i, ..., drop = FALSE) {
  isCharIndex <- is.character(i)
  res <- lapply(i, function(idx) x[[idx]])
  if (!is.null(names(x))) {
    if (isCharIndex) {
      names(res) <- i
    } else {
      names(res) <- names(x)[i]
    }
  }
  class(res) <- "tracklets"
  # conserve attributes 
  storedInfo <- MoveR::getInfo(x)
  res <- MoveR::setInfo(res, storedInfo[[1]],  storedInfo[[2]],  storedInfo[[3]])
  return(res)
}

# ensure the methods are registered
.onLoad <- function(libname, pkgname) {
  # Register S3 methods for internal use
  registerS3method("as.data.frame", "varList", as.data.frame.varList, envir = asNamespace(pkgname))
  registerS3method("[", "varList", `[.varList` , envir = asNamespace(pkgname))
  registerS3method("[", "tracklets", `[.tracklets`, envir = asNamespace(pkgname))
}