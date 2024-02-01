#' @title Converting varList or tracklets objects using S3 method.
#'
#' @description as.data.frame method to convert varList to a dataframe.
#'
#' @return these functions return the converted varList or tracklets objects.
#'
#' @author Quentin PETITJEAN
#'
#' @keywords internal

as.data.frame.varList <- function(x, row.names = NULL, optional = FALSE, ...) {
  lens <- sapply(x, length)
  if(length(unique(lens)) != 1) {
    stop("All elements of the varList must have the same length to be converted to a data.frame")
  }

  df <- do.call(data.frame, c(x, list(row.names = row.names, ...)))
  return(df)
}