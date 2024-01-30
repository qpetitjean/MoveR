#' @title Build count matrix in a 2 dimensional space.
#'
#' @description Given 2 numeric vectors defining a 2 dimensional space, and a specified number of bins, 
#' this function compute the count for each bins in the 2 dimensional space and return the result either as a dataframe or a matrix.
#' It is also possible to specify a grouping vector to obtain as much count matrix as the number of specified groups. 
#'
#' @param x A numeric vector used as the first dimension of the matrix.
#'
#' @param y A numeric vector used as the second dimension of the matrix.
#' 
#' @param groups A character or numeric vector used as a grouping variable to split the count matrix.
#' 
#' @param output A character string indicating whether the result should be returned as a dataframe or a list of matrix (depending on the number of groups, if groups is NULL, the function returns an unique matrix).
#'
#' @param nbins A numeric value indicating the number of bins in both vertical and horizontal directions (default = 100).
#'
#' @param na.rm A logical value (i.e., TRUE or FALSE) indicating whether NA values should be stripped before the computation proceeds (default = TRUE).
#'
#'
#' @return This function returns the number of counts over the two specified dimension (x and y) according to the number of specified bins (nbins argument). 
#' The resulting output can be either a dataframe or a list of matrix depending on the output argument and the number of groups specified by the groups argument.
#' 
#' @author Quentin PETITJEAN
#'
#' @examples
#' 
#' test <-
#' countMat(x = round(rnorm(5000, mean = 50 , sd = 10)),
#'          y = round(rnorm(5000, mean = 50 , sd = 10)),
#'          nbins = 10,
#'          output = "matrix")
#' test
#' 
#' # it is then possible to draw the count distribution in the 2d space using plotly
#' \dontrun{
#' # draw the 3d plot using plotly 
#' library(plotly)
#' 
#' # initialize the plot
#' fig <- plotly::plot_ly(x=~colnames(test), y=~rownames(test), contours = list(
#'   z = list(
#'     show = TRUE,
#'     start = round(min(sqrt(test)),-2),
#'     project = list(z = TRUE),
#'     end = round(max(sqrt(test)),-2),
#'     size = max(sqrt(test)) / 10,
#'     color = "white"
#'   )
#' ))
#' # add the layer
#' fig <- plotly::add_surface(
#'   p = fig,
#'   z = sqrt(test),
#'   opacity = 0.8,
#'   colorscale = "Hot",
#'   cmin = min(sqrt(test)),
#'   cmax = max(sqrt(test)),
#'   colorbar = list(title = "counts (sqrt)")
#' )
#' # add some legends
#' fig <- plotly::layout(
#'   fig,
#'   title = '3D density plot',
#'   scene1 = list(
#'     xaxis = list(title = "x"),
#'     yaxis = list(title = "y"),
#'     zaxis = list(title = "counts (sqrt)")
#'   )
#' )
#' fig
#' }
#' @export
#'
countMat <- function(x = NULL,
                     y = NULL,
                     nbins = 100,
                     groups = NULL,
                     output = c("matrix", "data.frame"),
                     na.rm = TRUE) {
  
  if (length(output) > 1) {
    output <- "matrix"
    warning("[output] argument is unspecified, default value is matrix")
  }
  # in case x and y have different length, return an error message.
  if (length(x) != length(y)) {
    stop(
      "x and y have different length: ",
      length(x),
      ", ",
      length(y),
      ".\n",
      "x and y must contain the same amount of data."
    )
  }
  
  # check the number of groups
  if (is.null(groups)) {
    gLev <- 1
    gn <- gLev
    tempDf <- data.frame(x, y, groups = rep(gLev, length(x)))
  } else{
    gLev <- unique(groups)
    if (isTRUE(na.rm)) {
      gLev <- na.omit(gLev)
      gn <- seq_along(na.omit(gLev))
    } else{
      gn <- seq_along(gLev)
    }
    tempDf <- data.frame(x, y, groups)
  }
  if (isTRUE(na.rm)) {
    tempDf <- na.omit(tempDf)
  }
  
  # in case there is infinite values remove them
  if (length(which(is.infinite(tempDf[["x"]]))) > 0) {
    tempDf <- tempDf[-c(which(is.infinite(tempDf[["x"]]))),]
    warning("x contains infinite values, these values has been removed")
  }
  if (length(which(is.infinite(tempDf[["y"]]))) > 0) {
    tempDf <- tempDf[-c(which(is.infinite(tempDf[["y"]]))),]
    warning("y contains infinite values, these values has been removed")
  }
  
  # identify the cuts
  nbins <- rep(nbins, 2)
  x.cuts <-
    seq(
      from = min(tempDf[["x"]]),
      to = max(tempDf[["x"]]),
      length = nbins[1] + 1
    )
  y.cuts <-
    seq(
      from = min(tempDf[["y"]]),
      to = max(tempDf[["y"]]),
      length = nbins[2] + 1
    )
  
  res <- list()
  for (i in gn) {
    # compute the matrix of count according to each cuts and groups
    tempDfsplit <- tempDf[which(tempDf[["groups"]] == gLev[[i]]),]
    index.x <-
      cut(tempDfsplit[["x"]], x.cuts, include.lowest = TRUE)
    index.y <-
      cut(tempDfsplit[["y"]], y.cuts, include.lowest = TRUE)
    m <-
      tapply(tempDfsplit[["x"]], list(index.x, index.y), base::length)
    m[is.na(m)] <- 0
    
    # transform it to dataframe
    mdf <-
      data.frame(rows = rownames(m)[row(m)],
                 vars = colnames(m)[col(m)],
                 values = c(m))
    names(mdf) <- c("x", "y", "value")
    mdfL <-
      lapply(seq(ncol(mdf) - 1), function(z)
        gsub('^.|.$', "", mdf[, z]))
    mdf2 <-
      cbind(data.frame(do.call("cbind", mdfL)), value = mdf[["value"]])
    xcomp <- strsplit(as.character(mdf2[["X1"]]), ",", fixed = TRUE)
    ycomp <- strsplit(as.character(mdf2[["X2"]]), ",", fixed = TRUE)
    xcomp <-
      unlist(lapply(xcomp, function(x)
        mean(as.numeric(x), na.rm = T)))
    ycomp <-
      unlist(lapply(ycomp, function(x)
        mean(as.numeric(x), na.rm = T)))
    mdf3 <-
      data.frame(x = xcomp,
                 y = ycomp,
                 value = mdf2[["value"]])
    res[[i]] <- mdf3
  }
  
  if (output == "matrix") {
    outP <-
      stats::setNames(lapply(res, function(z)
        stats::xtabs(value ~ y + x, z)),
        gLev)
    
    if (length(outP) == 1) {
      outP <- outP[[1]]
    }
  } else if (output == "data.frame") {
    names(res) <- gLev
    outP <-
      do.call(rbind, lapply(names(res), function(x)
        data.frame(res[[x]], groups = x)))
    
  }
  return(outP)
}
