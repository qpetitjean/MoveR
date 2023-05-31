#' @title Generate points located on circles contour and draw circle(s).
#'
#' @description Given x and y coordinates of circle(s)' center as well as the circle(s) radius, this function returns a list containing the coordinates of the points making the contour of the circle(s) and draw it.
#'
#' @param x A single numeric value or a vector corresponding to the x value of the center of circles.
#'
#' @param y A single numeric value or a vector corresponding to the y value of the center of circles.
#'
#' @param radius A single numeric value or a vector specifying the radius of the circles to generate.
#'
#' @param Res A numeric value corresponding to the resolution of the circles (i.e., the number of points used to draw the circles contour, default = 500).
#'
#' @param center Either TRUE or a single integers or a vector of integers specifying the symbol(s) or a single character to be used to represent the circle center
#' (see \code{\link[graphics]{points}} for possible values and their interpretation).
#'
#' @param draw A logical value (i.e., TRUE or FALSE) indicating whether the circle(s) should be drawn or not (default = TRUE).
#'
#' @param col The color or a vector of colors for filling the circles, the default leaves polygons unfilled.
#'
#' @param border The color or a vector of colors to draw the border of the circles (default = "black").
#'
#' @param lwd The value of line width or a vector of line width of the circles border (default = 1).
#'
#' @param lty An integer or a vector of integer corresponding to line type to be used for drawing circles border, as in par (default = 1).
#'
#' @return This function returns a list containing the coordinates of the points generated on contour of the circle(s). Each element of the list 
#' correspond to a circle (an ROI). In case only one circle is generated the function returns a dataframe. The function can also draw the circles on an existing plot window or, if there is no active plot window, on a new plot window.
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link[graphics]{locPos}}, \code{\link[graphics]{locROI}}, \code{\link[graphics]{polygon}}, \code{\link[graphics]{points}} 
#'
#' @examples
#'
#' set.seed(2023)
#' plot(NULL, xlim = c(1,120), ylim = c(1,120), xlab = "x", ylab = "y")
#' # draw 4 red-border and red-filled circles of different size on a new plot
#' circleCoords1 <- MoveR::circles(
#'   x = sample(1:100, 4),
#'   y = sample(1:100, 4),
#'   radius = sample(5:15, 4),
#'   center = TRUE,
#'   border = "red",
#'   col = adjustcolor("firebrick", alpha = 0.2),
#'   Res = 500,
#'   lwd = 1.5,
#'   lty = 1,
#'   draw = TRUE
#' )
#' str(circleCoords1)
#' 
#' # draw 2 red-border and red-filled circles and 2 blue-border and blue-filled circles of different size on #' a new plot
#' plot(NULL, xlim = c(-10,120), ylim = c(-10,120), xlab = "x", ylab = "y")
#' circleCoords2 <- MoveR::circles(
#'   x = sample(1:100, 4),
#'   y = sample(1:100, 4),
#'   radius = sample(5:20, 4),
#'   center = TRUE,
#'   border = c(rep("red", 2), rep("blue", 2)),
#'   col = c(rep(adjustcolor("firebrick", alpha = 0.2), 2), rep(adjustcolor("lightblue", alpha = 0.2), 2)),
#'   Res = 500,
#'   lwd = 1.5,
#'   lty = c(rep(1, 2), rep(2, 2)),
#'   draw = TRUE
#' )
#' str(circleCoords2)
#'
#' @export

circles <-
  function(x = NULL,
           y = NULL,
           radius = NULL,
           Res = 500,
           center = NULL,
           col = NULL,
           border = "black",
           lwd = 1,
           lty = 1,
           draw = TRUE) {
    if (is.null(x)) {
      stop("[x] argument is missing, 2 coordinate vectors are needed to draw the circles")
    }
    if (length(x) != length(y)) {
      stop("[x] and [y] arguments has different length: ",
           length(x),
           ", ",
           length(y))
    }
    if (is.null(radius)) {
      stop("[radius] argument is missing, a value is needed to determine the size of the circles")
    }
    else if (length(radius) == 1) {
      radius <- rep(radius, length(x))
    }
    else if (length(radius) > 1 & length(radius) != length(x)) {
      stop(
        "[radius], [x] and [y] arguments has different length: ",
        length(radius),
        ", ",
        length(x),
        ", ",
        length(y)
      )
    }
    if (length(col) == 1) {
      col <- rep(col, length(x))
    }
    else if (length(col) > 1 & length(col) != length(x)) {
      stop(
        "[col], [x] and [y] arguments has different length: ",
        length(col),
        ", ",
        length(x),
        ", ",
        length(y)
      )
    }
    if (length(border) == 1) {
      border <- rep(border, length(x))
    }
    else if (length(border) > 1 & length(border) != length(x)) {
      stop(
        "[border], [x] and [y] arguments has different length: ",
        length(border),
        ", ",
        length(x),
        ", ",
        length(y)
      )
    }
    if (length(lwd) == 1) {
      lwd <- rep(lwd, length(x))
    }
    else if (length(lwd) > 1 & length(lwd) != length(x)) {
      stop(
        "[lwd], [x] and [y] arguments has different length: ",
        length(lwd),
        ", ",
        length(x),
        ", ",
        length(y)
      )
    }
    if (length(lty) == 1) {
      lty <- rep(lty, length(x))
    }
    else if (length(lty) > 1 & length(lty) != length(x)) {
      stop(
        "[lty], [x] and [y] arguments has different length: ",
        length(lty),
        ", ",
        length(x),
        ", ",
        length(y)
      )
    }
      if (isTRUE(draw) & is.null(dev.list())) {
        #graphics::plot.new()
        plot(
          NULL,
          xlim = c(
            min(x, na.rm = T) - max(radius,  na.rm = T),
            max(x, na.rm = T) + max(radius,  na.rm = T)
          ),
          ylim = c(
            min(y, na.rm = T) - max(radius,  na.rm = T),
            max(y, na.rm = T) + max(radius,  na.rm = T)
          ),
          xlab = "x",
          ylab = "y"
        )
      }
    theta <- seq(0, 2 * pi, length = Res)
    circleList <- list()
    for (i in seq(length(x))) {
      xtemp <- radius[i] * cos(theta) + x[i]
      ytemp <- radius[i] * sin(theta) + y[i]
      if (isTRUE(draw)) {
        graphics::polygon(
          x = xtemp,
          y = ytemp,
          col = col[i],
          border = border[i],
          lwd = lwd[i],
          lty = lty[i]
        )
        if (!is.null(center)) {
          if (length(center) == 1) {
            center <- rep(center, length(x))
          }
          else if (length(center) > 1 & length(center) != length(x)) {
            stop(
              "[center], [x] and [y] arguments has different length: ",
              length(center),
              ", ",
              length(x),
              ", ",
              length(y)
            )
          }
          graphics::points(
            x[i],
            y[i],
            pch = ifelse(isTRUE(center[i]), "+", center[i]),
            col = border[i],
            cex = radius[i] / 6
          )
        }
      }
      circleList <-
        append(circleList, list(data.frame(x.pos = xtemp, y.pos = ytemp)))
    }
    names(circleList) <- paste("ROI", seq(length(circleList)), sep = "_")
    if(length(circleList) == 1){
      circleList <- circleList[[1]]
    }
    return(circleList)
  }
