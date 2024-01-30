#' @title Generate points located on polygon vertices and draw polygon(s).
#' 
#' @description Given x and y coordinates of the polygon(s)' center as well as width, height, number of sides, and rotation angle, this function returns a list containing the coordinates of the polygon(s) vertices and draw it.
#'
#' @param x A single numeric value or a vector corresponding to the x value of the center of polygons
#'
#' @param y A single numeric value or a vector corresponding to the y value of the center of polygons
#'
#' @param width A single numeric value or a vector specifying the width of the polygons to generate.
#' 
#' @param height A single numeric value or a vector specifying the height of the polygons to generate.
#' 
#' @param sides A single numeric value or a vector specifying the number of sides belonging to the polygons to generate.
#' 
#' @param rotation A single numeric value or a vector specifying the angle of rotation of the polygons in radians.
#'
#' @param center Either TRUE or a single integers or a vector of integers specifying the symbol(s) or a single character to be used to represent the polygons center
#' (see \code{\link[graphics]{points}} for possible values and their interpretation).
#'
#' @param draw A logical value (i.e., TRUE or FALSE) indicating whether the polygons should be drawn or not (default = TRUE).
#'
#' @param col The color or a vector of colors for filling the polygons, the default leaves polygons unfilled.
#'
#' @param border The color or a vector of colors to draw the border of the polygons (default = "black").
#'
#' @param lwd The value of line width or a vector of line width of the polygons border (default = 1).
#'
#' @param lty An integer or a vector of integer corresponding to line type to be used for drawing polygons border, as in par (default = 1).
#'
#' @return This function returns a list containing the coordinates of the points generated on contour of the polygon(s) (i.e., vertices). Each element of the list 
#' correspond to a polygon (an ROI). In case only one polygon is generated the function returns a dataframe. The function can also draw the circles on an existing plot window or, if there is no active plot window, on a new plot window.
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link[MoveR]{circles}}, \code{\link[MoveR]{locPos}}, \code{\link[MoveR]{locROI}}, \code{\link[graphics]{points}}, \code{\link[graphics]{polygon}} 
#'
#' @examples
#'
#'  set.seed(2023)
#'  plot(NULL, xlim = c(1,120), ylim = c(1,120), xlab = "x", ylab = "y")
#'  # draw 4 red-border and red-filled polygons of different shape, size and angle on a new plot
#'  polygonsCoords1 <- MoveR::polygons(
#'    x = sample(1:100, 4),
#'    y = sample(1:100, 4),
#'    width  = sample(5:40, 4),
#'    height = sample(5:40, 4),
#'    sides = sample(3:6, 4),
#'    rotation = sample(seq(0, 2*pi, 0.5), 4),
#'    center = TRUE,
#'    border = "red",
#'    col = adjustcolor("firebrick", alpha = 0.2),
#'    lwd = 1.5,
#'    lty = 1,
#'    draw = TRUE
#'  )
#'  str(polygonsCoords1)
#'  
#'  # draw 2 red-border and red-filled circles and 2 blue-border and blue-filled circles of different size on a new plot
#'  plot(NULL, xlim = c(-10,120), ylim = c(-10,120), xlab = "x", ylab = "y")
#'  polygonsCoords2 <- MoveR::polygons(
#'    x = sample(1:100, 4),
#'    y = sample(1:100, 4),
#'    width  = sample(5:40, 4),
#'    height = sample(5:40, 4),
#'    sides = sample(3:6, 4),
#'    rotation = sample(seq(0, 2*pi, 0.5), 4),
#'    center = TRUE,
#'    border = c(rep("red", 2), rep("blue", 2)),
#'    col = c(rep(adjustcolor("firebrick", alpha = 0.2), 2), rep(adjustcolor("lightblue", alpha = 0.2), 2)),
#'    lwd = 1.5,
#'    lty = c(rep(1, 2), rep(2, 2)),
#'    draw = TRUE
#'  )
#' str(polygonsCoords2)
#' 
#' @export

polygons <-
  function(x = NULL,
           y = NULL,
           width = NULL,
           height = NULL,
           sides = NULL,
           rotation = 0,
           center = TRUE,
           col = NULL,
           border = "black",
           lwd = 1,
           lty = 1,
           draw = TRUE) {
    if (is.null(x)) {
      stop("[x] argument is missing, 2 coordinate vectors are needed to draw a polygon")
    }
    if (length(x) != length(y)) {
      stop("[x] and [y] arguments has different length: ",
           length(x),
           ", ",
           length(y))
    }
    if (is.null(width)) {
      stop("[width] argument is missing, a value is needed to determine the position of the polygon vertices")
    } else if (length(width) == 1) {
      width <- rep(width, length(x))
    } else if (length(width) > 1 & length(width) != length(x)) {
      stop(
        "[width], [x] and [y] arguments has different length: ",
        length(width),
        ", ",
        length(x),
        ", ",
        length(y)
      )
    }
    if (is.null(height)) {
      stop("[height] argument is missing, a value is needed to determine the position of the polygon vertices")
    } else if (length(height) == 1) {
      height <- rep(height, length(x))
    } else if (length(height) > 1 & length(height) != length(x)) {
      stop(
        "[height], [x] and [y] arguments has different length: ",
        length(height),
        ", ",
        length(x),
        ", ",
        length(y)
      )
    }
    if (is.null(sides)) {
      stop("[sides] argument is missing, the number of polygon's side is needed to determine the position of the polygon vertices")
    } else if (length(sides) == 1) {
      sides <- rep(sides, length(x))
    } else if (length(sides) > 1 & length(sides) != length(x)) {
      stop(
        "[sides], [x] and [y] arguments has different length: ",
        length(sides),
        ", ",
        length(x),
        ", ",
        length(y)
      )
    }
    if (is.null(rotation)) {
      stop("[rotation] argument is missing, the rotation angle of polygon is needed to determine the position of the polygon vertices")
    } else if (length(rotation) == 1) {
      rotation <- rep(rotation, length(x))
    } else if (length(rotation) > 1 & length(rotation) != length(x)) {
      stop(
        "[rotation], [x] and [y] arguments has different length: ",
        length(rotation),
        ", ",
        length(x),
        ", ",
        length(y)
      )
    }
    if (length(col) == 1) {
      col <- rep(col, length(x))
    } else if (length(col) > 1 & length(col) != length(x)) {
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
    } else if (length(border) > 1 & length(border) != length(x)) {
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
    } else if (length(lwd) > 1 & length(lwd) != length(x)) {
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
    } else if (length(lty) > 1 & length(lty) != length(x)) {
      stop(
        "[lty], [x] and [y] arguments has different length: ",
        length(lty),
        ", ",
        length(x),
        ", ",
        length(y)
      )
    }
    
  # Calculate half-width and half-height
  hWidth <- width / 2
  hHeight <- height / 2
  
  # Calculate the angle between each vertex
  verticesAngles <- 2 * pi / sides
  
  # retrieve the starting angle according to the specified rotation angle
  startAngle <- -rotation - (pi / 2)
  
  if (isTRUE(draw) & is.null(dev.list())) {
    #graphics::plot.new()
    plot(
      NULL,
      xlim = c(
        min(x, na.rm = T) - max(width,  na.rm = T),
        max(x, na.rm = T) + max(width,  na.rm = T)
      ),
      ylim = c(
        min(y, na.rm = T) - max(height,  na.rm = T),
        max(y, na.rm = T) + max(height,  na.rm = T)
      ),
      xlab = "x",
      ylab = "y"
    )
  }
  # Initialize an empty list to store the coordinates of the polygons' vertices 
  polygonsList <- list()
  for (j in seq(length(x))){
    vertexX <- c()
    vertexY <- c()
    if (sides[[j]] <= 2) {
      stop("for the polygon ", "#", j, ", [sides] argument is below 2, three or more straight sides are needed to make a polygon")
    }
    for (i in seq(sides[[j]])) {
      # Calculate the current angle for the vertex
      currentAngle <- startAngle[[j]] + i * verticesAngles[[j]]
      # Calculate the coordinates of the vertex
      vertexX[i]<- x[[j]] + hWidth[[j]] * cos(currentAngle)
      vertexY[i] <- y[[j]] + hHeight[[j]] * sin(currentAngle)
    }
    if (isTRUE(draw)) {
      graphics::polygon(
        x = vertexX,
        y = vertexY,
        col = col[j],
        border = border[j],
        lwd = lwd[j],
        lty = lty[j]
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
          x[j],
          y[j],
          pch = ifelse(isTRUE(center[j]), "+", center[j]),
          col = border[j],
          cex = max(min(min(c(width[j], height[j])) / 72 / 0.01 / sqrt(length(center)), 2), 0.1)/2 
        )
      }
    }
    polygonsList <-
      append(polygonsList, list(data.frame(x.pos = vertexX, y.pos = vertexY)))
  }
  names(polygonsList) <- paste("ROI", seq(length(polygonsList)), sep = "_")
  if(length(polygonsList) == 1){
    polygonsList <- polygonsList[[1]]
  }
  return(polygonsList)
}
