#' @title Compute the distance from the particle to the edge of an object (e.g., the arena) over its trajectory.
#'
#' @description Given a data frame containing tracking information for a given tracklet and a data frame containing the
#' coordinates of an object edges, this function compute the euclidean distance between the edge of the object (e.g., the arena)
#' and the coordinates of the particle over its trajectory. The function then returns the distance between each points of the particle's
#' trajectory and the closest point to the object edge.
#'
#' @param df A data frame containing at x, y coordinates named "x.pos", "y.pos", for a tracklet.
#'
#' @param edge A data frame containing x, y coordinates named "x.pos", "y.pos" specifiyng the location of the
#' arena or any object edge.
#'
#' @param customFunc A function used to specify the formula allowing to compute the distance between a given object or arena edge and the particle over its trajectory.
#' It is possible to call already implemented methods for circular or polygonal arena by calling customFunc = "CircularArena" or customFunc = "PolygonArena", respectively.
#'
#' @return This function returns a vector containing the distance between each points of the
#' trajectory and the closest point of the object edge. If customFunc argument is an already implemented method (i.e., "CircularArena", "PolygonArena") negative values indicate increasing distance
#' from the edge of the object in the direction of its center (the particle is considered inside) and vice versa.
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link{dist2Pt}}, \code{\link{locPos}}
#'
#' @examples
#'
#' # generate a dummy tracklet
#' set.seed(2023)
#' ## start to specify some parameters to generate the tracklet
#' TrackL <-
#'   100 # the length of the tracklet or a sequence to randomly sample tracklet's length
#'
#' TrackDatTemp <-
#'   trajr::TrajGenerate(sample(TrackL, 1), random = TRUE, fps = 1)
#' TrackDat <- MoveR::trackletsClass(list(
#'   data.frame(
#'     x.pos = TrackDatTemp[["x"]] - min(TrackDatTemp[["x"]]),
#'     y.pos = TrackDatTemp[["y"]] - min(TrackDatTemp[["y"]]),
#'     frame = TrackDatTemp[["time"]]
#'   )
#' ))
#' 
#' # Exemple 1: With a circular arena
#' # simulate a circular arena of 20 pixels radius and centered on the particle trajectory
#' arenaEdge <-  as.data.frame(MoveR::circles(mean(TrackDat[[1]][["x.pos"]]),
#'                             mean(TrackDat[[1]][["y.pos"]]),
#'                             radius = 20,
#'                             draw = F))
#' str(arenaEdge)
#'
#' # draw the tracklet and the arena edge
#' MoveR::drawTracklets(TrackDat,
#'                  imgRes = c(80, 80),
#'                  add2It = list(graphics::polygon(
#'                    x = arenaEdge[["x.pos"]], y = arenaEdge[["y.pos"]], cex = 0.1
#'                  )))
#'
#' # Compute the distance to the closest part of the edge based on the form of the arena (here circular)
#' # Negative values indicate increasing distance from the edge of the object in the direction of its center (the particle is considered inside) and vice versa.
#' MoveR::dist2Edge(TrackDat[[1]],
#'                  edge = arenaEdge,
#'                  customFunc = "CircularArena")
#' 
#' # Exemple 2: With a a polygonal arena
#' # simulate a circular arena of 20 pixels radius and centered on the particle trajectory
#' arenaEdge <-  as.data.frame(MoveR::polygons(mean(TrackDat[[1]][["x.pos"]]),
#'                                            mean(TrackDat[[1]][["y.pos"]]),
#'                                            width = 40,
#'                                            height = 40,
#'                                            sides = 4,
#'                                            rotation = (45 * pi) / (180),
#'                                            draw = F))
#' str(arenaEdge)
#' 
#' # draw the tracklet and the arena edge
#' MoveR::drawTracklets(TrackDat,
#'                      imgRes = c(80, 80),
#'                      add2It = list(graphics::polygon(
#'                        x = arenaEdge[["x.pos"]], y = arenaEdge[["y.pos"]], cex = 0.1
#'                      )))
#' 
#' # Compute the distance to the closest part of the edge based on the form of the arena (here circular)
#' # Negative values indicate increasing distance from the edge of the object in the direction of its center (the particle is considered inside) and vice versa.
#' MoveR::dist2Edge(TrackDat[[1]],
#'                  edge = arenaEdge,
#'                  customFunc = "PolygonArena")
#'
#' # Exemple 3: With a polygonal arena, using a distance matrix to avoid tough computation
#' 
#' \dontrun{
#' 
#' # Download the first dataset from the sample data repository
#' Path2Data <- MoveR::DLsampleData(dataSet = 1, tracker = "TRex")
#' Path2Data
#'
#' # Import the list containing the 9 vectors classically used for further computation
#' trackDat <- MoveR::readTrex(Path2Data[[1]])
#'
#' # load the distance matrix to the arena edge, an object or the location of one or several areas of interest (here we have created a distance map using ImageJ)
#' # and retrieve the value of the edge limit (1) to plot it
#' arenaEdge <- MoveR::locROI(Path2Data[[2]], edgeCrit = 1, xy = 1, order = T)
#' 
#' # draw only the first tracklet
#' MoveR::drawTracklets(trackDat,
#'                  selTrack = 1,
#'                  add2It = list(graphics::polygon(x = arenaEdge$x.pos, y = arenaEdge$y.pos)))
#'
#' # Retrieve the distance from the edge using the distance matrix,
#' # because it is tough and time consuming to compute the distance to the closest part of the arena edge in this case (lot of vertices on this polygon)
#' # we can use another sister function helping to retrieve the distance to the edge from the distance matrix
#'
#' ## NB: here NAs are introduced because some tracklets in the raw data have Inf values in x and y.pos, which usually produce a warning message
#' ## here the warning has been silenced but in this case retrieving the distance from the edge should be preceded by a filtering step to remove Inf values (see \code{\link{filterTracklets}})
#' w <- getOption("warn")
#' options(warn = -1)
#' MoveR::locPos(Path2Data[[2]], trackDat[[1]], Fun = function(x) round(x, digits = 0))[950:1000] # display only the 50 position between the indices 950 and 1000
#' options(warn = w)
#' }
#' 
#' @export

dist2Edge <- function(df, edge, customFunc) {
  
  error <- .errorCheck(df = df, x.pos = "x.pos", y.pos = "y.pos")
  if(!is.null(error)){
    stop(error)
  }
  
  if (customFunc == "CircularArena") {
    customFunc <- function(i) {
      .Dist2Circle(df[["x.pos"]][i], df[["y.pos"]][i], edge)
    }
  } else if (customFunc == "PolygonArena") {
    customFunc <- function(i) {
      .Dist2Polygon(df[["x.pos"]][i], df[["y.pos"]][i], edge)
    }
  } else {
    customFunc = customFunc
  }
  Res <-
    sapply(seq(nrow(df)), customFunc)
  return(Res)
}

# some function to compute distance to the edge of a circle and polygon
# for circle: 
.Dist2Circle <- function(x, y, circleCoords) {
center <- c(mean(circleCoords[, "x.pos"]), mean(circleCoords[, "y.pos"]))
radius <- mean(unlist(sqrt((center[1] - circleCoords["x.pos"]) ^ 2 +
                             (center[2] - circleCoords["y.pos"]) ^ 2
)), na.rm = T)

dist <- sqrt((center[1] - x) ^ 2 + (center[2] - y) ^ 2) - radius
return(dist)
}

# for polygon:
# calculate distance from a point to a line segment
# p is the focal point, a and b are the endpoints of the line segment
.DPointLineSegment <- function(p, a, b) {
  # Convert to vectors
  p <- as.numeric(p); a <- as.numeric(a); b <- as.numeric(b)
  # Line segment vector
  lVec <- b - a
  # Vector from point a to point p
  pVec <- p - a
  # Projection length
  projL <- pracma::dot(pVec, lVec) / norm(lVec, type = "2")
  # Check cases
  if (projL < 0) {
    # Closest to point a
    return(norm(pVec, type = "2"))
  } else if (projL > norm(lVec, type = "2")) {
    # Closest to point b
    return(norm(p - b, type = "2"))
  } else {
    # Closest to a point on the line segment
    cPoint <- a + projL * (lVec / norm(lVec, type = "2"))
    return(norm(p - cPoint, type = "2"))
  }
}
# compute distance to polygon edge
.Dist2Polygon <- function(x, y, polygonV) {
  distCalc <- function(i) {
    if (i < nrow(polygonV)) {
      .DPointLineSegment(c(x, y), polygonV[i,], polygonV[i + 1,])
    } else {
      .DPointLineSegment(c(x, y), polygonV[nrow(polygonV),], polygonV[1,])
    }
  }
  isInside <- pracma::inpolygon(x, y, polygonV[,1], polygonV[,2])
  dist <- sapply(1:nrow(polygonV), distCalc)
  minDist <- min(dist)
  if (isInside) {
    minDist = -minDist
  }
  return(minDist)
}
