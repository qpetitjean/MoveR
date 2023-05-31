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
#' It is possible to call already implemented methods for Circular arena by calling customFunc = "CircularArena".
#'
#' @return This function returns a vector containing the distance between each points of the
#' trajectory and the closest point of the object edge. Negative values indicate increasing distance
#' from the edge of the object in the direction of its center (the particle is considered inside) and vice versa.
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link{dist2Pt}}, \code{\link{locPos}}
#'
#' @examples
#'
#' # Exemple 1: With a circular arena
#'
#' set.seed(2023)
#' # generate a dummy tracklet
#' ## start to specify some parameters to generate the tracklet
#' TrackL <-
#'   100 # the length of the tracklet or a sequence to randomly sample tracklet's length
#'
#' TrackDatTemp <-
#'   trajr::TrajGenerate(sample(TrackL, 1), random = TRUE, fps = 1)
#' TrackDat <- list(
#'   data.frame(
#'     x.pos = TrackDatTemp[["x"]] - min(TrackDatTemp[["x"]]),
#'     y.pos = TrackDatTemp[["y"]] - min(TrackDatTemp[["y"]]),
#'     frame = TrackDatTemp[["time"]]
#'   )
#' )
#'
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
#'                  add2It = list(points(
#'                    x = arenaEdge[["x.pos"]], y = arenaEdge[["y.pos"]], cex = 0.1
#'                  )))
#'
#' # Compute the distance to the closest part of the edge based on the form of the arena (here circular)
#' # Negative values indicate increasing distance from the edge of the object in the direction of its center (the particle is considered inside) and vice versa.
#' MoveR::dist2Edge(TrackDat[[1]],
#'                  edge = arenaEdge,
#'                  customFunc = "CircularArena")
#'
#'
#' # Exemple 2: With a a polygonal arena, using a distance matrix to avoid tough computation
#' \dontrun{
#' 
#' # Download the first dataset from the sample data repository
#' Path2Data <- MoveR::DLsampleData(dataSet = 1, tracker = "TRex")
#' Path2Data
#'
#' # Import the list containing the 9 vectors classically used for further computation
#' Data <- MoveR::readTrex(Path2Data[[1]])
#'
#' # convert it to a list of tracklets
#' trackDat <- MoveR::convert2Tracklets(Data[1:7], by = "identity")
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
#' # because it is tough to compute the distance to the closest part of the arena edge in this case
#' # we can use another sister function helping to retrieve the distance to the edge from the distance
#' # matrix
#'
#' ## NB: here NAs are introduced because some tracklets in the raw data have Inf values in x and y.pos, which usually produce a warning message
#' ## here the warning has been silenced but in this case retrieving the distance from the edge should be preceded by a filtering step to remove Inf values (see \code{\link{filterTracklets}})
#' w <- getOption("warn")
#' options(warn = -1)
#'
#' cbind(trackDat[[1]], MoveR::locPos(Path2Data[[2]], trackDat[[1]], Fun = function(x) round(x, digits = 0)))[950:1000, ]
#'
#' options(warn = w)
#'
#' }
#' @export

dist2Edge <- function(df, edge, customFunc) {
  if (customFunc == "CircularArena") {
    ### for a circular arena, compute the distance between the point of the trajectory and the center of the arena
    ### and substract it to the length of the radius :
    center <- c(mean(edge[, "x.pos"]), mean(edge[, "y.pos"]))
    radius <- mean(unlist(sqrt((center[1] - edge["x.pos"]) ^ 2 +
                                 (center[2] - edge["y.pos"]) ^ 2
    )), na.rm = T)
    customFunc <- function(i) {
      sqrt((center[1] - df[["x.pos"]][i]) ^ 2 +
             (center[2] - df[["y.pos"]][i]) ^ 2) - radius
    }
  }
  Res <-
    sapply(seq(nrow(df)), customFunc)
  return(Res)
}
