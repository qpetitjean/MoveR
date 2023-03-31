#' @title Compute euclidean distance between a particle and object(s).
#'
#' @description Given a dataframe containing tracking information for a particle as well as a dataframe containing
#' the spatial coordinates of one or several objects or points of interest, this function returns a dataframe
#' containing the euclidean distance between the object(s) and the particle over the whole trajectory.
#'
#' @param df A dataframe containing the spatial coordinates x and y named "x.pos", "y.pos", respectively for a particles over a tracklet.
#'
#' @param obj A dataframe containing the spatial coordinates x and y named "x.pos", "y.pos", respectively for one or several
#'  object(s) or point(s) of interest.
#'
#' @return A dataframe containing as much column as input objects (named obj_1, obj_2, ..., obj_n)
#' and as much row as within the df argument. Each dataframe's column contains 
#' the euclidean distance between the object and the particle(s) over the trajectory.
#'
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link{dist2Edge}}
#'
#' @examples
#'
#' set.seed(2023)
#' # generate a dummy tracklet
#' ## start to specify some parameters to generate the tracklet
#' TrackL <-
#'   100 # the length of the tracklet or a sequence to randomly sample tracklet's length
#' TrackDatTemp <-
#'   trajr::TrajGenerate(sample(TrackL, 1), random = TRUE, fps = 1)
#' TrackDat <- list(
#'   data.frame(
#'     x.pos = TrackDatTemp[["x"]] - min(TrackDatTemp[["x"]]),
#'     y.pos = TrackDatTemp[["y"]] - min(TrackDatTemp[["y"]]),
#'     frame = TrackDatTemp[["time"]]
#'   )
#' )
#' # generate x and y coordinates for two objects located close to the particle trajectory
#' Objn <- 2 # the number of object to simulate
#' obj <- do.call("rbind", lapply(seq(Objn), function(i)
#'   data.frame(
#'     x.pos = sample(min(TrackDat[[1]][["x.pos"]]):max(TrackDat[[1]][["x.pos"]]), 1),
#'     y.pos = sample(min(TrackDat[[1]][["y.pos"]]):max(TrackDat[[1]][["y.pos"]]), 1)
#'   )))
#' 
#' # draw the trajectory and the objects (red dots)
#' MoveR::drawTracklets(TrackDat,
#'                  timeCol = "frame",
#'                  add2It = list(
#'                    points(
#'                      obj[["x.pos"]],
#'                      obj[["y.pos"]],
#'                      pch = 19,
#'                      cex = 2.2,
#'                      col = adjustcolor("red", alpha = 0.2)
#'                    ),
#'                    text(obj[["x.pos"]],
#'                         obj[["y.pos"]],
#'                         rownames(obj),
#'                         cex = 0.6,
#'                         col = "firebrick")
#'                  ))
#' 
#' # compute the euclidean distance between the objects and each points of the particle's trajectory
#' DistList <- MoveR::dist2Pt(TrackDat[[1]], obj)
#' 
#' # represent the distance on the plot for a random points (62)
#' timePt = 60
#' for (i in seq(2)) {
#'   segments(
#'     x0 = TrackDat[[1]][["x.pos"]][timePt],
#'     y0 = TrackDat[[1]][["y.pos"]][timePt],
#'     x1 = obj[["x.pos"]][i],
#'     y1 = obj[["y.pos"]][i]
#'   )
#'   text((TrackDat[[1]][["x.pos"]][timePt] + obj[["x.pos"]][i]) / 2 + 2.5,
#'        (TrackDat[[1]][["y.pos"]][timePt] + obj[["y.pos"]][i]) / 2,
#'        round(DistList[timePt, i], digits = 1),
#'        cex = 0.6,
#'        col = "black"
#'   )
#' }
#'
#' @export

dist2Pt <- function(df, obj) {
  # iterate over the obj to measure euclidean distance over the trajectory for each object
  Res <- stats::setNames(data.frame(apply(obj, 1, function(i)
    sqrt((df[["x.pos"]] - i[["x.pos"]]) ^ 2 +
           (df[["y.pos"]] - i[["y.pos"]]) ^ 2
    ))),
    paste("obj", seq(nrow(obj)), sep = "_"))
  return(Res)
}
