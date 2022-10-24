#' @title Compute euclidean distance between a particle and object(s).
#'
#' @description Given a dataframe containing tracking information for a particle as well as a dataframe containing
#' the spatial coordinates of one or several objects or points of interest, this function returns a dataframe
#' containing the euclidean distance between the object(s) and the particle over the whole trajectory.
#'
#'
#' @param df A dataframe containing the spatial coordinates x and y named "x.pos", "y.pos", respectively for a particles over a fragment
#'
#' @param obj A dataframe containing the spatial coordinates x and y named "x.pos", "y.pos", respectively for one or several
#'  object(s) or point(s) of interest
#'
#' @return A dataframe containing as much column as input objects (named obj_1, obj_2, ..., obj_n)
#' and as much row as within df. Each dataframe's column contains 
#' the euclidean distance between the object and the particle over the trajectory
#'
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link{dist2Edge}}
#'
#' @examples
#'
#'# generate a dummy fragment
#'## start to specify some parameters to generate the fragment
#'FragL <- 100 # the length of the fragment or a sequence to randomly sample fragment length
#'
#'fragDatTemp <- trajr::TrajGenerate(sample(FragL, 1), random = TRUE, fps = 1)
#'fragDat <- data.frame(
#'  x.pos = fragDatTemp[["x"]] - min(fragDatTemp[["x"]]),
#'  y.pos = fragDatTemp[["y"]] - min(fragDatTemp[["y"]] ),
#'  frame = fragDatTemp[["time"]]
#')
#'
#'# generate x and y coordinates for two objects located close to the particle trajectory
#'Objn <- 2 # the number of object to simulate
#'obj <- do.call("rbind", lapply(seq(Objn), function(i)
#'  data.frame(
#'    x.pos = sample(min(fragDat[["x.pos"]]):max(fragDat[["x.pos"]]), 1),
#'    y.pos = sample(min(fragDat[["y.pos"]]):max(fragDat[["y.pos"]]), 1)
#'  )))
#'
#'# draw the trajectory and the objects (red dots)
#'drawFrags(
#'  list(fragDat),
#'  imgRes = c(ceiling(max(fragDat[["x.pos"]])), ceiling(max(fragDat[["y.pos"]]))),
#'  timeCol = "frame",
#'  add2It = list(
#'    points(
#'      obj[["x.pos"]],
#'      obj[["y.pos"]],
#'      pch = 19,
#'      cex = 2.2,
#'      col = adjustcolor("red", alpha = 0.2)
#'    ),
#'    text(obj[["x.pos"]],
#'         obj[["y.pos"]],
#'         rownames(obj),
#'         cex = 0.6,
#'         col = "firebrick"))
#')
#'
#'# compute the euclidean distance between the objects and each points of the particle's trajectory
#'DistList <- dist2Pt(fragDat, obj)
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
