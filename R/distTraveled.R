#' @title Compute the distance traveled by a particle along a trajectory.
#'
#' @description Given a data frames containing tracking information for a given fragment, 
#' this function compute the euclidean distance along the trajectory according to a given step.
#' 
#' @param df A data frame containing at x, y coordinates named "x.pos", "y.pos", for a fragment.
#' 
#' @param step A numeric value specifying the number of time unit or records between the two positions from which distance will be computed.
#' 
#' @return this function returns a vector containing the distance between each points of the 
#' trajectory according to the specified step.
#'
#' @author Quentin PETITJEAN
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
#'# compute the distance traveled between each point of the trajectory
#'fragDat[["distTraveled"]] <- distTraveled(fragDat, step = 1)
#'
#'# check the result
#'str(fragDat)
#'
#'# sum all the distance to compute the total distance traveled by the particle over the trajectory
#'sum(fragDat[["distTraveled"]], na.rm = T)
#'
#' @export

distTraveled <- function(df, step = 1){
  if(is.null(listGet(df, "x.pos"))){
    stop(
      "x.pos column is missing or might be misspelled: x coordinates are needed to compute euclidian distance"
    )
  }
  if(is.null(listGet(df, "y.pos"))){
    stop(
      "x.pos column is missing or might be misspelled: x coordinates are needed to compute euclidian distance"
    )
  }
  Res <-
    sapply(seq(1, nrow(df), by = step), function(i)
      sqrt((df[["x.pos"]][i + 1] - df[["x.pos"]][i])^ 2 +
             (df[["y.pos"]][i + 1] - df[["y.pos"]][i])^ 2))
  return(Res)
}
