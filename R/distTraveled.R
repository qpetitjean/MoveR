#' @title Compute the distance traveled by a particle over a trajectory.
#'
#' @description Given a data frames containing tracking information for a given tracklet, 
#' this function compute the euclidean distance along the trajectory according to a given step.
#' 
#' @param df A data frame containing at x, y coordinates named "x.pos", "y.pos", for a tracklet.
#' 
#' @param step A numeric value specifying the number of time unit or records between the two positions from which distance should be computed.
#' 
#' @return This function returns a vector containing the distance between each points of the 
#' trajectory according to the specified step.
#'
#' @author Quentin PETITJEAN
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
#' # compute the distance traveled between each point of the trajectory
#' TrackDat[[1]][["distTraveled"]] <-
#'   MoveR::distTraveled(TrackDat[[1]], step = 1)
#' 
#' # check the result
#' str(TrackDat)
#' 
#' # sum all the distance to compute the total distance traveled by the particle over the trajectory
#' sum(TrackDat[[1]][["distTraveled"]], na.rm = T)
#'
#' @export

distTraveled <- function(df, step = 1){
  error <- .errorCheck(df = df, x.pos = "x.pos", y.pos = "y.pos")
  if(!is.null(error)){
    stop(error)
  }
  
  Res <-
    sapply(seq(1, nrow(df), by = step), function(i)
      sqrt((df[["x.pos"]][i + 1] - df[["x.pos"]][i])^ 2 +
             (df[["y.pos"]][i + 1] - df[["y.pos"]][i])^ 2))
  return(Res)
}
