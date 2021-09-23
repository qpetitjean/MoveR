#' @title Compute distance to the a given point
#'
#'
#' @description Given a data frames containing tracking information for a given fragment, 
#' this function compute the euclidean distance between a point and coordinates of the trajectory, 
#' the function returns the distance between the specified point and the individual along the trajectory 
#' 
#'
#' @param df A data frame containing at x, y coordinates named "x.pos", "y.pos", for a fragment
#' 
#' @param edge A data frame containing at x, y coordinates named "x.pos", "y.pos" and specifiyng the location of the point
#' 
#' 
#'
#' @return this function returns a vector containing the distance between each points of the 
#' trajectory and the specified point
#'
#'
#' @authors Quentin Petitjean
#'
#'
#'
#' @examples
#'
#' #TODO
#'
#' @export


dist2Pt <- function(df, refPt){
  
  distPt <-
    sapply(seq(nrow(df)), function(i)
      sqrt((refPt["x.pos"] - df$x.pos[i])^2 +
             (refPt["y.pos"] - df$y.pos[i]) ^2)
      )
  return(unname(distPt))
}
