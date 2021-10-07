#' @title Compute distance to the a given point
#'
#'
#' @description Given a data frames containing tracking information for a given fragment, 
#' this function compute the euclidean distance between a point and coordinates of the trajectory, 
#' the function returns the distance between the specified point and the individual along the trajectory 
#' 
#'
#' @param df A dataframe containing at least 2 values corresponding to x, y coordinates named "x.pos", "y.pos", for a fragment
#' 
#' @param obj A dataframe containing 2 values corresponding to x, y coordinates named "x.pos", "y.pos" 
#' and specifying the location of an object or a point of interest 
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


dist2Pt <- function(df, obj) {
  if (nrow(obj) > 1) {
    stop(
      "obj argument contains to many coordinates, obj dataframe must contains only 1 value for x.pos and y.pos"
    )
  }
  Res <-
    sapply(seq(nrow(df)), function(i)
      sqrt((df$x.pos[i] - obj$x.pos) ^ 2 +
             (df$y.pos[i] - obj$y.pos) ^ 2))
  
  return(unname(Res))
}
