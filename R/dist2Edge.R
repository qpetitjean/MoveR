#' @title Compute distance to the edge of an object (e.g., the arena) along a trajectory
#'
#'
#' @description Given a data frames containing tracking information for a given fragment, 
#' this function compute the euclidean distance between points defining the edge of an object (e.g., the arena)
#' and coordinates of the trajectory, the function returns the distance between each points of the 
#' trajectory and the closest point of the object edge
#' 
#'
#' @param df A data frame containing at x, y coordinates named "x.pos", "y.pos", for a fragment
#' 
#' @param edge A data frame containing at x, y coordinates named "x.pos", "y.pos" and specifiyng the location of the 
#' arena or any object edge
#' 
#' @param customFunc A function used to specify the formula used to compute the 
#' distance between a given object or arena border and individuals along a trajectory
#'
#' @return this function returns a vector containing the distance between each points of the 
#' trajectory and the closest point of the object edge
#'
#'
#' @authors Quentin Petitjean
#'
#'
#' @examples
#'
#' #TODO
#'
#' @export

dist2Edge <- function(df, edge, customFunc){
  Res <-
    sapply(seq(nrow(df)), customFunc)
  return(Res)
}

