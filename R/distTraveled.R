#' @title Compute distance to something along a trajectory
#'
#'
#' @description Given a data frames containing tracking information for a given fragment, 
#' this function compute the euclidean distance along the trajectory according to a given step 
#' 
#'
#' @param df A data frame containing at x, y coordinates named "x.pos", "y.pos", for a fragment
#' 
#' @param step A numeric value specifying the number of unit between the two position for which distance will be computed
#' 
#' @return this function returns a vector containing the distance between each points of the 
#' trajectory according to the specified step
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

distTraveled <- function(df, Step = 1){
  Res <-
    sapply(seq(1, nrow(df), by = Step), function(i)
      sqrt((df$x.pos[i + 1] - df$x.pos[i])^ 2 +
             (df$y.pos[i + 1] - df$y.pos[i])^ 2))
  return(Res)
}

