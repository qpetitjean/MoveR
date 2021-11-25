#' @title LocaPos
#'
#' @description determine the position of a particles along a trajectory according to a reference matrix
#' 
#'
#' @param RefMat A distance matrix or path to a file containing a distance matrix representing 
#' the canevas of the arena or distance to any object (e.g., create a distance map using ImageJ)
#'
#' @param df A data frame containing x, y coordinates and speed columns named "x.pos", "y.pos", "speed" for a fragment
#'
#'
#' @return This function returns a vector containing the position of a particule according to a reference matrix along its trajectory
#'
#' @authors Quentin Petitjean
#'
#'
#' @examples
#'
#' #TODO
#'
#' @export
#' 

locaPos <- function(RefMat = NULL, df = NULL){
  if(is.null(RefMat)){
    stop("RefMatPath parameter is not specified, the function need a reference distance matrix to compute location of the given particle")
  }
  if(is.null(df)){
    stop("df parameter is not specified, \nthe function need a data frame containing x and y coordinates of a given particules along a trajectory to compute location")
  }
  if(!is.data.frame(df)){
    stop("df is not a dataframe: \nconsider transforming the data, the function need a data frame containing x and y coordinate of a given particules along a trajectory to compute its relative location")
  }
  if(!is.data.frame(df)){
    stop("df does not contain x.pos and y.pos: \nverify that x and y coordinates of the particles is present in the df or are named x.pos and y.pos")
  }  
  if(is.character(RefMat)){
RefMat <- read.delim(RefMat)
  } else {
    RefMat <- RefMat
  }
# find the position of the particle along its trajectory according to the reference matrix
sapply(seq(nrow(df)), function(y)  RefMat[round(df$y.pos[y]), round(df$x.pos[y])])
}
