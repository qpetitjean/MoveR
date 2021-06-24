#' @title Mirror Y coordinates
#'
#' @description Given a vector containing the Y coordinates of detected individuals and image height,
#' this function compute mirrored Y coordinates
#'
#'
#' @param YCoords A vector containing Y coordinates of detected individuals
#' 
#' @param imgHeight A numeric value expressed in pixels, the true length of Y axis 
#' corresponding to the height of the image or video resolution (default = 1080)
#'
#'
#' @return A vector containing new mirrored Y coordinates
#'
#' @authors Quentin Petitjean
#'
#'
#'
#' @examples
#'
#' # TODO
#'
#' @export

mirrorYFunc <- function(YCoords, imgHeight = NA) {
  if(is.na(imgHeight)){ 
    stop("imgHeight is not specified:\n impossible to mirror Y coordinates without the image resolution in Y axis")
    }
  
  mirrorY <- imgHeight - YCoords
  
  return(mirrorY)
}