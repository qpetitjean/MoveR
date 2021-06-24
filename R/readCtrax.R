#' @title Load Ctrax tracking output file
#'
#' @description Given a .mat file corresponding to Ctrax tracking output,
#' this function open the .mat file in R, reconstruct time serie and if needed mirror Y coordinates
#'
#' @seealso generate_time, mirrorY_coords
#'
#' @param ctraxPath The path of the Ctrax output file to load within R environment 
#' (e.g. "C:/Users/[username]/Desktop/video_folder/Ctrax_output.mat")
#' 
#' @param mirrorY TRUE or FALSE, set the origin of Y coords, if TRUE Y coords are mirrored
#' 
#' @param imgHeight A numeric value expressed in pixels, the true length of Y axis 
#' corresponding to the height of the image or video resolution (optional, only used when mirrorY = TRUE)
#' 
#' 
#'
#' @return A list containing tracking data
#' 
#'
#' @authors Quentin Petitjean, Vincent Calcagno
#'
#'
#'
#' @examples
#'
#' # TODO
#'
#' @export

readCtrax=function(ctraxPath, sep = ",", mirrorY = FALSE, imgHeight = NA){ 

  # Import output files from Ctrax in a list of df 
  if (inherits(try(readMat(ctraxPath, sep = sep), silent = TRUE)
               , "try-error")) {
    stop("undefined or wrong path supplied : No such file or directory")
    
  } else {
    Ctrax_Raw <- readMat(ctraxPath, sep = sep)
  }

  # generate a vector containing the corresponding frame number for each row of the dataset
  frame <- generateTime(Ctrax_Raw)
  
  # append the frame vector to the list of df
  Ctrax_Raw[["frame"]] <- frame
  Ctrax_Raw <- Ctrax_Raw[c("maj.ax", "angle", "min.ax", "x.pos", "y.pos", "identity", "frame", "ntargets", "timestamps", "startframe")]
  
  # if mirrorY is TRUE, use mirrorY_coords function to mirror Y coordinates
  if (mirrorY == TRUE) {
    Ctrax_Raw$y.pos = mirrorYFunc(Ctrax_Raw$y.pos, imgHeight = imgHeight)
  }
   return(Ctrax_Raw)  
}
