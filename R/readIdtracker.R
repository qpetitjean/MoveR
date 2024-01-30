#' @title Import idtracker.ai tracking output .npy file.
#'
#' @description Given the path of the .npy file corresponding to idtracker output,
#' this function returns an object of class "tracklets", a list of tracklets (data frame) containing 7 elements classically used for further computations using MoveR package:
#' \itemize{
#'    \item{'maj.ax': }{the length of the major axis (i.e., the midline) for a particle over frame (i.e., length of the ellipse), returns NA since it is not present in the Idtracker output.}
#'    \item{'angle': }{the particle's absolute angle in radians, orientation of the particle according to y-axis, returns NA since it is not present in the Idtracker output.}
#'    \item{'min.ax': }{the length of the minor axis for a particle over frame (i.e., width of the ellipse), returns NA since it is not present in the Idtracker output.}
#'    \item{'x.pos': }{x position of the particle's centroid.}
#'    \item{'y.pos': }{y position of the particle's centroid.}
#'    \item{'identity': }{the particle's identity given by the tracking software.}
#'    \item{'frame': }{the video frame number at which the measurements has been made.}
#' }
#' 
#' Also, the function can flip y coordinates (see [flipY] argument).
#'
#' @param IdtrackerPath The full path of the idtrackerai output file (.npy).
#'
#' @param flipY A logical value (i.e., TRUE or FALSE) indicating whether the origin of y coordinates should be flipped. If TRUE, y coordinates are flipped to start on the bottom-left (default = FALSE).
#' 
#' @param imgHeight A numeric value expressed in pixels, the length of Y axis
#' corresponding to the height of the image or video resolution (optional, only used when flipY = TRUE).
#'
#'
#' @return An object of class "tracklets" containing a list of tracklets and their characteristics classically used for further computations.
#' Also, by default the function returns y coordinates starting on the top-left.
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link{readAnimalTA}} \code{\link{readCtrax}}, \code{\link{readTrackR}}, \code{\link{readTrex}}, \code{\link{flipYCoords}}
#'
#' @references 
#' Romero-Ferrero, F., Bergomi, M.G., Hinz, R.C. et al. idtracker.ai: tracking all individuals in small or large collectives of unmarked animals. Nat Methods 16, 179–182 (2019). https://doi.org/10.1038/s41592-018-0295-5.
#' \href{https://idtrackerai.readthedocs.io/en/latest/}{idtracker.ai}
#'
#' @examples
#' \dontrun{
#' 
#' # Download the first dataset from the sample data repository
#' Path2Data <- MoveR::DLsampleData(dataSet = 1, tracker = "IdTracker")
#' Path2Data
#'
#' # Import the list containing the 9 vectors classically used for further computation
#' # and flip Y coordinates to start on the bottom-left
#' Data <-
#'   MoveR::readIdtracker(Path2Data[[1]],
#'          flipY = T,
#'          imgHeight = 2160
#'   )
#' str(Data)
#'
#' } 
#' @export

readIdtracker <- function(IdtrackerPath,
                          flipY = FALSE,
                          imgHeight = NULL) {
  
  error <- .errorCheck(imgHeight = imgHeight)
  if (flipY == TRUE & !is.null(error)) {
    stop(error)
  }

  # import numpy python module to read .npy files
  np <-
    reticulate::import("numpy")
  
  # Import output files from idtrackerai
  if (inherits(try(np$load(IdtrackerPath,
                           allow_pickle = TRUE), silent = TRUE)
               , "try-error")) {
    stop("No such file or directory : undefined or wrong path supplied")
    
  } else {
    idtrackerData <-
      np$load(IdtrackerPath,
              allow_pickle = TRUE)
  }
  
  # create x.pos and y.pos vectors containing cartesian coordinates for all detected particles
  x.pos <- c(idtrackerData[[1]]$trajectories[, , 1])
  y.pos <- c(idtrackerData[[1]]$trajectories[, , 2])
  
  # create identity and frame vector containing the numeric identity and the sequence of frame for each particle, respectively
  identity <-
    unlist(lapply(seq(ncol(
      idtrackerData[[1]]$trajectories[, , 2]
    )),
    function(x)
      rep(
        x, nrow(idtrackerData[[1]]$trajectories[, , 1])
      )))
  frame <-
    unlist(lapply(seq(ncol(
      idtrackerData[[1]]$trajectories[, , 2]
    )),
    function(x)
      seq(nrow(
        idtrackerData[[1]]$trajectories[, , 1]
      ))))
  

  # if flipY = TRUE, flip the Y coordinates according to image height
  if (flipY == TRUE) {
    y.pos = flipYCoords(y.pos, imgHeight = imgHeight)
  }
  
  idtrackerRaw <- list(
    maj.ax = as.numeric(rep(NA, length(x.pos))),
    angle = as.numeric(rep(NA, length(x.pos))),
    min.ax = as.numeric(rep(NA, length(x.pos))),
    x.pos = x.pos,
    y.pos = y.pos,
    identity = identity,
    frame = frame
  )
  
  # create a tracklets class object to return
  idtrackerRaw <- MoveR::convert2Tracklets(idtrackerRaw)
  
  # fill some attributes
  idtrackerRaw <- MoveR::setInfo(idtrackerRaw, frameR = idtrackerData[[1]]$frames_per_second)
  
  
  return(idtrackerRaw)
}
