#' @title Load idtracker.ai tracking output .npy file
#'
#' @description Given the path of the .npy file corresponding to idtracker output,
#' this function returns a list of 9 vectors classically used for further computations using MoveR package:
#' \itemize{
#'    \item{'maj.ax': }{the length of the major axis (i.e., the midline) for a particle over frame (i.e., length of the ellipse), returns NA since it is not present in the Idtracker output.}
#'    \item{'angle': }{the particle's absolute angle in radians, orientation of the particle according to y-axis, returns NA since it is not present in the Idtracker output.}
#'    \item{'min.ax': }{the length of the minor axis for a particle over frame (i.e., width of the ellipse), returns NA since it is not present in the Idtracker output.}
#'    \item{'x.pos': }{x position of the particle's centroid.}
#'    \item{'y.pos': }{y position of the particle's centroid.}
#'    \item{'identity': }{the particle's identity given by the tracking software.}
#'    \item{'frame': }{the video frame number at which the measurements has been made.}
#'    \item{'ntargets': }{the number of particle tracked over each frame.}
#'    \item{'timestamps': }{the elapsed time over each frame, in seconds.}
#' }
#' Also, by default the function mirror y coordinates to start on the bottom-left (see mirrorY argument).
#'
#' @param IdtrackerPath The full path of the idtrackerai output file (.npy).
#'
#' @param mirrorY TRUE or FALSE, set the origin of y coordinates, if TRUE y coordinates are mirrored to start on the bottom-left (default = TRUE).
#'
#' @param imgHeight A numeric value expressed in pixels, the length of Y axis
#' corresponding to the height of the image or video resolution (optional, only used when mirrorY = TRUE).
#'
#' @param frameR A numeric value expressed in frames per second, the frequency at which frames are recorded/displayed in the video 
#' (optional, only used to compute timestamps).
#'
#'
#' @return A list containing 9 elements classically used for further computations.
#' Also, by default the function returns y coordinates starting on the bottom-left.
#'
#'
#' @authors Quentin PETITJEAN
#'
#' @seealso \code{\link{readCtrax}}, \code{\link{readTrackR}}, \code{\link{readTrex}}, \code{\link{mirrorYFunc}}
#'
#' @examples
#'
#' # Load the list containing the 9 vectors classically used for further computation
#' # and mirror Y coordinates to start on the bottom-left
#'
#' Data <-
#'   readIdtracker(
#'     system.file("sampleData/sample_1/IdTrackerOutput", package = "MoveR"),
#'     mirrorY = T,
#'     imgHeight = 2160
#'   )
#'
#' # Load the list containing the 9 vectors classically used for further computation
#' # and do not mirror Y coordinates to start on the top-left
#'
#' Data <-
#'   readIdtracker(
#'     system.file("sampleData/sample_1/IdTrackerOutput", package = "MoveR"),
#'     mirrorY = F,
#'     imgHeight = NULL
#'   )
#'
#' @export

readIdtracker <- function(IdtrackerPath,
                          mirrorY = TRUE,
                          imgHeight = NULL,
                          frameR = NULL) {
  if (mirrorY == TRUE & is.null(imgHeight)) {
    stop(
      "imgHeight argument is missing, the height of the image resolution is needed to mirror y coordinates"
    )
  }
  if (is.null(frameR)) {
    warning(
      "frameR argument is missing: timestamps returned NA, a frame rate value is needed to compute timestamps"
    )
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
    idtracker_data <-
      np$load(IdtrackerPath,
              allow_pickle = TRUE)
  }
  
  # create x.pos and y.pos vectors containing cartesian coordinates for all detected particles
  x.pos <- c(idtracker_data[[1]]$trajectories[, , 1])
  y.pos <- c(idtracker_data[[1]]$trajectories[, , 2])
  
  # create identity and frame vector containing the numeric identity and the sequence of frame for each particle, respectively
  identity <-
    unlist(lapply(seq(ncol(
      idtracker_data[[1]]$trajectories[, , 2]
    )),
    function(x)
      rep(
        x, nrow(idtracker_data[[1]]$trajectories[, , 1])
      )))
  frame <-
    unlist(lapply(seq(ncol(
      idtracker_data[[1]]$trajectories[, , 2]
    )),
    function(x)
      seq(nrow(
        idtracker_data[[1]]$trajectories[, , 1]
      ))))
  
  # create ntargets vector containing the number of particles detected within each frame
  ntargets <-
    unlist(lapply(seq(nrow(idtracker_data[[1]]$trajectories[, , 1])),
                  function(x)
                    length(idtracker_data[[1]]$trajectories[, , 1][x, ]) - length(which(
                      is.na(idtracker_data[[1]]$trajectories[, , 1][x, ])
                    ))))
  
  # if mirrorY = TRUE, mirror the Y coordinates according to image height
  if (mirrorY == TRUE) {
    y.pos = mirrorYFunc(y.pos, imgHeight = imgHeight)
  }
  
  idtrackerRaw <- list(
    maj.ax = as.numeric(rep(NA, length(x.pos))),
    angle = as.numeric(rep(NA, length(x.pos))),
    min.ax = as.numeric(rep(NA, length(x.pos))),
    x.pos = x.pos,
    y.pos = y.pos,
    identity = identity,
    frame = frame,
    ntargets = ntargets,
    timestamps = ifelse(
      rep(is.null(frameR), length(unique(frame))),
      rep(NA, length(unique(frame))),
      unique(frame) / frameR
    )
  )
  
  return(idtrackerRaw)
}
