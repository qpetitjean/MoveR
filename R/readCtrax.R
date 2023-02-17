#' @title Import Ctrax tracking output .mat file.
#'
#' @description Given the path of the .mat file corresponding to Ctrax output,
#' this function returns a list of 9 vectors classically used for further computations using MoveR package:
#' \itemize{
#'    \item{'maj.ax': }{the length of the major axis (i.e., the midline) for a particle over frame (i.e., length of the ellipse).}
#'    \item{'angle': }{the particle's absolute angle in radians, orientation of the particle according to y-axis.}
#'    \item{'min.ax': }{the length of the minor axis for a particle over frame (i.e., width of the ellipse).}
#'    \item{'x.pos': }{x position of the particle's centroid.}
#'    \item{'y.pos': }{y position of the particle's centroid.}
#'    \item{'identity': }{the particle's identity given by the tracking software.}
#'    \item{'frame': }{the video frame number at which the measurements has been made.}
#'    \item{'ntargets': }{the number of particle tracked over each frame.}
#'    \item{'timestamps': }{the elapsed time over each frame, in seconds.}
#' }
#' Also, the function can mirror y coordinates (see mirrorY argument).
#'
#' @param ctraxPath The full path of the Ctrax output file (.mat).
#'
#' @param mirrorY TRUE or FALSE, set the origin of y coordinates, if TRUE y coordinates are mirrored to start on the top-left (default = FALSE).
#'
#' @param imgHeight A numeric value expressed in pixels, the length of Y axis
#' corresponding to the height of the image or video resolution (optional, only used when mirrorY = TRUE).
#'
#'
#' @return A list containing 9 elements classically used for further computations.
#' Also, by default the function returns y coordinates starting on the bottom-left.
#'
#'
#' @author Quentin PETITJEAN, Vincent CALCAGNO
#'
#' @seealso \code{\link{readTrackR}}, \code{\link{readTrex}}, \code{\link{readIdtracker}}, \code{\link{mirrorYFunc}}
#'
#' @references 
#' Branson, K., Robie, A., Bender, J. et al. High-throughput ethomics in large groups of Drosophila. Nat Methods 6, 451–457 (2009). https://doi.org/10.1038/nmeth.1328.
#' \href{https://ctrax.sourceforge.net/}{Ctrax: The Caltech Multiple Walking Fly Tracker}
#'
#' @examples
#'
#' # Import the list containing the 9 vectors classically used for further computation
#' # and mirror Y coordinates to start on the top-left
#'
#' Data <-
#'   readCtrax(
#'     system.file("sampleData/sample_1/CtraxOutput", package = "MoveR"),
#'     mirrorY = T,
#'     imgHeight = 2160
#'   )
#'
#' # Import the list containing the 9 vectors classically used for further computation
#' # and do not mirror Y coordinates to start on the bottom-left
#'
#' Data <-
#'   readCtrax(
#'     system.file("sampleData/sample_1/CtraxOutput", package = "MoveR"),
#'     mirrorY = F,
#'     imgHeight = NULL
#'   )
#'
#' @export

readCtrax <-
  function(ctraxPath,
           mirrorY = FALSE,
           imgHeight = NULL) {
    if (mirrorY == TRUE & is.null(imgHeight)) {
      stop(
        "imgHeight argument is missing, the height of the image resolution is needed to mirror y coordinates"
      )
    }
    
    # Import output files from Ctrax
    if (inherits(try(R.matlab::readMat(ctraxPath), silent = TRUE)
                 , "try-error")) {
      stop("No such file or directory : undefined or wrong path supplied")
      
    } else {
      Ctrax_Raw <- R.matlab::readMat(ctraxPath)
    }
    
    # generate a vector containing the corresponding frame number for each row of the dataset
    frame <- c()
    frame_out <- 0
    for (t in Ctrax_Raw[["ntargets"]]) {
      frame <- c(allTimes, rep(frame_out, times = t))
      frame_out <-  frame_out + 1
    }
    
    # append the frame vector to the list of df
    Ctrax_Raw[["frame"]] <- frame
    Ctrax_Raw <-
      Ctrax_Raw[c(
        "maj.ax",
        "angle",
        "min.ax",
        "x.pos",
        "y.pos",
        "identity",
        "frame",
        "ntargets",
        "timestamps"
      )]
    
    # if mirrorY = TRUE, mirror the Y coordinates according to image height
    if (mirrorY == TRUE) {
      Ctrax_Raw$y.pos <-
        mirrorYFunc(Ctrax_Raw$y.pos, imgHeight = imgHeight)
    }
    return(Ctrax_Raw)
  }
