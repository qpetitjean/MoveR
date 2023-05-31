#' @title Import TrackR tracking output .csv file.
#'
#' @description Given the path of the .csv file corresponding to TrackR output,
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
#'
#' Alternatively, the function can returns a list containing 2 sublists, the first corresponding to the one mentioned above
#' and the second containing all the elements retrieved from the .csv file (see rawDat argument).
#' Also, the function can flip y coordinates (see flipY argument).
#'
#' @param trackRPath The full path of the TrackR output file (.csv).
#'
#' @param flipY A logical value (i.e., TRUE or FALSE) indicating whether the origin of y coordinates should be flipped. If TRUE, y coordinates are flipped to start on the top-left (default = FALSE).
#'
#' @param imgHeight A numeric value expressed in pixels, the length of Y axis
#' corresponding to the height of the image or video resolution (optional, only used when flipY = TRUE).
#'
#' @param frameR A numeric value expressed in frames per second, the frequency at which frames are recorded/displayed in the video (optional, only used to compute timestamps).
#'
#' @param rawDat TRUE or FALSE, if TRUE add a second list containing all the elements retrieved from .csv file (see \href{https://swarm-lab.github.io/trackR}{trackR}),
#' may drastically increase the size of the object returned by the function (default = FALSE).
#'
#' @return A list containing either a list of 9 elements classically used for further computations or a list containing 2 sublists, the first corresponding to the one previously mentioned
#' and the second containing all the elements retrieved from the .csv file (see rawDat argument). Also, by default the function returns y coordinates starting on the bottom-left.
#'
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link{readCtrax}}, \code{\link{readTrex}}, \code{\link{readIdtracker}}, \code{\link{flipYCoords}}
#'
#' @references 
#' \href{https://swarm-lab.github.io/trackR}{trackR}
#'
#' @examples
#' \dontrun{
#' 
#' # Download the first dataset from the sample data repository
#' Path2Data <- MoveR::DLsampleData(dataSet = 1, tracker = "TrackR")
#' Path2Data
#' 
#' # Import the list containing the 9 vectors classically used for further computation
#' # and flip Y coordinates to start on the top-left
#' Data <-
#'   MoveR::readTrackR(Path2Data[[1]],
#'          flipY = T,
#'          imgHeight = 2160,
#'          rawDat = F
#'   )
#' str(Data)
#'
#' # Import the list containing 2 sublists, the first containing the 9 vectors classically used for further computation
#' # and the second list containing all the elements retrieved from .csv file,
#' # also do not flip Y coordinates (start on the bottom-left)
#'
#' DataFull <-
#'   MoveR::readTrackR(Path2Data[[1]],
#'     rawDat = T,
#'     frameR = 25
#'   )
#'
#' str(DataFull)
#'
#' } 
#' @export

readTrackR <- function(trackRPath,
                       flipY = FALSE,
                       imgHeight = NULL,
                       frameR = NULL,
                       rawDat = FALSE) {
  if (flipY == TRUE & is.null(imgHeight)) {
    stop(
      "imgHeight argument is missing, the height of the image resolution is needed to flip y coordinates"
    )
  }
  if (is.null(frameR)) {
    warning(
      "frameR argument is missing: timestamps returned NA, a frame rate value is needed to compute timestamps"
    )
  }
  # Import output file
  if (inherits(try(read.delim(trackRPath, sep = ","), silent = TRUE)
               , "try-error")) {
    stop("No such file or directory : undefined or wrong path supplied")
    
  } else {
    trackDat <- read.delim(trackRPath, sep = ",")
  }
  
  # if flipY = TRUE, flip the Y coordinates according to image height
  if (flipY == TRUE) {
    trackDat$y = flipYCoords(trackDat$y, imgHeight = imgHeight)
  }
  # create ntargets, the number of particles detected for each frame (not in Raw output of TrackR)
  ntargets <- unlist(lapply(unique(trackDat[["frame"]]), function(x)
    length(trackDat[["track"]][which(trackDat[["frame"]] == x)])))
  
  if (rawDat == FALSE) {
    trackR_all <- list(
      maj.ax = trackDat[["height"]],
      angle = (trackDat[["angle"]] * pi) / 180,
      min.ax = trackDat[["width"]],
      x.pos = trackDat[["x"]],
      y.pos = trackDat[["y"]],
      identity = trackDat[["track"]],
      frame = trackDat[["frame"]],
      ntargets = ntargets,
      timestamps = ifelse(
        rep(is.null(frameR), length(unique(trackDat[["frame"]]))),
        rep(NA, length(unique(trackDat[["frame"]]))),
        unique(trackDat[["frame"]]) / frameR
      )
    )
  } else if (rawDat == TRUE) {
    trackR_all <- list(
      Data_trackR = list(
        maj.ax = trackDat[["height"]],
        angle = trackDat[["angle"]] * pi / 180,
        min.ax = trackDat[["width"]],
        x.pos = trackDat[["x"]],
        y.pos = trackDat[["y"]],
        identity = trackDat[["track"]],
        frame = trackDat[["frame"]],
        ntargets = ntargets,
        timestamps = ifelse(
          rep(is.null(frameR), length(unique(trackDat[["frame"]]))),
          rep(NA, length(unique(trackDat[["frame"]]))),
          unique(trackDat[["frame"]]) / frameR
        )
      ),
      Data_trackR_Raw = as.list(trackDat)
    )
  }
  return(trackR_all)
}
