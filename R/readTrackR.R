#' @title Import TrackR tracking output .csv file.
#'
#' @description Given the path of the .csv file corresponding to TrackR output,
#' this function returns an object of class "tracklets", a list of tracklets (data frame) containing 7 elements classically used for further computations using MoveR package:
#' \itemize{
#'    \item{'maj.ax': }{the length of the major axis (i.e., the midline) for a particle over frame (i.e., length of the ellipse).}
#'    \item{'angle': }{the particle's absolute angle in radians, orientation of the particle according to y-axis.}
#'    \item{'min.ax': }{the length of the minor axis for a particle over frame (i.e., width of the ellipse).}
#'    \item{'x.pos': }{x position of the particle's centroid.}
#'    \item{'y.pos': }{y position of the particle's centroid.}
#'    \item{'identity': }{the particle's identity given by the tracking software.}
#'    \item{'frame': }{the video frame number at which the measurements has been made.}
#' }
#'
#' The function can also append all the others elements returned by the tracking software (see [rawDat] argument)
#' Also, the function can flip y coordinates (see [flipY] argument).
#'
#' @param trackRPath The full path of the TrackR output file (.csv).
#'
#' @param flipY A logical value (i.e., TRUE or FALSE) indicating whether the origin of y coordinates should be flipped. If TRUE, y coordinates are flipped to start on the top-left (default = FALSE).
#'
#' @param imgHeight A numeric value expressed in pixels, the length of Y axis
#' corresponding to the height of the image or video resolution (optional, only used when flipY = TRUE).
#'
#' @param rawDat  A logical value (i.e., TRUE or FALSE) indicating whether all other elements retrieved from the tracking output should appended to the tracklets data (see \href{https://swarm-lab.github.io/trackR}{trackR}).
#' Note that this may drastically increase the size of the object returned by the function (default = FALSE).
#'
#' @return An object of class "tracklets" containing a list of tracklets and their characteristics classically used for further computations.
#' In case [rawDat] argument is TRUE, it also append all the others elements returned by the tracking software (for TrackR it corresponds to "n" the number of pixels covered by the object in a given frame). 
#' Also, by default the function returns y coordinates starting on the bottom-left.
#'
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link{readAnimalTA}} \code{\link{readCtrax}}, \code{\link{readTrex}}, \code{\link{readIdtracker}}, \code{\link{flipYCoords}}
#'
#' @references 
#' \href{https://swarm-lab.github.io/trackR}{trackR - Multi-object tracking with R}
#'
#' @examples
#' \dontrun{
#' 
#' # Download the first dataset from the sample data repository
#' Path2Data <- MoveR::DLsampleData(dataSet = 1, tracker = "TrackR")
#' Path2Data
#' 
#' # Import the data as an object of class "tracklets"
#' # also do not flip Y coordinates (start on the bottom-left)
#' Data <-
#'   MoveR::readTrackR(Path2Data[[1]])
#'   
#' str(Data)
#'
#' } 
#' @export

readTrackR <- function(trackRPath,
                       flipY = FALSE,
                       imgHeight = NULL,
                       rawDat = FALSE) {
  
  error <- .errorCheck(imgHeight = imgHeight)
  if (flipY == TRUE & !is.null(error)) {
    stop(error)
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
    trackDat[["y"]] <- flipYCoords(trackDat[["y"]], imgHeight = imgHeight)
  }
  
  if (rawDat == FALSE) {
    trackR_all <- list(
      maj.ax = trackDat[["height"]],
      angle = (trackDat[["angle"]] * pi) / 180,
      min.ax = trackDat[["width"]],
      x.pos = trackDat[["x"]],
      y.pos = trackDat[["y"]],
      identity = trackDat[["track"]],
      frame = trackDat[["frame"]]
    )
  } else if (rawDat == TRUE) {
    trackR_all = list(
        maj.ax = trackDat[["height"]],
        angle = trackDat[["angle"]] * pi / 180,
        min.ax = trackDat[["width"]],
        x.pos = trackDat[["x"]],
        y.pos = trackDat[["y"]],
        identity = trackDat[["track"]],
        frame = trackDat[["frame"]],
        n = trackDat[["n"]]
      )
  }
  
  # create a tracklets class object to return
  trackR_all <- MoveR::convert2Tracklets(trackR_all)
  
  return(trackR_all)
}
