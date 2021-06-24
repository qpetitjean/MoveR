
#' @title Load TrackR tracking output .csv file
#'
#' @description
#'
#' @seealso \code{\link{readCtrax}}, \code{\link{readTrex}}, \code{\link{readIdtracker}}, \code{\link{mirrorY}}
#'
#' @param trackRPath The path of the TrackR output file (.csv) to load within R environment
#' (e.g. "C:/Users/[username]/Desktop/video_folder/Results.csv")
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
#' @authors Quentin Petitjean
#'
#'
#'
#' @examples
#'
#' # TODO
#'
#' @export

readTrackR <- function(trackRPath,
                       mirrorY = FALSE,
                       imgHeight = NA) {
  tracking_data <- read.delim(trackRPath, sep = ",")
  
  # if mirrorY = TRUE, mirror the Y coordinates according to image height
  if (mirrorY == TRUE) {
    tracking_data$y = mirrorYFunc(tracking_data$y, imgHeight = imgHeight)
  }
  
  trackR_all <- list(
    trackR_Raw <- list(
      maj.ax = tracking_data$height,
      angle = tracking_data$angle,
      min.ax = tracking_data$width,
      x.pos = tracking_data$x,
      y.pos = tracking_data$y,
      identity = tracking_data$track,
      frame = tracking_data$frame,
      ntargets = NULL,
      timestamps = unique(tracking_data$frame)
    ),
    others <- list(n = tracking_data$n)
  )
  
  return(trackR_all)
}
