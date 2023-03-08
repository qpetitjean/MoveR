#' @title Summary statistics of the tracking data.
#'
#' @description Given a list of tracking tracklets containing cartesian coordinates,
#' this function returns 2 sublists containing a summary of video and tracklets data:
#'
#' \itemize{
#'    \item{"VideoSummary": }{a list containing 4 elements:
#'       \itemize{
#'          \item{"videoDuration_f": }{the length of the video expressed in frames.}
#'          \item{"videoDuration_s": }{the length of the video expressed in seconds.}
#'          \item{"frameR": }{the frame rate of the video as specified by the user.}
#'          \item{"scale": }{the scaling factor applied to the trajectory coordinates as specifed by the user.}
#'       }
#'      }
#'
#'    \item{"TrackletSummary": }{a list containing 8 elements:
#'       \itemize{
#'          \item{"trackNb": }{the total number of tracklets over the video (i.e., trajectories).}
#'          \item{"totTrackDuration_f": }{the sum of the duration of all tracklets expressed in frames.}
#'          \item{"totTrackDuration_s": }{the sum of the duration of all tracklets expressed in seconds}
#'          \item{"totTrackLength_unit": }{the sum of the length of all tracklets in spatial unit as specified by the user (e.g., pixels, cm).}
#'          \item{"trackId": }{the tracklets identity.}
#'          \item{"trackDuration_f": }{the duration of each tracklets expressed in frames.}
#'          \item{"trackDuration_s": }{the duration of each tracklets expressed in seconds.}
#'          \item{"trackLength_unit": }{the length of each tracklets in spatial unit as specified by the user (e.g., pixels, cm).}
#'       }
#'    }
#'  }
#' @param trackDat A list of data frame containing tracking information for each tracklet (i.e., x.pos, y.pos, frame).
#'
#' @param frameR A numeric value expressed in frames per second, the frequency at which frames are recorded/displayed in the video
#' (optional).
#'
#' @param scale A ratio corresponding to the scaling factor to be applied to the trajectory coordinates
#' (e.g., size in cm / size in pixels, (default = 1).
#'
#' @param units A character string specifying the spatial unit of the coordinates after scaling (default = "pixels").
#'
#' @param progress A Boolean (i.e., TRUE or FALSE) indicating whether a progress bar should be displayed to inform process progression.
#'
#' @return A summary of video and tracklets data.
#'
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link[trajr]{TrajScale}}, \code{\link[trajr]{TrajFromCoords}}
#'
#' @examples
#'
#' set.seed(2023)
#' # generate some dummy tracklets
#' ## start to specify some parameters to generate tracklets
#' TrackN <- 500 # the number of tracklet to simulate
#' TrackL <- 1:1000 # the length of the tracklets or a sequence to randomly sample tracklet length
#' 
#' TrackList <- stats::setNames(lapply(lapply(seq(TrackN), function(i)
#'   trajr::TrajGenerate(sample(TrackL, 1), random = TRUE, fps = 1)), function(j)
#'     data.frame(
#'       x.pos = j$x - min(j$x),
#'       y.pos = j$y - min(j$y),
#'       frame = j$time
#'     )), seq(TrackN))
#' 
#' # compute and display tracking summary
#' TrackSummary <- MoveR::trackStats(TrackList,
#'                                   frameR = 25,
#'                                   scale = 1,
#'                                   units = "pixels")
#' 
#' # retrieve the TrackLength and duration in frame from the summary "manually"
#' TrackL <- TrackSummary[["TrackletSummary"]][["trackLength_pixels"]]
#' TrackD <- TrackSummary[["TrackletSummary"]][["trackDuration_f"]]
#' 
#' # or using listGet utility
#' TrackL2 <- listGet(TrackSummary, "trackLength_pixels")
#' TrackD2 <- listGet(TrackSummary, "trackDuration_f")
#' 
#' # plot the distribution of tracklets length and duration
#' par(mfrow = c(2, 2))
#' hist(TrackL)
#' hist(TrackD)
#' hist(TrackL2)
#' hist(TrackD2)
#'
#' @export

trackStats = function(trackDat,
                      frameR = NULL,
                      scale = NULL,
                      units = NULL,
                      progress = TRUE) {
  if (is.null(units)) {
    units <- "pixels"
    warning("units argument is missing, default value is pixels")
  }
  if (is.null(scale)) {
    scale <- 1
    warning("scale argument is missing, default value is 1 (i.e., no scaling)")
  }
  if (is.null(frameR)) {
    warning("frameR argument is missing, metrics expressed in seconds will return NA")
  }
  
  # compute some basic summary about video
  ## compute the duration of the video in frame
  videoDuration_f <-
    max(unlist(lapply(trackDat, function (x)
      max(
        listGet(x, "frame")
      ))))
  ## compute the duration of the video in second
  videoDuration_s <- videoDuration_f / frameR
  
  # compute some basic summary about tracklets
  ## compute the number of tracklets
  Tracks <- names(trackDat)
  nbTrack <- length(trackDat)
  ## compute the duration of each tracklet in frame
  TrackDuration_f <- unname(unlist(lapply(trackDat, function (x)
    nrow(x))))
  ## compute the duration of each tracklet in second
  TrackDuration_s <- unname(TrackDuration_f / frameR)
  ## compute total tracks duration in frame
  totTrackDuration_f <- sum(TrackDuration_f)
  ## compute total tracks duration in second
  totTrackDuration_s <- sum(TrackDuration_s)
  ## compute tracks length in the specified units for each tracklet
  if (isTRUE(progress)) {
    ### initialize progress bar
    total = length(trackDat)
    pb <-
      progress::progress_bar$new(format = "tracklets processing [:bar] :current/:total (:percent)", total = total)
    pb$tick(0)
  }
  
  trjList <- list()
  for (i in seq(length(trackDat))) {
    trjTemp <-
      trajr::TrajFromCoords(trackDat[[i]][, c("x.pos", "y.pos", "frame")], timeCol = 3)
    trjList[[names(trackDat)[i]]] <-
      trajr::TrajScale(trjTemp, scale, units)
    if (isTRUE(progress)) {
      # progress bar
      pb$tick(1)
    }
  }
  TrackLength_u <- paste("trackLength", units, sep = "_")
  TrackLength <- unname(unlist(lapply(trjList, function(x)
    trajr::TrajLength(x))))
  
  ## compute total tracks length in the specified units
  totTrackLength <- sum(unlist(TrackLength))
  totTrackLength_u <- paste("totTrackLength", units, sep = "_")
  
  # some checking for Inf and NA values
  InfCheck <-
    names(which(unlist(lapply(lapply(lapply(trackDat, function (x)
      is.infinite(listGet(x, "x.pos"))), function (y)
        which(y == TRUE)), function (z)
          length(z))) > 0))
  if (length(InfCheck) > 0) {
    warning(
      "Infinite values have been found in x.pos for the tracklets reported below; \nthis could produce a biased summary; \nconsider using filterFrags",
      ": \n",
      paste(InfCheck, collapse = ", ")
    )
  }
  NACheck <-
    names(which(unlist(lapply(lapply(lapply(trackDat, function (x)
      is.na(
        listGet(x, "x.pos")
      )), function (x)
        which(x == TRUE)), function (x)
          length(x))) > 0))
  
  if (length(NACheck) > 0) {
    warning(
      "NA values have been found in x.pos for the tracklets reported below; \nthis could produce a biased summary; \nconsider using filterFrags",
      ": \n",
      paste(NACheck, collapse = ", ")
    )
  }
  
  Summary <- c(list(
    VideoSummary = list(
      videoDuration_f = videoDuration_f,
      videoDuration_s = videoDuration_s,
      frameR = frameR,
      scale = scale
    ),
    TrackletSummary = list(
      trackNb = nbTrack,
      totTrackDuration_f = totTrackDuration_f,
      totTrackDuration_s = totTrackDuration_s,
      totTrackLength_u = totTrackLength,
      trackId = Tracks,
      trackDuration_f = TrackDuration_f,
      trackDuration_s = TrackDuration_s,
      trackLength_u = TrackLength
    )
    
  ))
  names(Summary$TrackletSummary)[[4]] <- totTrackLength_u
  names(Summary$TrackletSummary)[[8]] <- TrackLength_u
  # display the summary
  str(Summary)
  return(Summary)
}