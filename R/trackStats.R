#' @title tracking statistics.
#'
#' @description Given a list of tracking fragments containing cartesian coordinates,
#' this function returns 2 sublists containing a summary of video and fragments data:
#'
#' \itemize{
#'    \item{"Video_summary": }{a list containing 4 elements:
#'       \itemize{
#'          \item{"videoDuration_f": }{the length of the video in frames.}
#'          \item{"videoDuration_s": }{the length of the video in seconds.}
#'          \item{"frameR": }{the frame rate of the video as specified by the user.}
#'          \item{"scale": }{the scaling factor applied to the trajectory coordinates as specifed by the user.}
#'       }
#'      }
#'
#'    \item{"fragments_summary": }{a list containing 8 elements:
#'       \itemize{
#'          \item{"fragnb": }{the total number of fragments over the video (i.e., trajectories).}
#'          \item{"totFragsDuration_f": }{the sum of the duration of all fragments in frames.}
#'          \item{"totFragsDuration_s": }{the sum of the duration of all fragments in seconds}
#'          \item{"totFragLength_unit": }{the sum of the length of all fragments in spatial unit as specified by the user (e.g., pixels, cm).}
#'          \item{"fragId": }{the fragment identity.}
#'          \item{"fragsDuration_f": }{the duration of each fragments in frames.}
#'          \item{"fragsDuration_s": }{the duration of each fragments in seconds.}
#'          \item{"fragLength_unit": }{the length of each fragments in spatial unit as specified by the user (e.g., pixels, cm).}
#'       }
#'    }
#'  }
#' @param trackDat A list of data frame containing tracking information for each fragment (i.e., x.pos, y.pos, frame).
#'
#' @param frameR A numeric value expressed in frames per second, the frequency at which frames are recorded/displayed in the video
#' (optional).
#'
#' @param scale A ratio corresponding to the scaling factor to be applied to the trajectory coordinates
#' (e.g., size in cm / size in pixels, (default = 1).
#'
#' @param units A character string specifying the spatial unit of the coordinates after scaling (default = "pixels").
#'
#' @return A summary of video and fragments data.
#'
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link[trajr]{TrajScale}}, \code{\link[trajr]{TrajFromCoords}}
#' 
#' @examples
#'
#'# generate some dummy fragments
#' ## start to specify some parameters to generate fragments
#'Fragn <- 500 # the number of fragment to simulate
#'FragL <- 1:1000 # the length of the fragments or a sequence to randomly sample fragment length
#'
#'fragsList <- stats::setNames(lapply(lapply(seq(Fragn), function(i)
#'  trajr::TrajGenerate(sample(FragL, 1), random = TRUE, fps = 1)), function(j)
#'    data.frame(
#'      x.pos = j$x - min(j$x),
#'      y.pos = j$y - min(j$y),
#'      frame = j$time
#'    )), seq(Fragn))
#'
#'# compute and display tracking summary
#'TrackSumary <- trackStats(fragsList,
#'                          frameR = 25,
#'                         scale = 1,
#'                         units = "pixels")
#'
#'# retrieve the fragLength and duration in frame from the summary "manually"
#'FragL <- TrackSumary[["fragments_summary"]][["fragLength_pixels"]]
#'FragD <- TrackSumary[["fragments_summary"]][["fragsDuration_f"]]
#'
#'# or using listGet utility
#'FragL2 <- listGet(TrackSumary, "fragLength_pixels")
#'FragD2 <- listGet(TrackSumary, "fragsDuration_f")
#'
#'# plot the distribution of fragments length and duration
#'par(mfrow = c(2, 2))
#'hist(FragL)
#'hist(FragD)
#'hist(FragL2)
#'hist(FragD2)
#'
#' @export

trackStats = function(trackDat,
                      frameR = NULL,
                      scale = NULL,
                      units = NULL) {
  if (is.null(units)) {
    units <- "pixels"
    warning(
      "units argument is missing, default value is pixels"
    )
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
  
  # compute some basic summary about fragments
  ## compute the number of fragments
  frags <- names(trackDat)
  nbFrag <- length(trackDat)
  ## compute the duration of each fragment in frame
  fragsDuration_f <- unname(unlist(lapply(trackDat, function (x)
    nrow(x))))
  ## compute the duration of each fragment in second
  fragsDuration_s <- unname(fragsDuration_f / frameR)
  ## compute total frags duration in frame
  totFragsDuration_f <- sum(fragsDuration_f)
  ## compute total frags duration in second
  totFragsDuration_s <- sum(fragsDuration_s)
  ## compute frags length in the specified units for each fragment
  ### initialize progress bar
  total = length(trackDat)
  pb <-
    progress::progress_bar$new(format = "fragments processing [:bar] :current/:total (:percent)", total = total)
  pb$tick(0)
  Sys.sleep(0.001)
  
  trjList <- list()
  for (i in seq(length(trackDat))) {
    trjTemp <-
      trajr::TrajFromCoords(trackDat[[i]][, c("x.pos", "y.pos", "frame")], timeCol = 3)
    trjList[[names(trackDat)[i]]] <-
      trajr::TrajScale(trjTemp, scale, units)
    # progress bar
    pb$tick(1)
    Sys.sleep(1 / 1000)
  }
  fragLength_unit <- paste("fragLength", units, sep = "_")
  fragLength <- unname(unlist(lapply(trjList, function(x)
    trajr::TrajLength(x))))
  
  ## compute total frags length in the specified units
  totFragLength <- sum(unlist(fragLength))
  totFragLength_unit <- paste("totFragLength", units, sep = "_")
  
  # some checking for Inf and NA values
  InfCheck <-
    names(which(unlist(lapply(lapply(lapply(trackDat, function (x)
      is.infinite(listGet(x, "x.pos"))), function (y)
        which(y == TRUE)), function (z)
          length(z))) > 0))
  if (length(InfCheck) > 0) {
    warning(
      "Infinite values have been found in x.pos for the fragments reported below; \nthis could produce a biased summary; \nconsider using filterFrags",
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
      "NA values have been found in x.pos for the fragments reported below; \nthis could produce a biased summary; \nconsider using filterFrags",
      ": \n",
      paste(NACheck, collapse = ", ")
    )
  }
  
  Summary <- c(list(
    Video_summary = list(
      videoDuration_f = videoDuration_f,
      videoDuration_s = videoDuration_s,
      frameR = frameR,
      scale = scale
    ),
    fragments_summary = list(
      fragnb = nbFrag,
      totFragsDuration_f = totFragsDuration_f,
      totFragsDuration_s = totFragsDuration_s,
      totFragLength_unit = totFragLength,
      fragId = frags,
      fragsDuration_f = fragsDuration_f,
      fragsDuration_s = fragsDuration_s,
      fragLength_unit = fragLength
    )
    
  ))
  names(Summary$fragments_summary)[[4]] <- totFragLength_unit
  names(Summary$fragments_summary)[[8]] <- fragLength_unit
  # display the summary
  str(Summary)
  return(Summary)
}
