#' @title tracking statistics
#'
#' @description Given a list of data frame containing tracking informations for each fragment,
#' this function compute and return various basic metrics about video and fragments
#'
#'
#' @param trackDat A list of data frame containing tracking informations for each fragment
#' (e.g., maj.ax, angle, min.ax, x.pos, y.pos, ...)
#'
#' @param frameR A numeric value expressed in frames per second - the frame rate corresponding to
#' the frequency at which frames are recorded/displayed in the video
#'
#' @param scale A ratio corresponding to the scaling factor to be applied to the trajectory coordinates 
#' (e.g., size in cm / size in pixels; see trajr::TrajScale())
#'
#' @param unit The unit expected after scaling (e.g., "cm", "m", ...)
#'
#'
#' @return A list of basic metrics about video and fragments
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

trackStats = function(trackDat,
                      frameR = NA,
                      scale = NA,
                      unit = "unit") {
  if (is.na(unit)) {
    warning(
      "no unit have been specified: \nnames of fragLength and totFragLength will return fragLength_NA and totFragLength_NA"
    )
  }
  if (is.na(scale)) {
    warning(
      "no scale have been specified: \nmetrics expressed in length (i.e., fragLength and totFragLength) will return NA"
    )
  }
  if (is.na(frameR)) {
    warning("no frameR have been specified: \nmetrics expressed in seconds will return NA")
  }
  
  
  # compute some basic summary about video
  ## compute the duration of the video in frame
  videoDuration_f <-
    max(unlist(lapply(trackDat, function (x)
      max(
        list_get(x, "frame")
      ))))
  ## compute the duration of the video in second
  videoDuration_s <- videoDuration_f / frameR
  
  # compute some basic summary about fragments
  ## compute the number of fragments
  frags <- names(trackDat)
  nbFrag <- length(trackDat)
  ## compute the duration of each fragment in frame
  fragsDuration_f <- unname(unlist(lapply(trackDat, function (x)
    dim(x)[1])))
  ## compute the duration of each fragment in second
  fragsDuration_s <- unname(fragsDuration_f / frameR)
  ## compute total frags duration in frame
  totFragsDuration_f <-  sum(fragsDuration_f)
  ## compute total frags duration in second
  totFragsDuration_s <-   sum(fragsDuration_s)
  ## compute frags length in cm for each fragment
  ### initialize progress bar
  total = length(trackDat)
  pb <-
    progress::progress_bar$new(format = "fragments processing [:bar] :current/:total (:percent)", total = total)
  pb$tick(0)
  Sys.sleep(0.001)
  
  trjList <- list()
  for (i in seq(length(trackDat))) {
    trj_temp <-  
      trajr::TrajFromCoords(trackDat[[i]][, c("x.pos", "y.pos", "frame")], spatialUnits = "pixels", timeCol = 3)
    trjList[[names(trackDat)[i]]] <-
      trajr::TrajScale(trj_temp, scale, "cm")
    # progress bar
    pb$tick(1)
    Sys.sleep(1 / 1000)
  }
  fragLength_unit <- paste("fragLength", unit, sep = "_")
  fragLength <- unname(unlist(lapply(trjList, function(x)
    trajr::TrajLength(x))))
  
  ## compute total frags length in cm
  totFragLength <- sum(unlist(fragLength))
  totFragLength_unit <- paste("totFragLength", unit, sep = "_")
  
  # some checking for Inf and NA values
  InfCheck <-
    names(which(unlist(lapply(lapply(lapply(trackDat, function (x)
      is.infinite(list_get(x, "x.pos"))), function (x)
        which(x == TRUE)), function (x)
          length(x))) > 0))
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
        list_get(x, "x.pos")
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
  return(Summary)
}
