#' @title Resampling tracklets.
#'
#' @description Given a list of data frame containing tracking informations for each tracklet,
#' this function returns a list of resampled tracklets according to the length of the resampling
#' step specified by the Tstep argument.
#'
#'
#' @param trackDatA A list of data frame containing tracking informations for each tracklet.
#'
#' @param TimeCol A character string corresponding to the name of the column containing time information (e.g., "frame").
#'
#' @param Tstep A numeric value corresponding to the length of the resampling step according to the time unit used in TimeCol (e.g., frame).
#'
#' @param progress A logical value (i.e., TRUE or FALSE) indicating whether a progress bar should be displayed to inform process progression.
#'
#' @return A list of data frame containing the resampled tracklets.
#'
#' @author Quentin PETITJEAN
#'
#'
#' @examples
#'
#' set.seed(2023)
#' # generate some dummy tracklets
#' ## start to specify some parameters to generate tracklets
#' trackN <- 50 # the number of tracklets to simulate
#' trackL <-
#'   100:1000 # the length of the tracklets or a sequence to randomly sample tracklets length
#'
#' trackList <- stats::setNames(lapply(lapply(seq(trackN), function(i)
#'   trajr::TrajGenerate(sample(trackL, 1), random = TRUE, fps = 1)), function(j)
#'     data.frame(
#'       x.pos = j$x - min(j$x),
#'       y.pos = j$y - min(j$y),
#'       frame = j$time
#'     )), seq(trackN))
#'
#' # check the tracklets
#' MoveR::drawTracklets(trackList,
#'                  timeCol = "frame")
#'
#' # convert the time unit (frame) to seconds using analyseTracklets (according to a frame rate of 25 fps)
#' trackListV1 <-
#'   MoveR::analyseTracklets(trackList,
#'                       customFunc = list(
#'                         # convert the time expressed in frame in second using a conversion factor of 25 frame per second
#'                         TimeSec = function(x)
#'                           x[["frame"]] / 25
#'                       ))
#' # example 1: resample the tracklets every 0.2 seconds
#' ## check the size of the tracklets
#' trackSize <- unlist(lapply(trackListV1, function(x)
#'   nrow(x)))
#'
#' ## resample the tracklets every 1 seconds
#' trackListSampled1S <- MoveR::resamplTracklets(trackListV1,
#'                                            TimeCol = "TimeSec",
#'                                            Tstep = 1)
#'
#' ## check the size of the tracklets after resampling
#' trackSize1s <- unlist(lapply(trackListSampled1S, function(x)
#'   nrow(x)))
#'
#' ## Compare the tracklets size
#' cbind(trackSize, trackSize1s)
#'
#' # example 2: resample the tracklets every 1 minutes
#'
#' ## resample the tracklets every 0.10 seconds
#' trackListSampled0.10S <- MoveR::resamplTracklets(trackListV1,
#'                                               TimeCol = "TimeSec",
#'                                               Tstep = 0.10)
#' ## here some time step are not found in the tracklets' list (e.g., 0.10, 0.30),
#' ## they are hence replaced by NA in the output list
#'
#' ## check the size of the tracklets after resampling
#' trackSize0.10S <-
#'   unlist(lapply(trackListSampled0.10S, function(x)
#'     nrow(x)))
#'
#' ## Compare the tracklets size
#' cbind(trackSize, trackSize1s, trackSize0.10S)
#'
#' @export

resamplTracklets <-
  function(trackDat,
           TimeCol = NULL,
           Tstep = NULL,
           progress = TRUE) {
    if (is.null(Tstep)) {
      stop(
        "Tstep argument is missing: a value corresponding to the resampling step is needed to resample the data"
      )
    }
    if (is.null(TimeCol)) {
      stop(
        "TimeCol argument is missing: the name of the column carying time information is needed to resample the data"
      )
    }
    if (is.data.frame(trackDat)) {
      trackDat <- list(trackDat)
    }
    if (isTRUE(progress)) {
      # initialize progress bar
      total = length(trackDat)
      pb <-
        progress::progress_bar$new(format = "Resampling tracklets [:bar] :current/:total (:percent)", total = total)
      pb$tick(0)
    }
    
    for (i in seq(length(trackDat))) {
      # resampling tracklets according to Tstep
      ## identify the first time unit where the tracklet is detected
      start <-
        trackDat[[i]][trackDat[[i]][[TimeCol]] == sort(trackDat[[i]][[TimeCol]]),][1,]
      
      ## use it as the starting point of the increment for resempling
      increment <-
        seq(from = start[[TimeCol]],
            to = max(trackDat[[i]][[TimeCol]], na.rm = T),
            by = Tstep)
      selVal <-
        as.character(increment) %in% as.character(signif(trackDat[[i]][[TimeCol]],
                                                         digits = max(nchar(increment),
                                                                      na.rm = T)))
      ## store the time values to keep and detect which have to be added
      toKeepTemp <- increment[selVal]
      
      ## retrieve data of the tracklet and add a row containing NA when a time value is missing
      toKeep <-
        trackDat[[i]][trackDat[[i]][[TimeCol]] %in% toKeepTemp, ]
      if (FALSE %in% selVal) {
        toAdd <-
          setNames(data.frame(matrix(
            ncol = ncol(trackDat[[i]]),
            nrow = length(increment[which(selVal == FALSE)])
          )), c(names(trackDat[[i]])))
        toAdd[TimeCol] <-
          c(unlist(list(increment[which(selVal == FALSE)])))
        toKeep <-
          rbind(toKeep, toAdd)[order(rbind(toKeep, toAdd)[[TimeCol]]),]
        if ("trackletId" %in% names(trackDat[[i]]) == TRUE) {
          w1 <-
            paste("In tracklet", unique(trackDat[[i]]$trackletId), sep = " ")
          w2 <-
            paste("\n some time units are missing",
                  list(increment[which(selVal == FALSE)]) ,
                  sep = " ")
          w3 <-
            ",\n resampling have added lines containing NA, perhaps consider modifying Tstep"
          warning(w1, ": ", w2, w3)
        } else {
          w1 <-
            paste("In tracklet", unique(trackDat[[i]]$identity), sep = " ")
          w2 <-
            paste("\n some time units are missing",
                  list(increment[which(selVal == FALSE)]) ,
                  sep = " ")
          w3 <-
            ",\n resampling have added lines containing NA, perhaps consider modifying Tstep"
          warning(w1, ": ", w2, w3)
          
        }
      }
      trackDat[[i]] <- toKeep
      
      if (isTRUE(progress)) {
        # progress bar
        pb$tick(1)
      }
    }
    
    if (length(trackDat) == 1) {
      trackDat <- trackDat[[1]]
    }
    return(trackDat)
  }
