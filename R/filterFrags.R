#' @title Apply a custom filter on tracklets.
#'
#' @description  Given a list of data frame containing tracking information for each tracklet and the result
#' of a specified condition' test as returned by filterFunc this function remove the values that do not meet the condition
#' of the test and split the tracklets accordingly. Also by specifying an additional "minDur" argument,
#' filtered tracklets that have a low number of records can be removed.
#' The function hence returns two sublist, the first containing a summary of the information about the tracklets before and after filtering
#' and the second containing a list with the filtered tracklets according to the condition test and "minDur" argument.
#'
#' @param trackDat A list of data frames containing tracking information for each tracklet
#'
#' @param filter A list of vector as returned by filterFunc and containing the result of a condition test.
#'
#' @param splitCond The result of the condition test for which values have to be removed and tracklets have to be splitted (Default: TRUE).
#'
#' @param minDur The minimum duration (i.e., number of records) to consider a tracklet valid after the filtering (default = 1).
#'
#' @param progess A Boolean (i.e., TRUE or FALSE) indicating whether a progress bar should be displayed to inform process progression.
#'
#' @return This function returns a list containing two sublists:
#'    \itemize{
#'          \item{"SummaryFiltering": }{a list containing a summary of the informations about the tracklets before and after filtering:
#'       \itemize{
#'          \item{"Tracknb_before_filter": }{the initial number of tracklets.}
#'          \item{"Tracknb_after_filter": }{the number of remaining tracklet after filtering, according to the "filter" argument.}
#'          \item{"Tracknb_after_minDur": }{the number of remaining tracklet after filtering that have a number of records above "minDur" argument.}
#'          \item{"TotTrackDuration_before_filter": }{the sum of the records belonging to each tracklet before the filtering.}
#'          \item{"TotTrackDuration_after_filter": }{the sum of the records belonging to each tracklet after the filtering.}
#'          \item{"TotTrackDuration_after_minDur": }{the sum of the records belonging to each tracklet that have a number of records above "minDur" argument after the filtering.}
#'          \item{"%Data_kept_after_filter": }{the percent of records remaining afer the filtering.}
#'          \item{"%Data_kept_after_minDur": }{the percent of records remaining afer the filtering for the tracklets that have a number of records above "minDur" argument only.}
#'       }}
#'          \item{"CleanedTracklets": }{a list of data frames containing the filtered tracklets according to the condition test specified by filterFunc and "minDur" argument.}
#'         }
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link{filterFunc}}
#'
#' @examples
#'
#' # Download the first dataset from the sample data repository
#' Path2Data <- MoveR::dlSampleDat(dataSet = 1, tracker = "TRex")
#' Path2Data
#'
#' # Import the list containing the 9 vectors classically used for further computation
#' Data <- MoveR::readTrex(Path2Data[[1]])
#'
#' # convert it to a list of tracklets
#' trackDat <- MoveR::convert2frags(Data[1:7], by = "identity")
#'
#' # example 1:
#' ## test for the presence of infinite value in x.pos, if infinite values are detected, the result is TRUE
#' FiltInf <- MoveR::filterFunc(trackDat, toFilter = "x.pos", customFunc = function(x) is.infinite(x))
#'
#' ## Then remove infinite values and split the tracklets when an infinite value is found
#' ## here we keep every remaining tracklet, whatever its duration (the number of record within each tracklet)
#' trackDatNoInf <- MoveR::filterFrags(trackDat, filter = FiltInf, splitCond = TRUE, minDur = 1)
#'
#' ## Check the summary of the filtering
#' str(trackDatNoInf$SummaryFiltering)
#'
#' ## take a look at the filtered tracklets list
#' str(trackDatNoInf$CleanedTracklets)
#'
#' ## alternatively, we can only keep the tracklets with a duration above 10 (the number of record within each tracklet)
#' ## by modifying the minDur argument
#' trackDatNoInfDur10 <- MoveR::filterFrags(trackDat, filter = FiltInf, splitCond = TRUE, minDur = 10)
#'
#' ## Check the summary of the filtering
#' str(trackDatNoInfDur10$SummaryFiltering)
#'
#' ## take a look at the filtered tracklets list
#' str(trackDatNoInfDur10$CleanedTracklets)
#'
#' # example 2: test for the length of the particles:
#' ## keep only particles with a size included between 1 and 20 pixels
#' ## if particles' size is ranging between 1 and 20 pixels, the result is TRUE.
#' FiltSize <- MoveR::filterFunc(trackDat, toFilter = "maj.ax", customFunc = function(x) x >= 1 & x <= 20)
#'
#' ## Then remove values that are not included within the [1:20] interval and split the tracklets accordingly
#' ## here we keep every remaining tracklet, whatever its duration (the number of record within each tracklet)
#' trackDatSize20 <- MoveR::filterFrags(trackDat, filter = FiltSize, splitCond = TRUE, minDur = 1)
#'
#' ## Check the summary of the filtering
#' str(trackDatSize20$SummaryFiltering)
#'
#' ## take a look at the filtered tracklets list
#' trackDatSize20$CleanedTracklets
#'
#' ## alternatively, we can only keep the tracklets with a duration above 10 (the number of record within each tracklet) by modifying the minDur argument
#' trackDatSize20Dur10 <- MoveR::filterFrags(trackDat, filter = FiltSize, splitCond = TRUE, minDur = 10)
#'
#' ## Check the summary of the filtering
#' str(trackDatSize20Dur10$SummaryFiltering)
#'
#' ## take a look at the filtered tracklets list
#' trackDatSize20Dur10$CleanedTracklets
#'
#' @export

filterFrags <- function(trackDat,
                        filter,
                        splitCond = TRUE,
                        minDur = 1,
                        progess = TRUE) {
  # compute some basic summary about tracklets
  ## compute the number of tracklets
  Tracks <- names(trackDat)
  nbTrack <- length(trackDat)
  ## compute the length (duration) of each tracklet before filtering (expressed as the number of records)
  TracksDuration <- lapply(trackDat, function (x)
    nrow(x))
  ## compute total tracklets duration before filtering (expressed as the number of records)
  totTracksDuration <-  sum(unlist(TracksDuration))
  
  # create an empty list to append filtered tracklets
  allCorrTracks <- list()
  # create an empty vector to store the number of new tracklets made from the original
  nbNew <- vector()
  # create an empty vector to store the number of new tracklets made from the original after filtering for minimum duration
  nbGood <- vector()
  # create an empty vector to store the duration of new tracklets made from the original
  TracksDurationNew <- vector()
  # create an empty vector to store the duration of new tracklets made from the original after filtering for minimum duration
  TracksDurationGood <- vector()
  
  # use custom function to filter tracklets
  
  if (length(trackDat) > length(filter)) {
    stop(
      "trackDat and filter have different length: the result of the condition test is missing for some tracklets"
    )
  } else if (length(trackDat) < length(filter)) {
    stop(
      "trackDat and filter have different length: the result of the condition test is longer than the number of tracklets"
    )
  } else if (length(trackDat) == length(filter)) {
    if (isTRUE(progess)) {
      # initialize progress bar
      total = length(Tracks)
      pb <-
        progress::progress_bar$new(format = "tracklets processing [:bar] :current/:total (:percent)", total = total)
      pb$tick(0)
    }
    
    for (i in Tracks) {
      ## in case there is NA in replace it by the opposite of splitCond
      filter[[i]][is.na(filter[[i]])] <- !splitCond
      ## split each tracklet according to the filter and the specified condition
      newTracks <-
        split(trackDat[[i]], cumsum(filter[[i]] == splitCond))
      ## remove the first row of each new tracklet when it correspond to an element which does not respect condition
      if (length(newTracks) > 1) {
        newTracksL1 <- newTracks[[1]]
        newTracks <-
          lapply(newTracks[2:max(length((newTracks)))], function(x)
            x[-1,])
        newTracks[["0"]] <- newTracksL1
        newTracks <- newTracks[sort(names(newTracks))]
      }
      ## in case there is empty new tracklets removed it
      newTracks <-
        newTracks[sapply(newTracks, function(x)
          nrow(x)) > 0]
      ## filter the new tracklets by minDur and append them to the output
      ### in case the filtering step does not return new tracklet
      if (length(newTracks) == 0) {
        nbNew_temp <- 0
        nbNew <- c(nbNew, nbNew_temp)
        TracksDurationNew_temp <- 0
        TracksDurationNew <-
          c(TracksDurationNew, TracksDurationNew_temp)
        allCorrTracks <- allCorrTracks
        
        w1 <-
          paste("For", i, sep = " ")
        w2 <-
          "\nno remaining tracklet after filtering, no tracklet returned"
        warning(w1, ": ", w2)
        
      }
      ### in case the filtering step does return new tracklets
      else {
        names(newTracks) <- seq(length(newTracks))
        nbNew_temp <- length(newTracks)
        nbNew <- c(nbNew, nbNew_temp)
        newSizes <- lapply(newTracks, function(x)
          nrow(x))
        TracksDurationNew_temp <- sum(unlist(newSizes))
        TracksDurationNew <-
          c(TracksDurationNew, TracksDurationNew_temp)
        
        ## keep only new tracklets with a sufficient duration
        goodTracks <- which(newSizes >= minDur)
        
        #### in case the filtering by minDur does not return new tracklet
        if (length(goodTracks) == 0) {
          nbGood_temp <- 0
          nbGood <- c(nbGood, nbGood_temp)
          allCorrTracks <- allCorrTracks
          
          w1 <-
            paste("For", i, sep = " ")
          w2 <-
            "\nAll the new tracklets created after filtering are shorter than minDur, no tracklet returned"
          warning(w1, ": ", w2)
          
        }
        #### in case the filtering by minDur does return new tracklets
        else {
          nbGood_temp <- length(goodTracks)
          nbGood <- c(nbGood, nbGood_temp)
          TracksDurationGood_temp2 <- vector()
          for (j in goodTracks) {
            allCorrTracks <- c(allCorrTracks, list(newTracks[[j]]))
            TracksDurationGood_temp <- c(dim(newTracks[[j]])[1])
            TracksDurationGood_temp2 <-
              c(TracksDurationGood_temp2,
                TracksDurationGood_temp)
          }
          TracksDurationGood <-
            c(TracksDurationGood, TracksDurationGood_temp2)
        }
      }
      
      if (isTRUE(progess)) {
        # progress bar
        pb$tick(1)
      }
    }
    
    # rename new tracklets
    if (length(allCorrTracks) == 0) {
      allCorrTracks <- NULL
    } else{
      names(allCorrTracks) <-
        paste("Tracklet", (seq(length(
          allCorrTracks
        ))), sep = "_")
    }
  }
  
  return(c(
    list(
      SummaryFiltering = list(
        Tracknb_before_filter = nbTrack,
        Tracknb_after_filter = sum(nbNew),
        Tracknb_after_minDur = sum(nbGood),
        TotTrackDuration_before_filter = totTracksDuration,
        TotTrackDuration_after_filter = sum(TracksDurationNew),
        TotTrackDuration_after_minDur = sum(TracksDurationGood),
        "%Data_kept_after_filter" =  sum(TracksDurationNew) / totTracksDuration * 100,
        "%Data_kept_after_minDur" =  sum(TracksDurationGood) / totTracksDuration * 100
      ),
      CleanedTracklets = allCorrTracks
    )
  ))
}