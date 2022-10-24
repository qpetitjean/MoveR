#' @title Resampling fragments.
#' 
#' @description Given a list of data frame containing tracking informations for each fragment,
#' this function returns a list of resampled fragments according to the length of the resampling 
#' step specified by the user.
#'
#'
#' @param trackDatA A list of data frame containing tracking informations for each fragment.
#'
#' @param Tstep A numeric value corresponding to the length of the resampling step (time unit, e.g., frame).
#' 
#' @param TimeCol A character string corresponding to the name of the column containing time information (e.g., "frame").
#' 
#' @return A list of data frame containing the resampled fragments. 
#'
#' @author Quentin PETITJEAN
#'
#'
#' @examples
#' 
#'# generate some dummy fragments
#'## start to specify some parameters to generate fragments
#'Fragn <- 50 # the number of fragment to simulate
#'FragL <- 100:1000 # the length of the fragments or a sequence to randomly sample fragment length
#'
#'fragsList <- stats::setNames(lapply(lapply(seq(Fragn), function(i)
#'  trajr::TrajGenerate(sample(FragL, 1), random = TRUE, fps = 1)), function(j)
#'    data.frame(
#'      x.pos = j$x - min(j$x),
#'      y.pos = j$y - min(j$y),
#'      frame = j$time
#'    )), seq(Fragn))
#'
#'# check the fragments
#'drawFrags(fragsList,
#'          imgRes = c(max(convert2list(fragsList)[["x.pos"]]),
#'                     max(convert2list(fragsList)[["y.pos"]])),
#'          timeCol = "frame")
#'
#'# Run convert the time unit (frame) to seconds using analyseFrags (according to a frame rate of 25 fps)
#'fragsListV1 <-
#'  analyseFrags(
#'    fragsList,
#'    customFunc = list(
#'      # convert the time expressed in frame in second using a conversion factor of 25 frame per second
#'      TimeSec = function(x)
#'        x[["frame"]] / 25
#'    )
#'  )
#'# exemple 1: resample the fragment every 0.2 seconds
#'## check the size of the fragments 
#'fragSize <- unlist(lapply(fragsListV1, function(x) nrow(x)))
#'
#'## resample the fragment every 1 seconds
#'fragListSampled1S <- resampleFrags(fragsListV1, 
#'                                   TimeCol = "TimeSec", 
#'                                   Tstep = 1)
#'
#'## check the size of the fragments after resampling 
#'fragSize1s <- unlist(lapply(fragListSampled1S, function(x) nrow(x)))
#'
#'## Compare the fragments size
#'cbind(fragSize, fragSize1s)
#'
#'# exemple 2: resample the fragment every 1 minutes
#'
#'## resample the fragment every 0.10 seconds
#'fragListSampled0.10S <- resampleFrags(fragsListV1, 
#'                                      TimeCol = "TimeSec", 
#'                                      Tstep = 0.10)
#'## here some time step are not found in the fragment list (e.g., 0.10, 0.30), 
#'## they are hence replaced by NA in the output list
#'
#'## check the size of the fragments after resampling 
#'fragSize0.10S <- unlist(lapply(fragListSampled0.10S, function(x) nrow(x)))
#'
#'## Compare the fragments size
#'cbind(fragSize, fragSize1s, fragSize0.10S)
#' 
#' @export

resampleFrags <- function(trackDat, Tstep = NULL, TimeCol = NULL) {

  if (is.null(Tstep)) {
    stop(
      "Tstep argument is missing: a value corresponding to the resampling step is needed to resample the data"
    )}
  if (is.null(TimeCol)) {
    stop(
      "TimeCol argument is missing: the name of the column carying time information is needed to resample the data"
    )}
  if(is.data.frame(trackDat)){
    trackDat <- list(trackDat)
  }
# initialize progress bar
total = length(trackDat)
pb <-
  progress::progress_bar$new(format = "Resampling fragment [:bar] :current/:total (:percent)", total = total)
pb$tick(0)
Sys.sleep(0.001)

for(i in seq(length(trackDat))){ 
# resampling fragment according to Tstep
## identify the first time unit where the fragment is detected
start <- trackDat[[i]][trackDat[[i]][[TimeCol]] == sort(trackDat[[i]][[TimeCol]]),][1,]
## use it as the starting point of the increment for resempling
increment <- seq(from = start[[TimeCol]], to = max(trackDat[[i]][[TimeCol]], na.rm = T), by = Tstep)
selVal <- as.character(increment) %in% as.character(signif(trackDat[[i]][[TimeCol]],
                                                           digits = max(nchar(increment),
                                                                        na.rm = T))) 
## store the time values to keep and detect which have to be added
toKeepTemp <- increment[selVal]
## retrieve data of the fragment and add a row containing NA when a time value is missing
toKeep <- trackDat[[i]][trackDat[[i]][[TimeCol]] %in% toKeepTemp, ]
if (FALSE %in% selVal) {
  toAdd <-
    setNames(data.frame(matrix(
      ncol = ncol(trackDat[[i]]), nrow = length(increment[which(selVal == FALSE)])
    )), c(names(trackDat[[i]])))
  toAdd[TimeCol] <- c(unlist(list(increment[which(selVal == FALSE)])))
  toKeep <- rbind(toKeep, toAdd)[order(rbind(toKeep, toAdd)[[TimeCol]]),]
  if ("fragsId" %in% names(trackDat[[i]]) == TRUE) {
    w1 <- paste("In fragment", unique(trackDat[[i]]$fragsId), sep = " ")
    w2 <-
      paste("\n some time units are missing",
            list(increment[which(selVal == FALSE)]) ,
            sep = " ")
    w3 <-
      ",\n resampling have added lines containing NA, perhaps consider modifying Tstep"
    warning(w1, ": ", w2, w3)
  } else {
    w1 <- paste("In fragment", unique(trackDat[[i]]$identity), sep = " ")
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
    ## progress bar
    pb$tick(1)
    Sys.sleep(1 / 1000)
}
if(length(trackDat) == 1){
  trackDat <- trackDat[[1]]
}
return(trackDat)
}
