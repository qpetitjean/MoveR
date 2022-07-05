#' @title Fragment resampling
#' 
#' @description Given a list of data frame containing tracking informations for each fragment,
#' this function returns a list of resampled fragments according to the length of the resampling 
#' step specified by the user
#'
#'
#' @param trackDat A list of data frame containing tracking informations for each fragment
#' (e.g., maj.ax, angle, min.ax, x.pos, y.pos, ...)
#'
#' @param Tstep A numeric value corresponding to the length of the resampling step (unit of time, e.g., frame)
#' 
#' @param TimeCol A character string corresponding to the name of the column containing Time information (e.g., "frame")
#' 
#' @return A list of data frame containing resampled tracking informations for each fragment 
#'
#' @authors Quentin Petitjean
#'
#'
#' @examples
#' 
#' #TODO
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
# identify the first time unit where the fragment is detected
start <- trackDat[[i]][trackDat[[i]][[TimeCol]] == sort(trackDat[[i]][[TimeCol]]),][1,]
# use it as the starting point of the increment for resempling
increment <- seq(from = start[[TimeCol]], to = max(trackDat[[i]][[TimeCol]], na.rm = T), by = Tstep)
selFrames <- increment %in% trackDat[[i]][[TimeCol]]
# store the time values to keep and detect which have to be added
toKeep_temp <- increment[selFrames]
# retrieve data of the fragment and add a row containing NA when a time value is missing
toKeep <- trackDat[[i]][trackDat[[i]][[TimeCol]] %in% toKeep_temp, ]
if (FALSE %in% selFrames) {
  toAdd <-
    setNames(data.frame(matrix(
      ncol = ncol(trackDat[[i]]), nrow = length(list(increment[which(selFrames == FALSE)]))
    )), c(names(trackDat[[i]])))
  toAdd[TimeCol] <- c(unlist(list(increment[which(selFrames == FALSE)])))
  toKeep <- rbind(toKeep, toAdd)[order(rbind(toKeep, toAdd)[[TimeCol]]),]
  if ("frags_id" %in% names(trackDat[[i]]) == TRUE) {
    w1 <- paste("In fragment", unique(trackDat[[i]]$frags_id), sep = " ")
    w2 <-
      paste("\n some frames are missing",
            list(increment[which(selFrames == FALSE)]) ,
            sep = " ")
    w3 <-
      ",\n resampling have added lines containing NA, perhaps consider modifying Tstep"
    warning(w1, ": ", w2, w3)
  } else {
    w1 <- paste("In fragment", unique(trackDat[[i]]$identity), sep = " ")
    w2 <-
      paste("\n some frames are missing",
            list(increment[which(selFrames == FALSE)]) ,
            sep = " ")
    w3 <-
      ",\n resampling have added lines containing NA, perhaps consider modifying Tstep"
    warning(w1, ": ", w2, w3)
    
  }
}
    trackDat[[i]] <- toKeep
    # progress bar
    pb$tick(1)
    Sys.sleep(1 / 1000)
}
if(length(trackDat) == 1){
  trackDat <- trackDat[[1]]
}
return(trackDat)
}
