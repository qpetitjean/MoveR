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
#' @param Tstep A numeric value corresponding to the length of the resampling step (number of frames)
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


resampleFrags <- function(trackDat, Tstep) {

# initialize progress bar
total = length(trackDat)
pb <-
  progress::progress_bar$new(format = "Resampling fragment [:bar] :current/:total (:percent)", total = total)
pb$tick(0)
Sys.sleep(0.001)

for(i in seq(length(trackDat))){ 
# resampling fragment according to Tstep
# identify the first frame where the fragment is detected
start <- trackDat[[i]][trackDat[[i]]$frame == sort(trackDat[[i]]$frame),][1,]
# use it as the starting point of the increment for resempling
increment <- seq(from = start$frame[1,1], to = max(trackDat[[i]]$frame, na.rm = T), by = Tstep)
selFrames <- increment %in% trackDat[[i]]$frame 
# store frame to keep and detect which have to be added
toKeep_temp <- increment[selFrames]
# retrieve data of the fragment and add a row containing NA when the frame is missing
toKeep <- trackDat[[i]][trackDat[[i]]$frame %in% toKeep_temp, ]
if (FALSE %in% selFrames) {
  toAdd <-
    setNames(data.frame(matrix(
      ncol = dim(trackDat[[i]])[[2]], nrow = length(list(increment[which(selFrames == FALSE)]))
    )), c(names(trackDat[[i]])))
  toAdd$frame <- c(unlist(list(increment[which(selFrames == FALSE)])))
  toKeep <- rbind(toKeep, toAdd)[order(rbind(toKeep, toAdd)$frame),]
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
return(trackDat)
}
