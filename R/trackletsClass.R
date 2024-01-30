#' @title Create an object of class \code{tracklets}.
#'
#' @description create an object of class \code{tracklets} from a list of tracklets.
#' 
#' @param tracklets A list of data frame containing tracklets information. The presence of x, y coordinates and time column named as x.pos, y.pos and frame is necessary for each tracklets.
#' 
#' 
#' @return An object of class \code{tracklets}.
#'
#'
#' @author Quentin PETITJEAN
#'
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
#'  test <- trackletsClass(TrackList)
#'  
#' @export

trackletsClass <- function(tracklets) {
  Obj <- tracklets
  
  # check whether each tracklet have the same number of columns (quantity of information)
  nCol <- sapply(Obj, ncol)
  ## Find the most common number of columns
  commonColsNumb <- as.integer(names(sort(table(nCol), decreasing = TRUE)[1]))
  ## Find indices of data frames that don't match the most common number of columns
  diffInd <- which(nCol != commonColsNumb)
  if (length(diffInd) > 0) {
    diffDetails <- sapply(diffInd, function(idx) {
      paste0("#", idx,
             " (", nCol[idx], " columns)",
             collapse = ", ")
    })
    stop(
      paste0(
        "tracklet(s) ",
        paste0(diffDetails, collapse = ", "),
        " have a different number of columns than other tracklets.\n",
        "Most common number of columns across tracklets is [",
        commonColsNumb,
        "]."
      )
    )
  }
  
  # check whether each tracklet have identical columns names (similar information)
  ## Extract column names from each data frame and create a unique identifier for each pattern
  colNamesPatterns <- sapply(Obj, function(df) paste(sort(colnames(df)), collapse = "_"))
  ## Find the most common pattern
  commonPattern <- names(sort(table(colNamesPatterns), decreasing = TRUE))[1]
  commonInd<- which(colNamesPatterns == commonPattern)[1]
  commoncolNames <- colnames(Obj[[commonInd]]) 
  ## Find indices and column names of data frames not matching the most common pattern
  diffInd <- which(colNamesPatterns != commonPattern)

  if (length(diffInd) > 0) {
    diffDetails <- sapply(diffInd, function(idx) {
      paste0("#", idx)
    })
    
    stop(error_message <- paste0(
      "tracklet(s) ",
      paste0(diffDetails, collapse = ", "),
      " have different column names than other tracklets.\n",
      "Most common columns names across tracklets are [",
      paste0(commoncolNames, collapse = ", "),
      "]."
    ))
  }

  # check that x.pos, y.pos and frame are included in each tracklet else return an error
  requiredCol <- c("x.pos", "y.pos", "frame")
  ## Use sapply to check for required columns in each data frame
  OkTracklets <- sapply(Obj, function(df) {
    all(requiredCol %in% colnames(df))
  })
  ## Identify tracklets that do not have the required columns
  missingColInd <- which(!OkTracklets)
  
  if (length(missingColInd) > 0) {
    stop(paste0(
      "The following tracklets are missing the required columns ", 
      paste0(requiredCol, collapse = ", "),
      ": ",
      paste(missingColInd, collapse = ", ")
    ))
  }
  
  class(Obj) <- "tracklets"
  error <- .errorCheck(trackDat = Obj, x.pos = "x.pos", y.pos = "y.pos", timeCol = "frame")
  if(!is.null(error)){
    stop(error)
  }
  return(Obj)
}
 