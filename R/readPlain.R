#' @title Import plain tracking output from a table.
#'
#' @description Given the path of a table file (e.g., .txt or .csv, see \code{\link[utils]{read.table}}) or an already imported tibble, dataframe or list of vectors
#' containing x and y coordinates (named X and Y respectively), the identity of the particle(s) and time,
#' this function returns an object of class "tracklets", a list of tracklets (data frame) containing 7 elements classically used for further computations using MoveR package:
#' \itemize{
#'    \item{'x.pos': }{x position of the particle's centroid.}
#'    \item{'y.pos': }{y position of the particle's centroid.}
#'    \item{'identity': }{the particle's identity given by the tracking software.}
#'    \item{'frame': }{the video frame number at which the measurements has been made.}
#' }
#'
#' Also, the function can flip y coordinates (see [flipY] argument).
#'
#' @param plainTab The full path of a plain tracking output (e.g., .txt or .csv, see \code{\link[utils]{read.table}}) or an already imported tibble, dataframe or list of vectors.
#'
#' @param sep The field separator character. Values on each line of the file are separated by this character (default = ";" - only useful if plainTab is a path).
#'
#' @param dec The character used in the file for decimal points (default = "." - only useful if plainTab is a path).
#'
#' @param id A character string corresponding to the name of the column containing particle's identity (default = "identity").
#'
#' @param timeCol A character string corresponding to the name of the column containing Time information (default = "frame").
#'
#' @param flipY A logical value (i.e., TRUE or FALSE) indicating whether the origin of y coordinates should be flipped. If TRUE, y coordinates are flipped to start on the top-left (default = FALSE).
#'
#' @param imgHeight A numeric value expressed in pixels, the length of Y axis
#' corresponding to the height of the image or video resolution (optional, only used when flipY = TRUE).
#'
#'
#' @return An object of class "tracklets" containing a list of tracklets and their characteristics classically used for further computations.
#' In case the input data (plainTab) contains other columns, those columns are also appended to the returned tracklets object
#'
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link{readAnimalTA}} \code{\link{readCtrax}}, \code{\link{readTrackR}}, \code{\link{readTrex}}, \code{\link{readIdtracker}}, \code{\link{flipYCoords}}, \code{\link[utils]{read.table}}
#'
#' @examples
#' 
#' set.seed(2023)
#' # generate some dummy tracklets
#' ## start to specify some parameters to generate tracklets
#' TrackN <- 40 # the number of tracklet to simulate
#' TrackL <-
#'   1:1000 # the length of the tracklets or a sequence to randomly sample tracklet length
#' TrackList <- stats::setNames(lapply(lapply(seq(TrackN), function(i)
#'   trajr::TrajGenerate(sample(TrackL, 1), random = TRUE, fps = 1)), function(j) {
#'     data.frame(
#'       X = j$x - min(j$x),
#'       Y = j$y - min(j$y),
#'       frame = j$time
#'     )
#'   }), seq(TrackN))
#' ## convert it to a simple list of vector
#' plainTab <- MoveR::convert2List(TrackList)
#' 
#' # Import the data as an object of class "tracklets" (the function can also retrieve the plainTab from a table file by giving the full path to the file)
#' # also do not flip Y coordinates (start on the bottom-left)
#' Data <-
#'   MoveR::readPlain(plainTab,
#'                    id = "trackletId")
#' str(Data)
#'
#' @export

readPlain <- function(plainTab,
                      sep = ";",
                      dec = ".",
                      id = "identity",
                      timeCol = "frame",
                      flipY = FALSE,
                      imgHeight = NULL) {
  
  error <- .errorCheck(imgHeight = imgHeight)
  if (flipY == TRUE & !is.null(error)) {
    stop(error)
  }
  
  # Import output file
  if (!is.data.frame(plainTab) & is.character(plainTab)) {
    if (inherits(try(read.delim(plainTab, sep = sep), silent = TRUE)
                 , "try-error")) {
      stop("No such file or directory : undefined or wrong path supplied")
      
    } else {
      trackDat <- read.delim(plainTab, sep = sep, dec = dec)
    }
  } else {
    trackDat <- as.data.frame(plainTab)
  }
  
  pos <- c()
  for (i in c(id, timeCol, "X", "Y")) {
    posTemp <- which(i == names(trackDat))
    if (length(posTemp) == 0) {
      stop(
        "[",
        i,
        "]",
        " is not found in the input table, consider using id or timeCol arguments to specify it manually."
      )
    }
    if (length(posTemp) > 1) {
      stop(
        "Several instances of ",
        "[",
        i,
        "]",
        " have been found in the input table, consider renaming or removing duplicated elements from the input table."
      )
    }
    pos <- c(pos, posTemp)
  }
  
  # if flipY = TRUE, flip the Y coordinates according to image height
  if (flipY == TRUE) {
    trackDat[[pos[4]]] = flipYCoords(trackDat[[pos[4]]], imgHeight = imgHeight)
  }
  
  Data <- list(
    x.pos = trackDat[[pos[3]]],
    y.pos = trackDat[[pos[4]]],
    identity = trackDat[[id]],
    frame = trackDat[[timeCol]]
  )
  
  # in case the plain tracking output contains other columns than those used by MoveR append them to the output list
  if(ncol(trackDat) > length(pos)){
    datCol <- seq(ncol(trackDat))
    colAdd <- which(!datCol %in% pos)
    toadd <- trackDat[,colAdd]
    Data <- c(Data, as.list(toadd))
  }
  
  # create a tracklets class object to return
  Data <- MoveR::convert2Tracklets(Data)
  
  return(Data)
}
