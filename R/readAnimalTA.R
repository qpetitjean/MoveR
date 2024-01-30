#' @title Import AnimalTA tracking output .csv file.
#'
#' @description Given the path of the .csv file corresponding to AnimalTA output,
#' this function returns an object of class "tracklets", a list of tracklets (data frame) containing 7 elements classically used for further computations using MoveR package:
#' \itemize{
#'    \item{'maj.ax': }{the length of the major axis (i.e., the midline) for a particle over frame (i.e., length of the ellipse).}
#'    \item{'angle': }{the particle's absolute angle in radians, orientation of the particle according to y-axis.}
#'    \item{'min.ax': }{the length of the minor axis for a particle over frame (i.e., width of the ellipse).}
#'    \item{'x.pos': }{x position of the particle's centroid.}
#'    \item{'y.pos': }{y position of the particle's centroid.}
#'    \item{'identity': }{the particle's identity given by the tracking software.}
#'    \item{'frame': }{the video frame number at which the measurements has been made.}
#' }
#'
#' The function can also append all the others elements returned by the tracking software (see [rawDat] argument)
#' Also, the function can flip y coordinates (see [flipY] argument).
#'
#' @param animalTAPath The full path of the TrackR output file (.csv).
#'
#' @param flipY A logical value (i.e., TRUE or FALSE) indicating whether the origin of y coordinates should be flipped. If TRUE, y coordinates are flipped to start on the top-left (default = FALSE).
#'
#' @param imgHeight A numeric value expressed in pixels, the length of Y axis
#' corresponding to the height of the image or video resolution (optional, only used when flipY = TRUE).
#'
#' @param rawDat A logical value (i.e., TRUE or FALSE) indicating whether all other elements retrieved from the tracking output should appended to the tracklets data (see \href{http://vchiara.eu/index.php/animalta}{AnimalTA}).
#' Note that this may drastically increase the size of the object returned by the function (default = FALSE).
#'
#' @return An object of class "tracklets" containing a list of tracklets and their characteristics classically used for further computations.
#' In case [rawDat] argument is TRUE, it also append all the others elements returned by the tracking software (for animalTA it corresponds to the arena id). 
#' Also, by default the function returns y coordinates starting on the bottom-left.
#'
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link{readCtrax}}, \code{\link{readTrackR}}, \code{\link{readTrex}}, \code{\link{readIdtracker}}, \code{\link{flipYCoords}}
#'
#' @references 
#' \href{http://vchiara.eu/index.php/animalta}{AnimalTA}
#' \href{https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14115}{Chiara, V., & Kim, S.-Y. (2023). AnimalTA: A highly flexible and easy-to-use program for tracking and analysing animal movement in different environments. Methods in Ecology and Evolution, 14, 1699– 1707. https://doi.org/10.1111/2041-210X.14115.}
#'
#' @examples
#' \dontrun{
#' 
#' # Download the first dataset from the sample data repository
#' Path2Data <- MoveR::DLsampleData(dataSet = 1, tracker = "AnimalTA")
#' Path2Data
#' 
#' # Import the data as an object of class "tracklets"
#' # also do not flip Y coordinates (start on the bottom-left)
#' Data <-
#'   MoveR::readAnimalTA(Path2Data[[1]])
#'   
#' str(Data)
#'
#' } 
#' @export

readAnimalTA <- function(animalTAPath,
                       flipY = FALSE,
                       imgHeight = NULL,
                       rawDat = FALSE) {
  
  error <- .errorCheck(imgHeight = imgHeight)
  if (flipY == TRUE & !is.null(error)) {
    stop(error)
  }
  
  # Import output file
  if (inherits(try(read.delim(animalTAPath, sep = ";"), silent = TRUE)
               , "try-error")) {
    stop("No such file or directory : undefined or wrong path supplied")
    
  } else {
    trackDat <- read.delim(animalTAPath, sep = ";")
  }
  
  FT <- which(names(trackDat) %in% c("Frame", "Time"))
  Data <- lapply(c("Y_", "X_"), function(x) {
    temp <- cbind(trackDat[,FT], trackDat[grep(x, names(trackDat))])
    Res <- cbind(temp[FT], utils::stack(lapply(temp[-FT], as.numeric)))
    names(Res)[which(names(Res) == "values")] <- gsub("_", "", x)
    splittedId <- do.call(rbind, strsplit(as.character(Res$ind), "_"))[,2:3]
    Id <- stats::setNames(data.frame(apply(splittedId, 2, function(x) gsub("[^0-9]", "", as.character(x)))), c("Arena", "Ind"))
    Res <- cbind(Res, Id)
    return(Res)
  })
  trackDatLong <- do.call(cbind, Data)
  trackDatLong <- trackDatLong[!duplicated(names(trackDatLong))]
  # if flipY = TRUE, flip the Y coordinates according to image height
  if (flipY == TRUE) {
    trackDatLong[["Y"]] <- MoveR::flipYCoords(trackDatLong[["Y"]], imgHeight = imgHeight)
  }
  
  if (rawDat == FALSE) {
    AnimTA_all <- list(
      maj.ax =  rep(NA, nrow(trackDatLong)),
      angle =  rep(NA, nrow(trackDatLong)),
      min.ax = rep(NA, nrow(trackDatLong)),
      x.pos = trackDatLong[["X"]],
      y.pos = trackDatLong[["Y"]],
      identity = trackDatLong[["Ind"]],
      frame = trackDatLong[["Frame"]]
    )
  } else if (rawDat == TRUE) {
      AnimTA_all <- list(
        maj.ax =  rep(NA, nrow(trackDatLong)),
        angle =  rep(NA, nrow(trackDatLong)),
        min.ax = rep(NA, nrow(trackDatLong)),
        x.pos = trackDatLong[["X"]],
        y.pos = trackDatLong[["Y"]],
        identity = trackDatLong[["Ind"]],
        frame = trackDatLong[["Frame"]],
        arena = trackDatLong[["Arena"]]
      )
  }
  
  # retrieve some information about tracking
  frameR <- max(AnimTA_all[["frame"]], na.rm=T) / max(trackDatLong[["Time"]], na.rm=T)
  
  # create a tracklets class object to return
  AnimTA_all <- MoveR::convert2Tracklets(AnimTA_all)
  
  # fill some attributes
  AnimTA_all <- MoveR::setInfo(AnimTA_all, frameR = frameR)
  
  return(AnimTA_all)
}
