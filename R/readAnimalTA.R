#' @title Import AnimalTA tracking output .csv file.
#'
#' @description Given the path of the .csv file corresponding to AnimalTA output,
#' this function returns a list of 9 vectors classically used for further computations using MoveR package:
#' \itemize{
#'    \item{'maj.ax': }{the length of the major axis (i.e., the midline) for a particle over frame (i.e., length of the ellipse).}
#'    \item{'angle': }{the particle's absolute angle in radians, orientation of the particle according to y-axis.}
#'    \item{'min.ax': }{the length of the minor axis for a particle over frame (i.e., width of the ellipse).}
#'    \item{'x.pos': }{x position of the particle's centroid.}
#'    \item{'y.pos': }{y position of the particle's centroid.}
#'    \item{'identity': }{the particle's identity given by the tracking software.}
#'    \item{'frame': }{the video frame number at which the measurements has been made.}
#'    \item{'ntargets': }{the number of particle tracked over each frame.}
#'    \item{'timestamps': }{the elapsed time over each frame, in seconds.}
#' }
#'
#' Alternatively, the function can returns a list containing 2 sublists, the first corresponding to the one mentioned above
#' and the second containing all the elements retrieved from the .csv file (see rawDat argument).
#' Also, the function can flip y coordinates (see flipY argument).
#'
#' @param trackRPath The full path of the TrackR output file (.csv).
#'
#' @param flipY A logical value (i.e., TRUE or FALSE) indicating whether the origin of y coordinates should be flipped. If TRUE, y coordinates are flipped to start on the top-left (default = FALSE).
#'
#' @param imgHeight A numeric value expressed in pixels, the length of Y axis
#' corresponding to the height of the image or video resolution (optional, only used when flipY = TRUE).
#'
#' @param rawDat TRUE or FALSE, if TRUE add a second list containing all the elements retrieved from .csv file (see \href{http://vchiara.eu/index.php/animalta}{AnimalTA}),
#' may drastically increase the size of the object returned by the function (default = FALSE).
#'
#' @return A list containing either a list of 9 elements classically used for further computations or a list containing 2 sublists, the first corresponding to the one previously mentioned
#' and the second containing all the elements retrieved from the .csv file (see rawDat argument). Also, by default the function returns y coordinates starting on the bottom-left.
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
#' # Import the list containing the 9 vectors classically used for further computation
#' # and flip Y coordinates to start on the top-left
#' Data <-
#'   MoveR::readAnimalTA(Path2Data[[1]],
#'          flipY = T,
#'          imgHeight = 2160,
#'          rawDat = F
#'   )
#' str(Data)
#'
#' # Import the list containing 2 sublists, the first containing the 9 vectors classically used for further computation
#' # and the second list containing all the elements retrieved from .csv file,
#' # also do not flip Y coordinates (start on the bottom-left)
#'
#' DataFull <-
#'   MoveR::readAnimalTA(Path2Data[[1]],
#'     rawDat = T
#'   )
#'
#' str(DataFull)
#'
#' } 
#' @export

readAnimalTA <- function(trackRPath,
                       flipY = FALSE,
                       imgHeight = NULL,
                       rawDat = FALSE) {
  if (flipY == TRUE & is.null(imgHeight)) {
    stop(
      "imgHeight argument is missing, the height of the image resolution is needed to flip y coordinates"
    )
  }
  # Import output file
  if (inherits(try(read.delim(trackRPath, sep = ";"), silent = TRUE)
               , "try-error")) {
    stop("No such file or directory : undefined or wrong path supplied")
    
  } else {
    trackDat <- read.delim(trackRPath, sep = ";")
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
  # create ntargets, the number of particles detected for each frame (not in Raw output of TrackR)
  ntargets <- unlist(lapply(unique(trackDatLong[["Frame"]]), function(x)
    length(trackDatLong[["Ind"]][which(trackDatLong[["Frame"]] == x)])))
  
  if (rawDat == FALSE) {
    AnimTA_all <- list(
      maj.ax =  rep(NA, nrow(trackDatLong)),
      angle =  rep(NA, nrow(trackDatLong)),
      min.ax = rep(NA, nrow(trackDatLong)),
      x.pos = trackDatLong[["X"]],
      y.pos = trackDatLong[["Y"]],
      identity = trackDatLong[["Ind"]],
      frame = trackDatLong[["Frame"]],
      ntargets = ntargets,
      timestamps = unique(trackDatLong[["Time"]])
    )
  } else if (rawDat == TRUE) {
    AnimTA_all <- list(
      AnimTA_all <- list(
        maj.ax =  rep(NA, nrow(trackDatLong)),
        angle =  rep(NA, nrow(trackDatLong)),
        min.ax = rep(NA, nrow(trackDatLong)),
        x.pos = trackDatLong[["X"]],
        y.pos = trackDatLong[["Y"]],
        identity = trackDatLong[["Ind"]],
        frame = trackDatLong[["Frame"]],
        ntargets = ntargets,
        timestamps = unique(trackDatLong[["Time"]])
      ),
      Data_AnimTA_Raw = as.list(trackDat)
    )
  }
  return(AnimTA_all)
}
