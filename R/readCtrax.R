#' @title Import Ctrax tracking output .mat file.
#'
#' @description Given the path of the .mat file corresponding to Ctrax output,
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
#' Also, the function can flip y coordinates (see [flipY] argument).
#'
#' @param ctraxPath The full path of the Ctrax output file (.mat).
#'
#' @param flipY A logical value (i.e., TRUE or FALSE) indicating whether the origin of y coordinates should be flipped. If TRUE, y coordinates are flipped to start on the top-left (default = FALSE).
#'
#' @param imgHeight A numeric value expressed in pixels, the length of Y axis
#' corresponding to the height of the image or video resolution (optional, only used when flipY = TRUE).
#'
#'
#' @return An object of class "tracklets" containing a list of tracklets and their characteristics classically used for further computations.
#' Also, by default the function returns y coordinates starting on the bottom-left.
#'
#'
#' @author Quentin PETITJEAN, Vincent CALCAGNO
#'
#' @seealso \code{\link{readAnimalTA}} \code{\link{readTrackR}}, \code{\link{readTrex}}, \code{\link{readIdtracker}}, \code{\link{flipYCoords}}
#'
#' @references
#' Branson, K., Robie, A., Bender, J. et al. High-throughput ethomics in large groups of Drosophila. Nat Methods 6, 451–457 (2009). https://doi.org/10.1038/nmeth.1328.
#' \href{https://ctrax.sourceforge.net/}{Ctrax: The Caltech Multiple Walking Fly Tracker}
#'
#' @examples
#' \dontrun{
#'
#' # Download the first dataset from the sample data repository
#' Path2Data <- MoveR::DLsampleData(dataSet = 1, tracker = "Ctrax")
#' Path2Data
#'
#' # Import the data as an object of class "tracklets"
#' # and do not flip Y coordinates (start on the bottom-left)
#' Data <- MoveR::readCtrax(Path2Data[[1]])
#' str(Data)
#'
#' }
#' @export

readCtrax <-
  function(ctraxPath,
           flipY = FALSE,
           imgHeight = NULL) {
    
    error <- .errorCheck(imgHeight = imgHeight)
    if (flipY == TRUE & !is.null(error)) {
      stop(error)
    }
    
    # Import output files from Ctrax
    if (inherits(try(R.matlab::readMat(ctraxPath), silent = TRUE)
                 , "try-error")) {
      stop("No such file or directory : undefined or wrong path supplied")
      
    } else {
      CtraxRaw <- R.matlab::readMat(ctraxPath)
    }
    
    # generate a vector containing the corresponding frame number for each row of the dataset
    frame <- c()
    frameOut <- 0
    for (t in CtraxRaw[["ntargets"]]) {
      frame <- c(frame, rep(frameOut, times = t))
      frameOut <-  frameOut + 1
    }

    # append the frame vector to the list of df
    CtraxRaw[["frame"]] <- frame
    CtraxRaw <-
      CtraxRaw[c(
        "maj.ax",
        "angle",
        "min.ax",
        "x.pos",
        "y.pos",
        "identity",
        "frame",
        "ntargets",
        "timestamps"
      )]
    
    # if flipY = TRUE, flip the Y coordinates according to image height
    if (flipY == TRUE) {
      CtraxRaw$y.pos <-
        flipYCoords(CtraxRaw$y.pos, imgHeight = imgHeight)
    }
    
    # retrieve some information about tracking
    frameR <- max(CtraxRaw[["frame"]], na.rm=T) / max(CtraxRaw[["timestamps"]], na.rm=T)
    
    # create a tracklets class object to return
    CtraxRaw <- MoveR::convert2Tracklets(CtraxRaw[-c(8,9)])
    
    # fill some attributes
    CtraxRaw <- MoveR::setInfo(CtraxRaw, frameR = frameR)

    return(CtraxRaw)
  }
