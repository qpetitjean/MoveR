#' @title Set information in a tracklets object.
#'
#' @description Given an object of class "tracklets", this function append some information (attribute) about tracking processing to the tracklets object.
#' This can be especially useful for further use in other function of the MoveR package. Indeed the functions will automatically try to retrieve these informations without specifying it as argument.
#'
#' @param trackDat An object of class "tracklets" containing a list of tracklets and their characteristics classically used for further computations (at least x.pos, y.pos, frame).
#'
#' @param frameR A numeric value expressed in frames per second, the frequency at which frames are recorded/displayed in the video.
#'
#' @param scale A ratio corresponding to the scaling factor to be applied to the trajectory coordinates (e.g., size in cm / size in pixels, default = 1).
#'
#' @param imgRes A vector of 2 numeric values, the resolution of the video used as x and y limit of the plot (i.e., the number of pixels in image width and height).
#' 
#'
#' @return An object of class "tracklets" with the selected information (attribute) appended as attributes. 
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link{trackletsClass}} \code{\link{getinfo}}
#'
#'
#' @examples
#' \dontrun{
#'
#' # Download the first dataset from the sample data repository
#' Path2Data <- MoveR::DLsampleData(dataSet = 1, tracker = "TRex")
#' Path2Data
#'
#' # Import the data as an object of class "tracklets"
#' # and flip Y coordinates to start on the bottom-left
#' Data <- MoveR::readTrex(Path2Data[[1]],
#'                flipY = T,
#'                imgHeight = 2160,
#'                rawDat = F
#'         )
#'         
#' str(Data)
#' 
#' # get the tracking informations from the tracklets object (frameR corresponding to the frame rate is already returned because it is included in TRex output)
#' getInfo(Data)
#' 
#' # set the image resolution in the tracklets object
#' Data <- setInfo(Data, imgRes = c(3840, 2160))
#' 
#' # check the result (now the tracklets object contains both information about the frame rate and the image resolution, which can be used in further functions of the MoveR package automatically - without specifying it in function's argument)
#' getInfo(Data)
#'
#' }
#' @export


setInfo <- function(trackDat, frameR = NULL, scale = NULL, imgRes = c(NULL, NULL)) {
  if (!inherits(trackDat, "tracklets") & !inherits(trackDat, "varList")) {
    stop("Invalid 'tracklets' or 'varList' object, please import your data with the 'read' functions included in the MoveR package or use trackletsClass function to turn your data into 'tracklets' object")
  }
  if (!is.null(frameR) || is.null(attr(trackDat, "frameR"))) {
    attr(trackDat, "frameR") <- frameR
  }
  if (!is.null(scale) || is.null(attr(trackDat, "scale"))) {
    attr(trackDat, "scale") <- scale
  }
  if (!is.null(imgRes) || is.null(attr(trackDat, "imgRes"))) {
    attr(trackDat, "imgRes") <- imgRes
  }
  return(trackDat)
}