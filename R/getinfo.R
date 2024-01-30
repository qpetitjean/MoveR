#' @title Get information in a tracklets object.
#'
#' @description Given an object of class "tracklets", this function returns some information about tracking processing from the tracklets object.
#'
#' @param trackDat An object of class "tracklets" containing a list of tracklets and their characteristics classically used for further computations (at least x.pos, y.pos, frame).
#'
#' @param attrName  A character string indicating which attirbute to return. If this argument is not provided the function returns a list of all hidden attributes (default = NULL).
#'
#' @return The value of the selected attribute or a list of the attributes' values in the tracklets object. By default (attrName = NULL), the function returns a list of all hidden attributes (frameR, scale, imgRes).
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


getInfo <- function(trackDat, attrName = NULL) {
  if (!inherits(trackDat, "tracklets") & !inherits(trackDat, "varList")) {
    stop("Invalid 'tracklets' or 'varList' object, please import your data with the 'read' functions included in the MoveR package or use trackletsClass function to turn your data into 'tracklets' object")
  }
  if (!is.null(attrName)) {
    if (attrName %in% c("frameR", "scale", "imgRes")) {
      return(attr(trackDat, attrName))
    } else {
      stop(
        "Invalid attribute name, choose either 'frameR', 'scale' or 'imgRes' or leave the argument empty to return all values"
      )
    }
  } else {
    return(list(
      frameR = attr(trackDat, "frameR"),
      scale = attr(trackDat, "scale"),
      imgRes = attr(trackDat, "imgRes")
    ))
  }
}