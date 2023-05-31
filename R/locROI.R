#' @title Locate ROI
#'
#' @description Given the path of a table file (e.g., .txt or .csv, see \code{\link[utils]{read.table}}) or an already imported dataframe or matrix containing the location of
#' one or several ROI, this function returns the coordinates of the ROI edges according to the condition specified by the `edgeCrit` argument.
#'
#'
#' @param ROImat The full path of matrix (e.g., a distance matrix to the ROI edge) containing the location (x and y coordinates as row and columns respectively) of the ROI (e.g., .txt or .csv, see \code{\link[utils]{read.table}}) or an already imported dataframe or matrix.
#'
#' @param edgeCrit A vector containing at least 1 numeric value or character string indicating the criterion used to retrieve ROI edges.
#'
#' @param xy A numeric value, either 1 or 2 indicating whether the matrix have x values on columns and y on rows (1) or x values on rows and y on columns (2) (default = 1).
#' 
#' @param sep The field separator character. Values on each line of the file are separated by this character (default = tabs - only useful if ROImat is a path).
#'
#' @param dec The character used in the file for decimal points (default = "." - only useful if ROImat is a path).
#'
#' @param order A logical value (i.e., TRUE or FALSE) indicating whether the coordinates of the ROI edges (i.e., vertices) should be reordered according to the centroid of the ROI and angle of each vertex (see \code{\link[MoveR]{sortVertices}}).
#'
#' @return This function returns a list containing the coordinates of the ROI edges. Each element of the list correspond to a dataframe containing the coordinates of the edges of one ROI.
#' In case there is only one ROI to retrieve (i.e., edgeCrit argument is a vector of length 1), the function returns a dataframe containing the coordinates of the ROI edges.
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link[MoveR]{locPos}}, \code{\link[MoveR]{circles}}, \code{\link[MoveR]{polygons}}, \code{\link[MoveR]{sortVertices}}
#' 
#' @examples
#' \dontrun{
#'
#' # Download the first dataset from the sample data repository
#' Path2Data <- MoveR::DLsampleData(dataSet = 1, tracker = "TRex")
#' Path2Data
#'
#' # Example 1:
#' ## For this exemple, we are importing the distance matrix from the edge of the arena (a distance matrix generated using color tresholding in imageJ)
#' ## here, the edge of the ROI (i.e., the arena) correspond to the lower value of the distrance matrix plus 1 pixel (i.e., 1)
#' ArenaEdge <- MoveR::locROI(Path2Data[[2]], edgeCrit = 1)
#' ## plot the arena edges
#' plot(ArenaEdge)
#'
#' # Example 2:
#' ## From the same distance matrix we can also retrieve the center of the arena which correspond to the maximum value of the distance matrix minus 1 pixel (i.e., 254)
#' ROIEdges <- MoveR::locROI(Path2Data[[2]], edgeCrit = c(1, 254))
#' ## plot the arena edges and the edge of the center
#' plot(NULL,
#'      xlim = c(0, max(do.call("rbind",ROIEdges)[1])),
#'      ylim = c(0, max(do.call("rbind",ROIEdges)[2])))
#'
#' lapply(ROIEdges, function(x)
#'   points(x))
#'
#' }
#' @export

locROI <-
  function(ROImat = NULL,
           edgeCrit = NULL,
           xy = 1,
           sep = "\t",
           dec = ".",
           order = FALSE) {
    # Import the matrix containing ROI location
    if (!is.data.frame(ROImat) &
        !is.matrix(ROImat) & is.character(ROImat)) {
      if (inherits(try(read.delim(ROImat, sep = sep), silent = TRUE)
                   , "try-error")) {
        stop("No such file or directory : undefined or wrong path supplied")
        
      } else {
        ROImat <- read.delim(ROImat, sep = sep, dec = dec)
      }
    } else {
      ROImat <- as.data.frame(ROImat)
    }
    if(xy == 1){
      colNames <- c("y.pos", "x.pos")
    }else if(xy == 2){
      colNames <- c("x.pos", "y.pos")
    }else{
      stop("[xy] argument should be either 1 or 2 depending on the position of x and y coords in the matrix, see ?locROI")
    }
    # retrieve the edge of the ROI and store them in a list
    ROIedge <-
      stats::setNames(lapply(edgeCrit, function(x)
        if (isTRUE(order)) {
          stats::setNames(MoveR::sortVertices(data.frame(which(
            ROImat == x, arr.ind = T
          ))),
          colNames)
        } else{
          stats::setNames(data.frame(which(ROImat == x, arr.ind = T)),
                          colNames)
        }),
        paste("ROI", seq(length(edgeCrit)), sep = "_"))
    if (length(ROIedge) == 1) {
      ROIedge <- ROIedge[[1]]
    }
    return(ROIedge)
  }
