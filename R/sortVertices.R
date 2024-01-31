#' @title Sort the vertices of an ROI in a clockwise order
#'
#' @description Given a dataframe or a matrix containing the x and y coordinates corresponding to the vertices or any points located on the edge of an ROI (i.e., polygon),
#' this function returns the vertices sorted in a clockwise order according to their angles.
#'
#' @param ROI A dataframe or a matrix containing x and y coordinates corresponding to the vertices (or any points located on the edge) of an ROI (i.e., polygon).
#'
#' @return This function returns a dataframe containing the sorted coordinates of the ROI's vertices in a clockwise order.
#'
#' @author Quentin PETITJEAN
#'
#' @keywords internal

.sortVertices <- function(ROI) {
  # Compute the centroid of the ROI
  centroid <- apply(ROI, 2, mean)
  # Compute the angle of each vertex according to the centroid
  angles <- atan2(ROI[[2]] - centroid[[2]], ROI[[1]] - centroid[[1]])
  # reorder the vertices based on their angles
  OrdROI <- ROI[order(angles), ]
  # Return the points of the ROI's edge ordered
  return(OrdROI)
}
