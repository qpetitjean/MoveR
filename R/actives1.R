#' @title Determine active or inactive states according to the speed of a particles along its trajectory.
#'
#' @description Given a data frames containing tracking informations for a given fragment including speed, 
#' this function return a vector containing TRUE or FALSE when individual is active or not respectively.
#'
#' @param df A data frame containing x and y coordinates in columns named "x.pos", "y.pos" for a given fragment, as well as 
#' a column containing the speed of the particle, whatever the unit, over the fragment.
#' 
#' @param speedCol A character string corresponding to the name of the column containing the speed values of the particles along the fragment, whatever the unit.
#' 
#' @param minSpeed A numeric value expressed in the same unit than speed, corresponding to the threshold above which individual 
#' is considered as active.
#'
#' @return This function returns a vector containing TRUE or FALSE when individual is active or not respectively.
#'
#' @author Quentin PETITJEAN
#'
#' @examples
#'
#'# generate a dummy fragment
#'## start to specify some parameters to generate the fragment
#'FragL <- 100 # the length of the fragment or a sequence to randomly sample fragment length
#'
#'fragDatTemp <- trajr::TrajGenerate(sample(FragL, 1), random = TRUE, fps = 1)
#'fragDat <- data.frame(
#'  x.pos = fragDatTemp[["x"]] - min(fragDatTemp[["x"]]),
#'  y.pos = fragDatTemp[["y"]] - min(fragDatTemp[["y"]] ),
#'  frame = fragDatTemp[["time"]]
#')
#'
#'# compute the speed of the particle along its trajectory, here we consider that the space unit is the pixels, 
#'# expressing the speed as pixels/frame
#'fragDat[["speed"]] <- speed(fragDat, scale = 1, TimeCol = "frame", unit = "pixels/frame")
#'
#'# we can then define the speed treshold above which the particle is considered actives using quantiles
#'# here we use the 0.025 quantile to find the minimum speed treshold
#'hist(log10(fragDat[["speed"]][-is.na(fragDat[["speed"]])]))
#'tresh <- quantile(log10(fragDat[["speed"]][-is.na(fragDat[["speed"]])]), 0.025)
#"fragDat[["active_1"]] <- actives_1(fragDat, speedCol = "speed", minSpeed = 10^tresh)
#'
#'actives1(fragDat, speedCol = "speed", minSpeed = tresh[[1]])
#'
#'# draw the particle' trajectory and spot the inactive moments using red dots
#'drawFrags(
#'list(fragDat),
#'imgRes = c(max(fragDat[["x.pos"]]), max(fragDat[["y.pos"]])),
#'add2It = list(
#'  points(fragDat[["x.pos"]][which(fragDat[["active_1"]] == FALSE)],
#'         fragDat[["y.pos"]][which(fragDat[["active_1"]] == FALSE)],
#'         col = "red", 
#'         pch = 19,
#'         cex = 1.5))
#')
#'
#' @export

actives1 <- function (df, speedCol = NULL, minSpeed = NULL) {
  if(is.null(MoveR::listGet(df, "x.pos"))){
    stop(
      "x.pos column is missing or might be misspelled: x coordinates are needed to compute euclidian distance"
    )
  }
  if(is.null(MoveR::listGet(df, "y.pos"))){
    stop(
      "x.pos column is missing or might be misspelled: x coordinates are needed to compute euclidian distance"
    )
  }
  if (is.null(speedCol)) {
    stop(
      "speedCol argument is missing: the name of the column carying speed of the particle along its trajectory is needed to determine activity state"
    )}
  if(is.null(MoveR::listGet(df, speedCol))){
    stop(
      "speedCol argument is misspelled or is absent from the input df: the name of the column carying speed of the particle along its trajectory is needed to determine activity state"
    )}
  if (is.null(minSpeed)) {
    stop(
      "minSpeed argument is missing: impossible to determine whether individual is active or not without a minimum speed treshold"
    )
  } else if (!is.null(minSpeed)) {
    activeRes <- rep(NA, nrow(df))
    activeRes[which(df[[speedCol]] > minSpeed)] <- "active"
    activeRes[which(df[[speedCol]] < minSpeed)] <- "inactive"
  }
  return(activeRes)
}
