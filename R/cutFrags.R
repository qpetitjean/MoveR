#' @title Cutting fragments
#'
#' @description Given a list of data frames containing tracking information for each fragments the function
#' returns the fragments part(s) that meet the condition specified in the customFunc.
#'
#' @param trackDat A list of data frames containing tracking information for each fragment (i.e., x.pos, y.pos).
#'
#' @param customFunc A function used to cut/subset the fragments.
#'
#' @return A list of data frames containing subsetted tracking information for each fragments according to
#' the conditions specified by the customFunc argument.
#'
#'
#' @authors Quentin PETITJEAN
#'
#' @examples
#'
# load the sample data
#'
#'Data <-
#'  readTrex(
#'    system.file("sampleData/sample_1/TREXOutput", package = "MovR"),
#'    mirrorY = T,
#'    imgHeight = 2160,
#'    rawDat = F
#'  )
#'# convert it to a list of fragments
#'trackDat <- convert2frags(Data[1:7], by = "identity")
#'
#'## exemple 1 :
#'# cuts the fragments by keeping only the part of the their trajectory that are
#'# made during the first 25 seconds of the video
#'TimeSeq <- seq(0, 25 * 25, by = 1) # create a time sequence from 0 to 25 seconds (25 seconds * 25 frames)
#'trackDatSub <- cutFrags(trackDat,
#'                        customFunc = function(x)
#'                          x[["frame"]] %in% TimeSeq)
#'
#'par(mfrow = c(1,2))
#'# draw the fragments before having cut them
#'drawFrags(trackDat,
#'          imgRes = c(3840, 2160),
#'          legend = T,
#'          cex.leg = 0.8,
#'          main = "")
#'
#'# draw the fragments after cutting (keep only the first 25 seconds of the video)
#'drawFrags(trackDatSub,
#'          imgRes = c(3840, 2160),
#'          legend = T,
#'          cex.leg = 0.8,
#'          main = "")
#'
#'## exemple 2 :
#'# or cut the fragment to remove every part that are on the edge of the arena
#'
#'# load a reference dataset (A matrix or dataframe or path to a file (either .txt or .csv) containing a distance matrix to any object or
#'# the location of one or several areas of interest (here we have created a distance map using ImageJ)
#'refDat <-
#'  as.matrix(read.delim(
#'    system.file("sampleData/sample_1/ReferenceData/TREXOutput/RefDat_2602_ISA3080_Low_5.csv", package = "MovR"),
#'    dec = "."
#'  ))
#'##  retrieve the value of the edge limit (1) and of the center limit (254) to plot them
#'arenaEdge <- data.frame(which(refDat == 1, arr.ind=T))
#'arenaCenter <- data.frame(which(refDat == 254, arr.ind=T))
#'
#'## then specify that all values above 254 are considered as the center
#'refDat[which(as.numeric(refDat) > 254)] <- "center"
#'## rather all values above 0 and below or equal to 254 are considered as within the border
#'refDat[which(as.numeric(refDat) >= 0 & as.numeric(refDat) <= 254)] <- "edge"
#'
#'# retrieve the area where the particles
#'# are located over their whole trajectory using locaPos function
#'trackDat <- analyseFrags(trackDat,
#'                         customFunc = list(
#'                           Position = function(x)
#'                             locaPos(refDat, x)))
#'
#'# cut the fragment to remove every part that are on the edge of the arena
#'trackDatSub2 <- cutFrags(trackDat,
#'                         customFunc = function(x)
#'                           x[["Position"]] != "edge")
#'
#'
#'par(mfrow = c(1,2))
#'# draw the fragment before having cut them, fragments part that are on the edge of the arena colored in red
#'# and those at the center in black
#'drawFrags(
#'  trackDat,
#'  imgRes = c(3840, 2160),
#'  add2It = list(
#'    points(x = arenaEdge[, 2], y = arenaEdge[, 1], cex = 0.01),
#'    points(x = arenaCenter[, 2], y = arenaCenter[, 1], cex = 0.01)
#'  ),
#'  colId = "Position",
#'  main = ""
#')
#'# draw the fragment after having cut them (remove part of the fragment that are in the edge)
#'drawFrags(
#'  trackDatSub2,
#'  imgRes = c(3840, 2160),
#'  add2It = list(
#'    points(x = arenaEdge[, 2], y = arenaEdge[, 1], cex = 0.01),
#'    points(x = arenaCenter[, 2], y = arenaCenter[, 1], cex = 0.01)
#'  ),
#'  colId = "Position",
#'  main = ""
#')
#'
#' @export

cutFrags <- function(trackDat, customFunc) {
  # identify part of fragment detected in the selected Time interval
  When <-
    lapply(trackDat, customFunc)
  # identify which fragment are detected in the selected Time interval
  Who <-
    which(unlist(lapply(When, function(y)
      TRUE %in% y)) == TRUE)
  # isolate part of detected fragment included in time interval
  WhoWhen <-
    lapply(When[c(names(Who))], function (z)
      which(z == TRUE))
  # store the selected fragment part in a list
  output <- setNames(lapply(names(WhoWhen), function(w)
    trackDat[[w]][c(WhoWhen[[w]]), ]), names(WhoWhen))
  return(output)
}