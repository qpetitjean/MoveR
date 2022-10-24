#' @title Locate a particle position.
#'
#' @description find the position of a particle along its trajectory according to a reference matrix indicating areas of interest.
#'
#'
#' @param refDat A matrix or dataframe or path to a file (either .txt or .csv) containing a distance matrix to any object or 
#' the location of one or several areas of interest (e.g., create a distance map using ImageJ).
#'
#' @param df A data frame containing x and y coordinates in columns named "x.pos", "y.pos" for a given fragment.
#'
#' @param sep The field separator character, the values on each line of the file are separated by this character (optional, needed only if refDat is a path).
#'
#' @param dec The character used in the file for decimal points (optional, needed only if refDat is a path).
#'
#' @return This function returns a vector containing the position of a particle according to a distance matrix to any object or 
#' the location of one or several areas of interest along the particle's trajectory.
#'
#' @author Quentin PETITJEAN
#'
#'
#' @examples
#'
#'# load the sample data
#'Data <-
#'  readTrex(
#'    system.file("sampleData/sample_1/TREXOutput", package = "MoveR"),
#'    mirrorY = T,
#'    imgHeight = 2160,
#'    rawDat = F
#'  )
#'# convert it to a list of fragments
#'trackDat <- convert2frags(Data[1:7], by = "identity")
#'
#'# load the reference dataset (A matrix or dataframe or path to a file (either .txt or .csv) containing a distance matrix to any object or 
#'# the location of one or several areas of interest (here we have created a distance map using ImageJ)
#'refDat <-
#'  as.matrix(read.delim(
#'    system.file("sampleData/sample_1/ReferenceData/TREXOutput/RefDat_2602_ISA3080_Low_5.csv", package = "MoveR"),
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
#'# draw the result, with fragments part that are on the edge of the arena colored in red 
#'# and those at the center in black
#'drawFrags(
#'  trackDat,
#'  imgRes = c(3840, 2160),
#'  add2It = list(
#'    points(x = arenaEdge[, 2], y = arenaEdge[, 1], cex = 0.01),
#'    points(x = arenaCenter[, 2], y = arenaCenter[, 1], cex = 0.01)
#'  ),
#'  colId = "Position"
#')
#'
#' @export

locaPos <- function(refDat = NULL, df = NULL, sep = NULL, dec = NULL) {
  if (is.null(refDat)) {
    stop(
      "refDat argument is missing, the function need a reference matrix to retrieve the location of the given particle"
    )
  }
  if (is.null(df)) {
    stop(
      "df argument is missing, \nthe function need a data frame containing x and y coordinates of a given particules along a trajectory to retrieve its location"
    )
  }
  if (!is.data.frame(df)) {
    stop(
      "df is not a dataframe, \nconsider transforming the data, the function need a data frame containing x and y coordinates of a given particules along a trajectory to retrieve its location"
    )
  }
  if (!is.data.frame(df)) {
    stop(
      "df does not contain x.pos and y.pos, \nverify that x and y coordinates of the particles are present in the df or are named x.pos and y.pos"
    )
  }
  if (!is.data.frame(refDat) &
      !is.matrix(refDat) & !is.array(refDat) & is.character(refDat)) {
    extens <- sub("^(.*\\.|[^.]+)(?=[^.]*)", "", refDat, perl = TRUE)
    if(is.null(dec)){
      dec <- "."
      warning(paste("dec argument is missing, default decimal points is", deparse("."), sep = " "))
    }
    if (extens == "txt") {
      if(is.null(sep)){
        sep <- "\t"
        warning(paste("sep argument is missing, for a .txt file default field separator is", deparse("\t"), sep = " "))
      }
      refDat <- read.delim(refDat, sep = sep, dec = dec)
    } else if (extens == "csv") {
      if(is.null(sep)){
        sep <- ";"
        warning(paste("sep argument is missing, for a .csv file default field separator is", deparse(";"), sep = " "))
      }
      refDat <- read.csv(refDat, sep = sep, dec = dec)
    } else{
      stop(
        "The extension of refDat is unknown (supported extension: .txt and .csv), \nconsider loading the matrix in R environment and call it within refDat argument"
      )
    }
  } else {
    refDat <- refDat
  }
  # find the position of the particle along its trajectory according to the reference matrix
  unlist(sapply(seq(nrow(df)), function(y)
    refDat[round(df$y.pos[y], digits = 0), round(df$x.pos[y], digits = 0)]))
}
