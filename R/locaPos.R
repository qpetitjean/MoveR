#' @title Locate a particle position.
#'
#' @description Given the position of a particle along its trajectory (a tracklet) and a reference matrix indicating the location of areas of interest in the same coordinates system
#' as those specifying the particle' position, this function retrieve the value (area of interest) given by the reference matrix for the corresponding coordinates over the particle' trajectory.
#'
#'
#' @param refDat A matrix or dataframe or path to a file (either .txt or .csv) containing a distance matrix to any object or
#' the location of one or several areas of interest (e.g., create a distance map using ImageJ - with x and y coordinates as columns and rows names respectively).
#'
#' @param df A data frame containing x and y coordinates in columns named "x.pos", "y.pos" for a given tracklet.
#'
#' @param sep The field separator character, the values on each line of the file are separated by this character (optional, needed only if refDat argument is a path).
#'
#' @param dec The character used in the file for decimal points (optional, needed only if refDat is a path).
#' 
#' @param Fun A custom function used to transform the data from df (e.g., round, ceiling, log).
#' NB: Indeed, while the tracking software generally return cartesian coordinates (x and y) as decimal numbers, distances map does not, 
#' it could hence be necessary to round the x and y coordinates to find correspondences with refDat. 
#'
#' @return This function returns a vector containing the position of a particle over its trajectory according to a distance matrix to any object or a matrix containing the location of 
#' an area of interest.
#'
#' @author Quentin PETITJEAN
#'
#'
#' @examples
#'
#' ## Not run:
#' 
#' # Download the first dataset from the sample data repository
#' Path2Data <- MoveR::DLsampleData(dataSet = 1, tracker = "TRex")
#' Path2Data
#' 
#' # Import the list containing the 9 vectors classically used for further computation
#' # and flip Y coordinates to start on the bottom-left
#' Data <- MoveR::readTrex(Path2Data[[1]],
#'                         flipY = T,
#'                         imgHeight = 2160,
#'                         rawDat = F)
#' str(Data)
#' 
#' # convert it to a list of tracklets
#' trackDat <- MoveR::convert2Tracklets(Data[1:7], by = "identity")
#' 
#' # Import the reference dataset (A matrix or dataframe or path to a file (either .txt or .csv) containing a distance matrix to any object or
#' # the location of one or several areas of interest (here we have created a distance map using ImageJ)
#' refDat <- as.matrix(read.delim(Path2Data[[2]],
#'                                dec = "."))
#' 
#' ##  retrieve the value of the edge limit (1) and of the center limit (254) to plot them
#' arenaEdge <-
#'   stats::setNames(data.frame(which(refDat == 1, arr.ind = T)),
#'                   c("y.pos", "x.pos"))
#' arenaCenter <-
#'   stats::setNames(data.frame(which(refDat == 254, arr.ind = T)),
#'                   c("y.pos", "x.pos"))
#' 
#' ## then specify that all values above 254 are considered as the center and the remaning at the edge
#' refDat <-
#'   apply(refDat, 2, function(x)
#'     ifelse(x > 254, "center", "edge"))
#' 
#' # retrieve the area where the particles
#' # are located over their whole trajectory using locaPos function
#' trackDat <- MoveR::analyseTracklets(trackDat,
#'                                     customFunc = list(
#'                                       Position = function(x)
#'                                         locaPos(
#'                                           refDat,
#'                                          x,
#'                                          Fun = function(y)
#'                                             ifelse(round(y, digits = 0) == 0,
#'                                                    ceiling(y),
#'                                                    round(y, digits = 0))
#'                                         )
#'                                       
#'                                     ))
#' 
#' # draw the result, with tracklets part that are on the edge of the arena colored in red
#' # and those at the center in black
#' MoveR::drawTracklets(trackDat,
#'                      add2It = list(
#'                        points(x = arenaEdge[["x.pos"]], y = arenaEdge[["y.pos"]], cex = 0.01),
#'                        points(x = arenaCenter[["x.pos"]], y = arenaCenter[["y.pos"]], cex = 0.01)
#'                      ),
#'                      colId = "Position")
#' 
#' ## End(Not run)
#'
#' @export

locaPos <-
  function(refDat = NULL,
           df = NULL,
           sep = NULL,
           dec = NULL,
           Fun = NULL) {
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
        !is.matrix(refDat) &
        !is.array(refDat) & is.character(refDat)) {
      extens <- sub("^(.*\\.|[^.]+)(?=[^.]*)", "", refDat, perl = TRUE)
      if (is.null(dec)) {
        dec <- "."
        warning(paste(
          "dec argument is missing, default decimal points is",
          deparse("."),
          sep = " "
        ))
      }
      if (extens == "txt") {
        if (is.null(sep)) {
          sep <- "\t"
          warning(
            paste(
              "sep argument is missing, for a .txt file default field separator is",
              deparse("\t"),
              sep = " "
            )
          )
        }
        refDat <- as.matrix(read.delim(refDat, sep = sep, dec = dec))
      } else if (extens == "csv") {
        if (is.null(sep)) {
          sep <- ";"
          warning(
            paste(
              "sep argument is missing, for a .csv file default field separator is",
              deparse(";"),
              sep = " "
            )
          )
        }
        refDat <- as.matrix(read.csv(refDat, sep = sep, dec = dec))
      } else{
        stop(
          "The extension of refDat is unknown (supported extension: .txt and .csv), \nconsider loading the matrix in R environment and call it within refDat argument"
        )
      }
    } else {
      refDat <- refDat
    }
    # find the position of the particle along its trajectory according to the reference matrix
    Res <- unlist(sapply(seq(nrow(df)), function(y) {
      if (!is.null(Fun)) {
        temp <-
          refDat[Fun(df[["y.pos"]][y]), Fun(df[["x.pos"]][y])]
      } else{
        temp <- refDat[df[["y.pos"]][y], df[["x.pos"]][y]]
      }
      if (length(temp) == 0) {
        temp <- NA
        # return a warning message if some values have been not found in refDat
        warning(
          paste0(
            "locaPos returned NA for some values: y.pos = ",
            "[", ifelse(!is.null(Fun), Fun(df[["y.pos"]][y]),  df[["y.pos"]][y]),"]",
            ", x.pos = ",
            "[", ifelse(!is.null(Fun), Fun(df[["x.pos"]][y]), df[["x.pos"]][y]), "]",
            ", these values have been not found in refDat."
          )
        )
      }
      return(temp[[1]])
    }))
    return(Res)
  }