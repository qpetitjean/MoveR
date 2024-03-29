#' @title Import Trex tracking output .npz files.
#'
#' @description Given the path of a folder containing multiple Trex outputs (one .npz file per particle/individual),
#' this function returns an object of class "tracklets", a list of tracklets (data frame) containing 7 elements classically used for further computations using MoveR package:
#' \itemize{
#'    \item{'maj.ax': }{the length of the major axis (i.e., the midline) for a particle over frame (i.e., length of the ellipse).}
#'    \item{'angle': }{the particle's absolute angle in radians, orientation of the particle according to X-axis.}
#'    \item{'min.ax': }{the length of the minor axis for a particle over frame (i.e., width of the ellipse), returns NA since it is not present in the TREX output.}
#'    \item{'x.pos': }{x position of the particle's centroid.}
#'    \item{'y.pos': }{y position of the particle's centroid.}
#'    \item{'identity': }{the particle's identity given by the tracking software.}
#'    \item{'frame': }{the video frame number at which the measurements has been made.}
#' }
#'
#' The function can also append all the others elements returned by the tracking software (see [rawDat] argument)
#' Also, the function can flip y coordinates (see [flipY] argument).
#'
#' @param trexPath The path of the TRex output folder where .npz files are stored.
#'
#' @param flipY A logical value (i.e., TRUE or FALSE) indicating whether the origin of y coordinates should be flipped. If TRUE, y coordinates are flipped to start on the bottom-left (default = FALSE).
#'
#' @param imgHeight A numeric value expressed in pixels, the length of Y axis
#' corresponding to the height of the image or video resolution (optional, only used when flipY = TRUE).
#'
#' @param rawDat A logical value (i.e., TRUE or FALSE) indicating whether all other elements retrieved from the tracking output should appended to the tracklets data (see \href{https://trex.run}{trex.run}),
#' Note that this may drastically increase the size of the object returned by the function (default = FALSE).
#'
#' @return An object of class "tracklets" containing a list of tracklets and their characteristics classically used for further computations.
#' In case [rawDat] argument is TRUE, it also append all the others elements returned by the tracking software (for TRex it corresponds to several metrics see \href{https://trex.run}{trex.run} for more details). 
#' Also, by default the function returns y coordinates starting on the top-left.
#'
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link{readAnimalTA}} \code{\link{readCtrax}}, \code{\link{readTrackR}}, \code{\link{readIdtracker}}, \code{\link{flipYCoords}}
#'
#' @references 
#' Tristan Walter, Iain D Couzin (2021) TRex, a fast multi-animal tracking system with markerless identification, and 2D estimation of posture and visual fields eLife 10:e64000.
#' \href{https://trex.run}{trex.run}
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
#' } 
#' @export

readTrex = function(trexPath,
                    flipY = FALSE,
                    imgHeight = NULL,
                    rawDat = FALSE) {
  
  error <- .errorCheck(imgHeight = imgHeight)
  if (flipY == TRUE & !is.null(error)) {
    stop(error)
  }
  
  # import numpy python module to read .npz files
  np <-
    reticulate::import("numpy")
  
  # create a list of the .npz files (i.e., the output for each particle detected by Trex)
  if (length(list.files(trexPath, pattern = "*.npz")) == 0) {
    stop("No such file or directory : undefined or wrong path supplied")
    
  } else {
    Trex_ind_list <-
      list.files(trexPath, pattern = "*.npz")
  }
  # remove the statistics file in case it is detected
  Trex_ind_list <-
    Trex_ind_list[!grepl ("statistics", Trex_ind_list)]
  
  # remove the posture files in case it is detected
  Trex_ind_list <-
    Trex_ind_list[!grepl ("posture", Trex_ind_list)]
  
  # remove the recognition files in case it is detected
  Trex_ind_list <-
    Trex_ind_list[!grepl ("recognition", Trex_ind_list)]
  
  # create a vector containing the particles' id and replace fish or other by the generic name: "indiv"
  Trex_ind_names <- unlist(lapply(Trex_ind_list, function(i) {
    indivDigits <- unlist(regmatches(i, gregexpr("[[:digit:]]+", i)))
    Trex_ind <- paste("indiv", indivDigits[length(indivDigits)], sep = "")
    return(Trex_ind)
  }))
  
  # load .npz files for all the particles detected in the tracking session in a list
  # and rename levels of the list according to particle's id
  indiv_list <- stats::setNames(lapply(Trex_ind_list, function(i) {
    np$load(file.path(trexPath, i))
  }),
  paste("data", Trex_ind_names, sep = "_"))
  
  Variable_list <- unlist(lapply(seq(length(indiv_list)), function(x) {
    stats::setNames(
      lapply(seq(length(indiv_list[[1]]$files)), function(y) {
        c(indiv_list[[x]][[indiv_list[[1]]$files[[y]]]])
      }),
      paste(Trex_ind_names[[x]], indiv_list[[1]]$files, sep =
              "_"))
  }), recursive = F)
  
  # create identity dataframe (not in Raw output of Trex)
  ## retrieve the missing metric: 1/0 when indiv is missing for each frame, respectively
  df_list_missing <-
    lapply(lapply(mget(
      ls(pattern = "*_missing", envir = as.environment(Variable_list)),
      envir = as.environment(Variable_list)
    ), function(x)
      as.data.frame(x)),
    stats::setNames,
    nm = "missingInd")
  for (i in seq(length(df_list_missing))) {
    df_list_missing[[i]]$frameNumb = seq(length(df_list_missing[[i]]$missingInd))
  }
  missing_track <- do.call("rbind", df_list_missing)
  missing_track$identity <-
    gsub("[.][0-9]*", "", rownames(missing_track))
  
  ## create identity df
  identity_temp <-
    data.frame(gsub("_missing", "", missing_track$identity))
  names(identity_temp)[1] <- "identity"
  ## keep the id of particle only
  identity <- as.numeric(gsub("indiv", "", identity_temp$identity))
  
  # merge the data of all particle in a list of df containing data for each measure
  ## create a vector with variable names ordered by decreasing number of character
  VarN <- unique(gsub("indiv[0-9]*_", "", names(Variable_list)))
  VarN <- VarN[order(-nchar(VarN))]
  
  ## loop through the vector to concatenate data by variable
  ## it is important to order VarN by decreasing number of characters to avoid wrong merging
  ## when some variable have a really short name which is used for naming another variable
  ## e.g. "X" and "AX"
  metricList <- list()
  for (i in seq(length(VarN))) {
    metricList[[VarN[i]]] <-
      unlist(mget(
        ls(pattern = VarN[i], envir = as.environment(Variable_list)),
        envir = as.environment(Variable_list)
      ), use.names = FALSE)
    ### suppress the appended variable from variable_list to avoid error during merging
    Variable_list <-
      Variable_list[-c(which(
        names(Variable_list) %in% ls(pattern = VarN[i], envir = as.environment(Variable_list))
      ))]
  }
  # if flipY = TRUE, flip the Y coordinates according to image height
  if (flipY == TRUE) {
    metricList[["Y#wcentroid"]] <-
      MoveR::flipYCoords(metricList[["Y#wcentroid"]], imgHeight = imgHeight)
    metricList$Y <- MoveR::flipYCoords(metricList$Y, imgHeight = imgHeight)
    metricList$midline_y <-
      MoveR::flipYCoords(metricList$midline_y, imgHeight = imgHeight)
  }
  # add these variables to the output
  if (rawDat == FALSE) {
    Data_Trex_All <- list(
      maj.ax = metricList[["midline_length"]],
      angle = metricList[["ANGLE"]],
      min.ax = as.numeric(rep(NA, length(
        metricList[["midline_length"]]
      ))),
      x.pos = metricList[["X#wcentroid"]],
      y.pos = metricList[["Y#wcentroid"]],
      identity = identity,
      frame = metricList[["frame"]]
    )
  } else if (rawDat == TRUE) {
    Data_Trex_All <- c(list(
        maj.ax = metricList[["midline_length"]],
        angle = metricList[["ANGLE"]],
        min.ax = as.numeric(rep(NA, length(
          metricList[["midline_length"]]
        ))),
        x.pos = metricList[["X#wcentroid"]],
        y.pos = metricList[["Y#wcentroid"]],
        identity = identity,
        frame = metricList[["frame"]]),
        lapply(metricList[-which(names(metricList) %in% c("frame", "frame_segments", "segment_vxys"))], function (x) x)
    )
  }

  # retrieve some information about tracking
  frameR <- max(Data_Trex_All[["frame"]], na.rm=T) / (max(metricList[["timestamp"]], na.rm=T)/1000000)
  
  # create a tracklets class object to return
  Data_Trex_All <- MoveR::convert2Tracklets(Data_Trex_All)
  
  # fill some attributes
  Data_Trex_All <- MoveR::setInfo(Data_Trex_All, frameR = frameR)
  
  return(Data_Trex_All)
}
