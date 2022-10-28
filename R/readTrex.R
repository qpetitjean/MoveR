#' @title Load Trex tracking output .npz files.
#'
#' @description Given the path of a folder containing multiple Trex outputs (one .npz file per particle/individual),
#' this function returns a list of 9 vectors classically used for further computations using MoveR package:
#' \itemize{
#'    \item{'maj.ax': }{the length of the major axis (i.e., the midline) for a particle over frame (i.e., length of the ellipse).}
#'    \item{'angle': }{the particle's absolute angle in radians, orientation of the particle according to X-axis.}
#'    \item{'min.ax': }{the length of the minor axis for a particle over frame (i.e., width of the ellipse), returns NA since it is not present in the TREX output.}
#'    \item{'x.pos': }{x position of the particle's centroid.}
#'    \item{'y.pos': }{y position of the particle's centroid.}
#'    \item{'identity': }{the particle's identity given by the tracking software.}
#'    \item{'frame': }{the video frame number at which the measurements has been made.}
#'    \item{'ntargets': }{the number of particle tracked over each frame.}
#'    \item{'timestamps': }{the elapsed time over each frame, in seconds.}
#' }
#'
#' Alternatively, the function can returns a list containing 2 sublists, the first corresponding to the one mentioned above
#' and the second containing all the elements retrieved from the .npz files (see rawDat argument).
#' Also, by default the function mirror y coordinates to start on the bottom-left (see mirrorY argument).
#'
#' @param trexPath The path of the Trex output folder where .npz files are stored.
#'
#' @param mirrorY TRUE or FALSE, set the origin of y coordinates, if TRUE y coordinates are mirrored to start on the bottom-left (default = FALSE).
#'
#' @param imgHeight A numeric value expressed in pixels, the length of Y axis
#' corresponding to the height of the image or video resolution (optional, only used when mirrorY = TRUE).
#'
#' @param rawDat TRUE or FALSE, if TRUE add a second list containing all the elements retrieved from .npz files (see \href{https://trex.run}{trex.run}),
#' may drastically increase the size of the object returned by the function (default = FALSE).
#'
#' @return A list containing either a list of 9 elements classically used for further computations or a list containing 2 sublists, the first corresponding to the one previously mentioned
#' and the second containing all the elements retrieved from the .npz files (see rawDat argument). Also, by default the function mirror y coordinates to start on the bottom-left.
#'
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link{readCtrax}}, \code{\link{readTrackR}}, \code{\link{readIdtracker}}, \code{\link{mirrorYFunc}}
#'
#' @references 
#' Tristan Walter, Iain D Couzin (2021) TRex, a fast multi-animal tracking system with markerless identification, and 2D estimation of posture and visual fields eLife 10:e64000.
#' \href{https://trex.run}{trex.run}
#'
#' @examples
#'
#' # Load the list containing the 9 vectors classically used for further computation
#' # and mirror Y coordinates to start on the bottom-left
#'
#' Data <-
#'   readTrex(
#'     system.file("sampleData/sample_1/TREXOutput", package = "MoveR"),
#'     mirrorY = T,
#'     imgHeight = 2160,
#'     rawDat = F
#'   )
#'
#' # Load the list containing 2 sublists, the first containing the 9 vectors classically used for further computation
#' # and the second list containing all the elements retrieved from .npz files,
#' # also do not mirror Y coordinates (start on the top-left)
#'
#' Data <-
#'   readTrex(
#'     system.file("sampleData/sample_1/TREXOutput", package = "MoveR"),
#'     mirrorY = F,
#'     imgHeight = NULL,
#'     rawDat = T
#'   )
#'
#' @export

readTrex = function(trexPath,
                    mirrorY = FALSE,
                    imgHeight = NULL,
                    rawDat = FALSE) {
  if (mirrorY == TRUE & is.null(imgHeight)) {
    stop(
      "imgHeight argument is missing, the height of the image resolution is needed to mirror y coordinates"
    )
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
  
  Variable_list <-  stats::setNames(mapply(
    function(j, k) {
      as.matrix(j[[k]])
    },
    indiv_list,
    rep(indiv_list[[1]]$files, times = length(indiv_list))
  ),
  paste(Trex_ind_names, rep(indiv_list[[1]]$files, times = length(indiv_list)), sep =
          "_"))
  
  # create identity and ntargets dataframes (not in Raw output of Trex)
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
  ntargets_temp <-
    stats::ave(missing_track$missingInd, missing_track$frameNumb, FUN = sum)
  ntargets_temp <-
    unique(as.data.frame(cbind(ntargets_temp, frameNumb = missing_track$frameNumb)))
  ## compute number of missing particle per frame
  ntargets_temp$indiv_numb <-
    rep(length(unique(missing_track$identity)))
  ntargets <- ntargets_temp$indiv_numb - ntargets_temp$ntargets_temp
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
      as.data.frame(do.call("rbind", mget(
        ls(pattern = VarN[i], envir = as.environment(Variable_list)),
        envir = as.environment(Variable_list)
      )))
    ### suppress the appended variable from variable_list to avoid error during merging
    Variable_list <-
      Variable_list[-c(which(
        names(Variable_list) %in% ls(pattern = VarN[i], envir = as.environment(Variable_list))
      ))]
  }
  # if mirrorY = TRUE, mirror the Y coordinates according to image height
  if (mirrorY == TRUE) {
    metricList[["Y#wcentroid"]] <-
      MoveR::mirrorYFunc(metricList[["Y#wcentroid"]], imgHeight = imgHeight)
    metricList$Y <- MoveR::mirrorYFunc(metricList$Y, imgHeight = imgHeight)
    metricList$midline_y <-
      MoveR::mirrorYFunc(metricList$midline_y, imgHeight = imgHeight)
  }
  # add these variables to the output
  if (rawDat == FALSE) {
    Data_Trex_All <- list(
      maj.ax = metricList[["midline_length"]][[1]],
      angle = metricList[["ANGLE"]][[1]],
      min.ax = as.numeric(rep(NA, length(
        metricList[["midline_length"]][[1]]
      ))),
      x.pos = metricList[["X#wcentroid"]][[1]],
      y.pos = metricList[["Y#wcentroid"]][[1]],
      identity = identity,
      frame = metricList[["frame"]][[1]],
      ntargets = ntargets,
      timestamps = unique(metricList[["timestamp"]][[1]] / 1000000)
    )
  } else if (rawDat == TRUE) {
    Data_Trex_All <- list(
      Data_Trex = list(
        maj.ax = metricList[["midline_length"]][[1]],
        angle = metricList[["ANGLE"]][[1]],
        min.ax = as.numeric(rep(NA, length(
          metricList[["midline_length"]][[1]]
        ))),
        x.pos = metricList[["X#wcentroid"]][[1]],
        y.pos = metricList[["Y#wcentroid"]][[1]],
        identity = identity,
        frame = metricList[["frame"]][[1]],
        ntargets = ntargets,
        timestamps = unique(metricList[["timestamp"]][[1]] / 1000000)
      ),
      Data_Trex_Raw =
        lapply(metricList, function (x) x[[1]])
      )
  }
  return(Data_Trex_All)
}
