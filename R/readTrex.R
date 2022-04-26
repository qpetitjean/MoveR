#' @title Load Trex tracking output .npz files
#'
#' @description Given the path of a folder containing multiple Trex outputs (one .npz file per particle/individual),
#' this function returns a list of 9 vectors classically used for further computations using MovR package: 
#' \itemize{
#'    \item{'maj.ax': }{the length of major axis (i.e., the midline) for a particle over frame (i.e., length of the ellipse).}
#'    \item{'angle': }{the particle's absolute angle in radians, orientation of the particle according to X-axis.}
#'    \item{'min.ax': }{the length of minor axis for a particle over frame (i.e., width of the ellipse), returns NA since it is not present in the TREX output.}
#'    \item{'x.pos': }{x position of the particle's centroid.}
#'    \item{'y.pos': }{y position of the particle's centroid.}
#'    \item{'identity': }{the particle's identity given by the tracking software.}
#'    \item{'frame': }{montonically increasing integer number representing the elapsed time over each particle's trajectory.}
#'    \item{'ntargets': }{the number of particle tracked over each frame.}
#'    \item{'timestamps': }{the elapsed time over each frame, in seconds.}
#' }
#' 
#' Alternatively, the function can returns a list containing 2 sublists, the first corresponding to the one mentioned above 
#' and the second containing all the elements retrieved from the .npz files (see rawDat argument).
#' Also, by default the function mirror y coordinates to start on the bottom-left (see mirrorY argument).
#'
#' @param trexPath The path of the Trex outputs folder to load .npz files within R environment
#' (e.g. "C:/Users/username/Desktop/Trex_output")
#'
#' @param mirrorY TRUE or FALSE, set the origin of y coordinates, if TRUE y coordinates are mirrored to start on the bottom-left (default = TRUE)
#'
#' @param imgHeight A numeric value expressed in pixels, the true length of Y axis
#' corresponding to the height of the image or video resolution (default = 1080)
#'
#' @param rawDat TRUE or FALSE, if TRUE add a second list containing all the elements retrieved from .npz files, may drastically increase the size of the object returned by the function (default = FALSE)
#'
#' @return A set of two lists containing tracking data, the first list corresponding to raw tracking data
#' in usable format for this package, the second corresponding to raw data as returned by Trex
#'
#'
#' @authors Quentin PETITJEAN
#'
#' @seealso \code{\link{readCtrax}}, \code{\link{readTrackR}}, \code{\link{readIdtracker}}, \code{\link{mirrorYFunc}}
#'
#' @examples
#'  
#' # Load the list containing the 9 vectors classically used for further computation and mirror Y coordinates to start on the bottom-left
#' Data <- readTrex(system.file("sampleData/sample_1/TREXOutput", package = "MovR"), mirrorY = T, imgHeight = 1080, rawDat = F)
#'
#' # Load the list containing 2 sublists, the first containing the 9 vectors classically used for further computation and the 
#' # second list containing all the elements retrieved from .npz files, also do not mirror Y coordinates (start on the top-left)
#' Data <- readTrex(system.file("sampleData/sample_1/TREXOutput", package = "MovR"), mirrorY = F, imgHeight = NULL, rawDat = T)
#'
#'
#' @export

readTrex = function(trexPath,
                    mirrorY = TRUE,
                    imgHeight = NULL,
                    rawDat = FALSE) {
  if(mirrorY == TRUE & is.null(imgHeight)){ 
    stop("imgHeight argument is missing, the height of the image resolution is needed to mirror y coordinates")
    }
  # import numpy python module to read .npz files
  np <-
    reticulate::import("numpy")
  
  # create a list of the .npz files (output for each particle detected by Trex)
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
  
  # create a vector containing the id of particles and replace fish per indiv
  Trex_ind_names <-
    unlist(lapply(Trex_ind_list,
                  function(i) {
                    paste("indiv", unname(utils::tail(as.data.frame(
                      unlist(regmatches(i, gregexpr(
                        "[[:digit:]]+", i
                      )))
                    ), 1)), sep = "")
                  }))
  # load npz files of all particles detected in the tracking session in a list
  # and rename levels elements of the list according to particle's id
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
  ntargets <-
    unname(as.matrix(ntargets_temp$indiv_numb - ntargets_temp$ntargets_temp))
  ## create identity df
  identity_temp <-
    data.frame(gsub("_missing", "", missing_track$identity))
  names(identity_temp)[1] <- "identity"
  ## keep the id of particle only
  identity <-
    as.matrix(unname(as.numeric(
      gsub("indiv", "", identity_temp$identity)
    )))
  
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
      mirrorYFunc(metricList[["Y#wcentroid"]], imgHeight = imgHeight)
    metricList$Y <- mirrorYFunc(metricList$Y, imgHeight = imgHeight)
    metricList$midline_y <-
      mirrorYFunc(metricList$midline_y, imgHeight = imgHeight)
  }
  # add these variables to the output
  if (rawDat == FALSE) {
    Data_Trex_All <- list(
      maj.ax = unname(as.matrix(metricList$midline_length)),
      angle = unname(as.matrix(metricList$ANGLE)),
      min.ax = as.numeric(rep(NA, dim(
        metricList$midline_length
      )[1])),
      x.pos = unname(as.matrix(metricList[["X#wcentroid"]])),
      y.pos = unname(as.matrix(metricList[["Y#wcentroid"]])),
      identity = identity,
      frame = unname(as.matrix(metricList[["frame"]])),
      ntargets = ntargets,
      timestamps = unname(as.matrix(
        unique(metricList$timestamp / 1000000)
      ))
    )
  } else if (rawDat == TRUE) {
    Data_Trex_All <- list(
      Data_Trex <- list(
        maj.ax = unname(as.matrix(metricList$midline_length)),
        angle = unname(as.matrix(metricList$ANGLE)),
        min.ax = as.numeric(rep(NA, dim(
          metricList$midline_length
        )[1])),
        x.pos = unname(as.matrix(metricList[["X#wcentroid"]])),
        y.pos = unname(as.matrix(metricList[["Y#wcentroid"]])),
        identity = identity,
        frame = unname(as.matrix(metricList[["frame"]])),
        ntargets = ntargets,
        timestamps = unname(as.matrix(
          unique(metricList$timestamp / 1000000)
        ))
      ),
      Data_Trex_Raw <-
        lapply(metricList, function (x)
          unname(as.matrix(x)))
    )
  }
  return(Data_Trex_All)
}