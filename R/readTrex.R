#' @title Load Trex tracking output .npz files
#'
#' @description Given the path of a folder containing multiple Trex outputs (one .npz file per individual),
#' this function group them in a list containing 2 dataframes' lists.
#' The first list containing 8 elements is the processed output (i.e., the elements further used by functions of this package).
#' The second list returns the set of variables computed by Trex.
#' If needed this function can also mirror Y coordinates
#'
#' @seealso \code{\link{readCtrax}}, \code{\link{readTrex}}, \code{\link{readTrackR}}, \code{\link{readIdtracker}}, \code{\link{mirrorYFunc}}
#'
#' @param trexPath The path of the Trex outputs folder to load .npz files within R environment 
#' (e.g. "C:/Users/[username]/Desktop/Trex_output")
#' 
#' @param mirrorY TRUE or FALSE, set the origin of Y coords, if TRUE Y coords are mirrored
#' 
#' @param imgHeight A numeric value expressed in pixels, the true length of Y axis 
#' corresponding to the height of the image or video resolution (default = 1080)
#' 
#' 
#'
#' @return A set of two lists containing tracking data, the first list corresponding to raw tracking data 
#' in usable format for this package, the second corresponding to raw data as returned by Trex
#' 
#'
#' @authors Quentin Petitjean
#'
#'
#'
#' @examples
#'
#' # TODO
#'
#' @export

readTrex = function(trexPath,
                    mirrorY = FALSE,
                    imgHeight = 1080) {
  # import numpy python module to read .npz files
  np <-
    reticulate::import("numpy")
  
  # create a list of the .npz files (output for each individuals detected by Trex)
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
  
  # remove the posture files in case it is detected
  Trex_ind_list <-
    Trex_ind_list[!grepl ("recognition", Trex_ind_list)]
  
  # create a vector containing the id of individuals and replace fish per indiv
  Trex_ind_names <-
    unlist(lapply(Trex_ind_list,
                  function(i) {
                    paste("indiv", unname(tail(as.data.frame(
                      unlist(regmatches(i, gregexpr(
                        "[[:digit:]]+", i
                      )))
                    ), 1)), sep = "")
                  }))
  # load npz files of all individuals detected in the tracking session in a list
  # and rename levels elements of the list according to individual's id
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
  ntargets_temp <- unique(as.data.frame(cbind(ntargets_temp, frameNumb = missing_track$frameNumb)))
  ## compute number of missing individual per frame
  ntargets_temp$indiv_numb <-
    rep(length(unique(missing_track$identity)))
  ntargets <-
    unname(as.matrix(ntargets_temp$indiv_numb - ntargets_temp$ntargets_temp))
  ## create identity df
  identity_temp <-
    data.frame(gsub("_missing", "", missing_track$identity))
  names(identity_temp)[1] <- "identity"
  ## keep the id of individual only
  identity <-
    as.matrix(unname(as.numeric(
      gsub("indiv", "", identity_temp$identity)
    )))
  
  # merge the data of all individual in a list of df containing data for each measure
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
      frame = unname(as.matrix(unique(metricList[["frame"]]))),
      ntargets = ntargets,
      timestamps = unname(as.matrix(unique(
        metricList$timestamp / 1000000
      )))
    ),
    Data_Trex_Raw <-
      lapply(metricList, function (x)
        unname(as.matrix(x)))
  )
  return(Data_Trex_All)
}
