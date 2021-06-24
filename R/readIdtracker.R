#' @title Load idtracker tracking output .npy file
#'
#' @description
#'
#' @seealso read_Ctrax, read_Trex, readTrackR, mirrorY_coords
#'
#' @param IdtrackerPath The path of the idtrackerai output file (.npz) to load within R environment
#' (e.g. "C:/Users/[username]/Desktop/video_folder/trajectories.npz")
#'
#' @param mirrorY TRUE or FALSE, set the origin of Y coords, if TRUE Y coords are mirrored
#'
#' @param imgHeight A numeric value expressed in pixels, the true length of Y axis
#' corresponding to the height of the image or video resolution (default = 1080)
#'
#'
#'
#' @return A list containing tracking data
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

readIdtracker = function(IdtrackerPath,
                         mirrorY = FALSE,
                         imgHeight = NA) {
  # import numpy python module to read .npz files
  np <-
    import("numpy")
  
  idtracker_data <-
    np$load(
      IdtrackerPath,
      allow_pickle = TRUE
    )
  
  # create x.pos and y.pos vectors containing cartesian coordinates for all detected individuals
  x.pos <- c(idtracker_data[[1]]$trajectories[, , 1])
  y.pos <- c(idtracker_data[[1]]$trajectories[, , 2])
  
  # create identity and frame vector containing the numeric identity of each individuals and the sequence of frame for each individuals
  identity <- vector()
  frame <- vector()
  for (i in seq(ncol(idtracker_data[[1]]$trajectories[, , 2]))) {
    identity_temp <- rep(i, nrow(idtracker_data[[1]]$trajectories[, , 1]))
    identity <- c(identity, identity_temp)
    frame_temp <- seq(nrow(idtracker_data[[1]]$trajectories[, , 1]))
    frame <- c(frame, frame_temp)
  }
  
  # create ntargets vector containing the number of individuals detected within each frame
  ntargets <- vector()
  for (j in seq(nrow(idtracker_data[[1]]$trajectories[, , 1]))) {
    ntargets_temp <-
      length(idtracker_data[[1]]$trajectories[, , 1][j, ]) - length(which(is.na(idtracker_data[[1]]$trajectories[, , 1][j, ])))
    ntargets <- c(ntargets, ntargets_temp)
  }
  ntargets
  
  # if mirrorY = TRUE, mirror the Y coordinates according to image height
  if (mirrorY == TRUE) {
    y.pos = mirrorYFunc(y.pos, imgHeight = imgHeight)
    
  }
  
  idtrackerRaw <- list(
    maj.ax = as.numeric(rep(NA, length(x.pos))),
    angle = as.numeric(rep(NA, length(x.pos))),
    min.ax = as.numeric(rep(NA, length(x.pos))),
    x.pos = x.pos,
    y.pos = y.pos,
    identity = identity,
    frame = frame,
    ntargets = ntargets,
    timestamps = unique(frame)
  )
  
  return(idtrackerRaw)
  
}

