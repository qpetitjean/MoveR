#' @title Load Trex tracking output .npz files
#'
#' @description Given the path of a folder containing multiple Trex outputs (one .npz file per individual),
#' this function group them in a list containing 2 dataframes' lists.
#' The first list containing 8 elements is the processed output (i.e., the elements further used by functions of this package).
#' The second list of 33 elements return the classic set of variables returned by Trex.
#' If needed this function can also mirror Y coordinates
#'
#' @seealso readCtrax, mirrorYFunc
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
  if (length(trexPath %>% list.files(pattern = "*.npz")) == 0) {
    stop("undefined or wrong path supplied : No such file or directory")
    
  } else {
    Trex_ind_list <-
      trexPath %>%
      list.files(pattern = "*.npz")
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
  indiv_list <-
    lapply(Trex_ind_list, function(i) {
      np$load(file.path(trexPath, i))
    }) %>%
    purrr::set_names(paste("data", Trex_ind_names, sep = "_")) 

  Variable_list <- mapply(function(j, k) {
    as.matrix(j[[k]])
  },
  indiv_list,
  rep(indiv_list[[1]]$files, times = length(indiv_list))) %>%
    purrr::set_names(paste(Trex_ind_names, rep(indiv_list[[1]]$files, times = length(indiv_list)), sep =
                      "_"))
  
  # create frame and frame_segments dataframes
  # create a list of df, specifying the pattern argument
  df_list_frame_temp <-
    mget(ls(pattern = "*_frame", envir = as.environment(Variable_list)),
         envir = as.environment(Variable_list)) 
  df_list_frame_segments <-
    rlist::list.match(df_list_frame_temp, "_frame_segments")
  df_list_frame_temp <-
    rlist::list.remove(df_list_frame_temp,  names(df_list_frame_segments))
  df_list_frame <-
    rlist::list.remove(df_list_frame_temp,  "df_list_frame_segments")
  
  # bind results of all individuals for frame
  frame <-
    rlist::list.rbind(df_list_frame) 
  
  # bind results of all individuals for frame_segments
   frame_segments <-
    rlist::list.rbind(df_list_frame_segments) 
  
  # create x.pos.head, and x.pos.centroid dataframes
  # create a list of df, specifying the pattern argument
  df_list_x_temp <-
    mget(ls(pattern = "*_X", envir = as.environment(Variable_list)),
         envir = as.environment(Variable_list)) 
  
  # select lists which contain _X#wcentroid only
  df_list_Xwcentroid <-
    rlist::list.match(df_list_x_temp, "_X#wcentroid") 
  
  # remove previously selected lists from the full x list
  df_list_x_temp <-
    rlist::list.remove(df_list_x_temp,  names(df_list_Xwcentroid)) 
 
   # remove previously selected df_list_Xwcentroid from the full x list
  df_list_x <-
    rlist::list.remove(df_list_x_temp,  "df_list_Xwcentroid") 
  
  # bind results of all individuals for x position
  x.pos.head <-
    rlist::list.rbind(df_list_x) 
  
  # bind results of all individuals for xw centroid
   x.pos.centroid <-
    rlist::list.rbind(df_list_Xwcentroid) 
  
  # create y.pos.head and y.pos.centroid dataframes
  df_list_y_temp <-
    mget(ls(pattern = "*_Y", envir = as.environment(Variable_list)),
         envir = as.environment(Variable_list)) # create a list of df, specifying the pattern argument
  df_list_Ywcentroid <- rlist::list.match(df_list_y_temp, "_Y#wcentroid")
  df_list_y_temp <-
    rlist::list.remove(df_list_y_temp,  names(df_list_Ywcentroid))
  df_list_y <- rlist::list.remove(df_list_y_temp,  "df_list_Ywcentroid")
  
  # bind results of all individuals for y position
  y.pos.head <-
    rlist::list.rbind(df_list_y) 
  
  # bind results of all individuals for yw centroid
  y.pos.centroid <-
    rlist::list.rbind(df_list_Ywcentroid) 
  
  # create time and timestamps dataframes
  df_list_time_temp <-
    mget(ls(pattern = "*_time", envir = as.environment(Variable_list)),
         envir = as.environment(Variable_list))
  df_list_timestamp <- rlist::list.match(df_list_time_temp, "_timestamp")
  df_list_time_temp <-
    rlist::list.remove(df_list_time_temp,  names(df_list_timestamp))
  df_list_time <- rlist::list.remove(df_list_time_temp,  "df_list_timestamp")
  time <- rlist::list.rbind(df_list_time)
  timestamp <- rlist::list.rbind(df_list_timestamp)
  timestamps <- as.matrix(unname(unique(timestamp / 1000000)))
  
  # create SPEED, SPEED#wcentroid, SPEED#smooth#wcentroid and SPEED#pcentroid dataframes
  df_list_SPEED_temp <-
    mget(ls(pattern = "*_SPEED", envir = as.environment(Variable_list)),
         envir = as.environment(Variable_list))
  df_list_SPEEDwcentroid <-
    rlist::list.match(df_list_SPEED_temp, "_SPEED#wcentroid")
  df_list_SPEEDsmoothwcentroid <-
    rlist::list.match(df_list_SPEED_temp, "_SPEED#smooth#wcentroid")
  df_list_SPEEDpcentroid <-
    rlist::list.match(df_list_SPEED_temp, "_SPEED#pcentroid")
  df_list_SPEED_temp <-
    rlist::list.remove(df_list_SPEED_temp,  names(df_list_SPEEDwcentroid))
  df_list_SPEED_temp <-
    rlist::list.remove(df_list_SPEED_temp,  names(df_list_SPEEDsmoothwcentroid))
  df_list_SPEED_temp <-
    rlist::list.remove(df_list_SPEED_temp,  names(df_list_SPEEDpcentroid))
  df_list_SPEED <-
    rlist::list.remove(
      df_list_SPEED_temp,
      c(
        "df_list_SPEEDwcentroid",
        "df_list_SPEEDsmoothwcentroid",
        "df_list_SPEEDpcentroid"
      )
    )
  SPEED <- rlist::list.rbind(df_list_SPEED)
  SPEEDwcentroid <- rlist::list.rbind(df_list_SPEEDwcentroid)
  SPEEDsmoothwcentroid <- rlist::list.rbind(df_list_SPEEDwcentroid)
  SPEEDpcentroid <- rlist::list.rbind(df_list_SPEEDpcentroid)
  
  # create ACCELERATIONpcentroid dataframes
  df_list_ACCELERATIONpcentroid <-
    mget(
      ls(pattern = "*_ACCELERATION#pcentroid", envir = as.environment(Variable_list)),
      envir = as.environment(Variable_list)
    )
  ACCELERATIONpcentroid <- rlist::list.rbind(df_list_ACCELERATIONpcentroid)
  
  # create angle dataframes
  df_list_ANGLE <-
    mget(ls(pattern = "*_ANGLE", envir = as.environment(Variable_list)),
         envir = as.environment(Variable_list))
  angle <- rlist::list.rbind(df_list_ANGLE)
  
  # create ACCELERATIONwcentroid dataframes
  df_list_ACCELERATIONwcentroid <-
    mget(
      ls(pattern = "*_ACCELERATION#wcentroid", envir = as.environment(Variable_list)),
      envir = as.environment(Variable_list)
    )
  ACCELERATIONwcentroid <- rlist::list.rbind(df_list_ACCELERATIONwcentroid)
  
  # create angle dataframes
  df_list_ANGULAR_Acentroid <-
    mget(
      ls(pattern = "*_ANGULAR_A#centroid", envir = as.environment(Variable_list)),
      envir = as.environment(Variable_list)
    )
  ANGULAR_Acentroid <- rlist::list.rbind(df_list_ANGULAR_Acentroid)
  
  # create AX dataframes
  df_list_AX <-
    mget(ls(pattern = "*_AX", envir = as.environment(Variable_list)),
         envir = as.environment(Variable_list))
  AX <- rlist::list.rbind(df_list_AX)
  
  # create ANGULAR_Vcentroid dataframes
  df_list_ANGULAR_Vcentroid <-
    mget(
      ls(pattern = "*_ANGULAR_V#centroid", envir = as.environment(Variable_list)),
      envir = as.environment(Variable_list)
    )
  ANGULAR_Vcentroid <- rlist::list.rbind(df_list_ANGULAR_Vcentroid)
  
  # create AY dataframes
  df_list_AY <-
    mget(ls(pattern = "*_AY", envir = as.environment(Variable_list)),
         envir = as.environment(Variable_list))
  AY <- rlist::list.rbind(df_list_AY)
  
  # create MIDLINE_OFFSET dataframes
  df_list_MIDLINE_OFFSET <-
    mget(ls(pattern = "*_MIDLINE_OFFSET", envir = as.environment(Variable_list)),
         envir = as.environment(Variable_list))
  MIDLINE_OFFSET <- rlist::list.rbind(df_list_AY)
  
  # create BORDER_DISTANCEpcentroid dataframes
  df_list_BORDER_DISTANCEpcentroid <-
    mget(
      ls(pattern = "*_BORDER_DISTANCE#pcentroid", envir = as.environment(Variable_list)),
      envir = as.environment(Variable_list)
    )
  BORDER_DISTANCEpcentroid <-
    rlist::list.rbind(df_list_BORDER_DISTANCEpcentroid)
  
  # create VX dataframes
  df_list_VX <-
    mget(ls(pattern = "*_VX", envir = as.environment(Variable_list)),
         envir = as.environment(Variable_list))
  VX <- rlist::list.rbind(df_list_VX)
  
  # create VY dataframes
  df_list_VY <-
    mget(ls(pattern = "*_VY", envir = as.environment(Variable_list)),
         envir = as.environment(Variable_list))
  VY <- rlist::list.rbind(df_list_VY)
  
  # create midline_length dataframes
  df_list_midline_length <-
    mget(ls(pattern = "*_midline_length", envir = as.environment(Variable_list)),
         envir = as.environment(Variable_list))
  midline_length <- rlist::list.rbind(df_list_midline_length)
  
  # create midline_x dataframes
  df_list_midline_x <-
    mget(ls(pattern = "*_midline_x", envir = as.environment(Variable_list)),
         envir = as.environment(Variable_list))
  midline_x <- rlist::list.rbind(df_list_midline_x)
  
  # create midline_y dataframes
  df_list_midline_y <-
    mget(ls(pattern = "*_midline_y", envir = as.environment(Variable_list)),
         envir = as.environment(Variable_list))
  midline_y <- rlist::list.rbind(df_list_midline_y)
  
  # create normalized_midline dataframes
  df_list_normalized_midline <-
    mget(
      ls(pattern = "*_normalized_midline", envir = as.environment(Variable_list)),
      envir = as.environment(Variable_list)
    )
  normalized_midline <- rlist::list.rbind(df_list_normalized_midline)
  
  # create num_pixels dataframes
  df_list_num_pixels <-
    mget(ls(pattern = "*_num_pixels", envir = as.environment(Variable_list)),
         envir = as.environment(Variable_list))
  num_pixels <- rlist::list.rbind(df_list_num_pixels)
  
  # create segment_length dataframes
  df_list_segment_length <-
    mget(ls(pattern = "*_segment_length", envir = as.environment(Variable_list)),
         envir = as.environment(Variable_list))
  segment_length <- rlist::list.rbind(df_list_segment_length)
  
  # create segment_vxys dataframes
  df_list_segment_vxys <-
    mget(ls(pattern = "*_segment_vxys", envir = as.environment(Variable_list)),
         envir = as.environment(Variable_list))
  segment_vxys <- rlist::list.rbind(df_list_segment_vxys)
  
  # create missing, identity and ntargets dataframes
  df_list_missing_temp <-
    mget(ls(pattern = "*_missing", envir = as.environment(Variable_list)),
         envir = as.environment(Variable_list))
  df_list_missing_temp2 <-
    lapply(df_list_missing_temp, function(m) {
      as.data.frame(m)
    })
  df_list_missing_temp3 <-
    lapply(df_list_missing_temp2, setNames, nm = "missing_ind")
  df_list_missing <-
    purrr::map(df_list_missing_temp3, ~ tibble::rowid_to_column(.x, "frame_numb"))
  missing_track <- as.data.frame(df_list_missing %>%
                                   purrr::map_df(bind_rows, .id = "identity"))
  ntargets_temp <- ddply(missing_track, "frame_numb", numcolwise(sum))
  ntargets_temp$indiv_numb <-
    rep(length(unique(missing_track$identity)))
  ntargets <-
    as.matrix(unname(ntargets_temp$indiv_numb - ntargets_temp$missing_ind))
  
  # create identity df
  identity_temp <-
    data.frame(str_remove(missing_track$identity, "_missing"))
  names(identity_temp)[1] <- "identity"
  
  # keep the id of individual only
   identity <-
    as.matrix(unname(as.numeric(
      str_remove(identity_temp$identity, "indiv")
    ))) 
  missing <- as.matrix(unname(missing_track$missing_ind))
  
  # if mirrorY = TRUE, mirror the Y coordinates according to image height
  if (mirrorY == TRUE) {
    y.pos.centroid = mirrorYFunc(y.pos.centroid, imgHeight = imgHeight)
    y.pos.head = mirrorYFunc(y.pos.head, imgHeight = imgHeight)
    midline_y = mirrorYFunc(midline_y, imgHeight = imgHeight)
  }
  
  # add these variables to the output
  Data_Trex_All <- list(
    Data_Trex <- list(
      maj.ax = midline_length,
      angle = angle,
      min.ax = as.numeric(rep(NA, length(midline_length))),
      x.pos = x.pos.centroid,
      y.pos = y.pos.centroid,
      identity = identity,
      frame = frame,
      ntargets = ntargets,
      timestamps = timestamps
    ),
    
    Data_Trex_Raw <-
      list(
        ACCELERATIONpcentroid = ACCELERATIONpcentroid,
        ANGLE = angle,
        ACCELERATIONwcentroid = ACCELERATIONwcentroid,
        ANGULAR_Acentroid = ANGULAR_Acentroid,
        AX = AX,
        ANGULAR_Vcentroid = ANGULAR_Vcentroid,
        AY = AY,
        MIDLINE_OFFSET = MIDLINE_OFFSET,
        BORDER_DISTANCEpcentroid = BORDER_DISTANCEpcentroid,
        SPEEDwcentroid = SPEEDwcentroid,
        SPEEDsmoothwcentroid = SPEEDsmoothwcentroid,
        SPEEDpcentroid = SPEEDpcentroid,
        SPEED = SPEED,
        VX = VX,
        VY = VY,
        Xwcentroid = x.pos.centroid,
        Ywcentroid = y.pos.centroid,
        X = x.pos.head,
        Y = y.pos.head,
        frame = frame,
        midline_length = midline_length,
        midline_x = midline_x,
        midline_y = midline_y,
        missing = missing,
        normalized_midline = normalized_midline,
        num_pixels = num_pixels,
        segment_length = segment_length,
        time = time,
        timestamps = timestamps,
        frame_segments = frame_segments,
        segment_vxys = segment_vxys
      )
  )
}
