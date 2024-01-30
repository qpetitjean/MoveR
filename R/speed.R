#' @title Compute particle's speed over a trajectory.
#'
#' @description Given a data frames containing tracking information for a given tracklet,
#' this function scale tracklet path, convert it to a desired time unit and returns a vector containing the value of
#' speed along this tracklet.
#'
#'
#' @param df A data frame containing x and y coordinates in columns named "x.pos", "y.pos" for a given tracklet, as well as
#' a column containing time information, whatever the unit, over the trajectory.
#'
#' @param scale A ratio corresponding to the scaling factor to be applied to the trajectory coordinates
#' (e.g., size in cm / size in pixels; see \code{\link[trajr]{TrajScale}}, default = 1).
#'
#' @param timeCol A character string corresponding to the name of the column containing Time information (default = 'frame').
#'
#' @param frameR A numeric value indicating the frame rate of the video, used to convert the speed value according to desired time unit, see also timeU argument (default = 1).
#'
#' @param timeU A character string indicating the time unit in which the speed values should be converted, "f"=frame, "s"=second, "m"=minute, "h"=hour, "d"=day (default = "f").
#' 
#'
#' @return This function returns a vector containing the values of speed over a trajectory.
#'
#'
#' @author Quentin PETITJEAN
#' 
#' @seealso \code{\link[trajr]{TrajDerivatives}}
#'
#' @examples
#'
#' set.seed(2023)
#' # generate a dummy tracklet
#' ## start to specify some parameters to generate the tracklet
#' TrackL <-
#'   100 # the length of the tracklet or a sequence to randomly sample tracklet's length
#' 
#' TrackDatTemp <-
#'   trajr::TrajGenerate(sample(TrackL, 1), random = TRUE, fps = 1)
#' TrackDat <-
#'   data.frame(
#'     x.pos = TrackDatTemp[["x"]] - min(TrackDatTemp[["x"]]),
#'     y.pos = TrackDatTemp[["y"]] - min(TrackDatTemp[["y"]]),
#'     frame = TrackDatTemp[["time"]]
#'   )
#' 
#' # compute the speed of the particle along its trajectory,
#' # expressing the speed as pixels/frame
#' TrackDat[["speedFrame"]] <-
#'   MoveR::speed(TrackDat, scale = 1, timeCol = "frame")
#' 
#' # to compute the speed according to another time unit, a new column containing the new timeline could be used
#' # here we consider that the frame rate is 25 frame per second
#' TrackDat[["second"]] <- TrackDat[["frame"]] / 25
#' 
#' # then compute the speed of the particle over its trajectory according to the new time unit
#' TrackDat[["speedSec"]] <-
#'   MoveR::speed(TrackDat, scale = 1, timeCol = "second")
#' 
#' str(TrackDat)
#' 
#' # alternatively, we can use the frameR and timeU arguments to specify the frame rate and the desired time unit within the function
#' TrackDat[["speedSec2"]] <-
#'   MoveR::speed(TrackDat, scale = 1, timeCol = "frame", frameR = 25, timeU = "s")
#' 
#' str(TrackDat)
#' 
#' # it is also possible to resample the tracklet before computing speed, here every 10 time unit (i.e., frame)
#' sampledFragDat <-
#'   MoveR::resampTracklets(MoveR::trackletsClass(list(TrackDat)), timeCol = "frame",  Tstep = 10)[[1]]
#' 
#' # and then compute the speed of the particle over its trajectory
#' sampledFragDat[["speedResampl"]] <-
#'   MoveR::speed(sampledFragDat, scale = 1, timeCol = "frame")
#' 
#' str(sampledFragDat) 
#'
#' @export

speed <- function(df,
                  scale = 1,
                  timeCol = 'frame',
                  frameR = 1,
                  timeU = c("f", "s", "m", "h", "d")) {
  
  error <- .errorCheck(df = df, x.pos = "x.pos", y.pos = "y.pos" , timeCol = timeCol)
  if(!is.null(error)){
    stop(error)
  }
  
  if(length(timeU) == length(c("f", "s", "m", "h", "d")) & identical(timeU, c("f", "s", "m", "h", "d"))){
    timeU = "f"
  }else if(length(timeU) > 1){
    stop("[timeU] argument is too long, choose among 'f', 's', 'm', 'h' or 'd' to convert speed values to the desired time unit")
  }else if(length(timeU) == 1 && !timeU %in% c("f", "s", "m", "h", "d")){
    stop("[timeU] argument is unknown:","[", timeU, "],", " choose among 'f', 's', 'm', 'h' or 'd' to convert speed values to the desired time unit")
  }
  
  trj <-
    trajr::TrajFromCoords(df[, c("x.pos", "y.pos", timeCol)],
                          timeCol = 3, 
                          spatialUnits = "NA",
                          timeUnits = "NA")
  trj <- trajr::TrajScale(trj, scale, units = "NA")
  trjderiv <- trajr::TrajDerivatives(trj)
  speedRes <-  base::append(trjderiv[[1]], NA, after = 0)
  
  if(timeU == "s"){ 
    speedRes <- speedRes * frameR
  }else if(timeU == "m"){
    speedRes <- speedRes * frameR * 60
  }else if(timeU == "h"){
    speedRes * frameR * 60 * 60
  }else if(timeU == "d"){
    speedRes * frameR * 60 * 60 * 24
  }
  return(speedRes)
}
