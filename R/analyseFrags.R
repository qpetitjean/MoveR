#' @title Perform custom computation over a tracklet list.
#'
#' @description Given a list of data frames containing tracking information for each tracklet and
#' a custom function (or list of functions), this function iterate trough the tracklet lists to perform the specified computation
#' and returns the original list of data frames with the result of the analysis appended.
#'
#' @param trackDat A list of data frame containing tracking information for each tracklet.
#'
#' @param customFunc A function or a list of functions used to perform the computation over all tracklets
#' NB: in case customFunc is a list of unnamed function it will try to retrieve their names by returning the first character string
#' following the function() call as result column name.
#'
#' @param progress A Boolean (i.e., TRUE or FALSE) indicating whether a progress bar should be displayed to inform process progression.
#'
#' @return this function returns the original list of data frames (i.e., tracklet)
#' with the result of the specified computation appended.
#'
#' @author Quentin PETITJEAN
#'
#' @examples
#'
#' set.seed(2023)
#' # generate some dummy tracklets
#' ## start to specify some parameters to generate tracklets
#' TrackN <- 40 # the number of tracklet to simulate
#' TrackL <-
#'   1:1000 # the length of the tracklets or a sequence to randomly sample tracklet length
#' id <- 0
#' TrackList <- stats::setNames(lapply(lapply(seq(TrackN), function(i)
#'   trajr::TrajGenerate(sample(TrackL, 1), random = TRUE, fps = 1)), function(j) {
#'     id <<- id + 1
#'     data.frame(
#'       x.pos = j$x - min(j$x),
#'       y.pos = j$y - min(j$y),
#'       frame = j$time,
#'       identity = paste("Tracklet", id, sep = "_")
#'     )
#'   }), seq(TrackN))
#' 
#' # check the fragments
#' MoveR::drawFrags(TrackList,
#'                  timeCol = "frame")
#' 
#' # Run some computation on the dataset using analyseFrags
#' TrackList2 <-
#'   MoveR::analyseFrags(
#'     TrackList,
#'     customFunc = list(
#'       # specify a first function to compute speed over each fragment (a modulus present within the MoveR package)
#'       speed = function(x)
#'         MoveR::speed(x,
#'                      TimeCol = "frame",
#'                      scale = 1),
#'       # compute turning angle in radians over each fragment (a modulus present within the MoveR package)
#'       TurnAngle = function(x)
#'         MoveR::turnAngle(
#'           x,
#'           TimeCol = "frame",
#'           unit = "radians",
#'           scale = 1
#'         ),
#'       # convert the time expressed in frame in second using a conversion factor of 25 frame per second
#'       TimeSec = function(x)
#'         x[["frame"]] / 25,
#'       # or in minutes
#'       TimeMin = function(x)
#'         x[["frame"]] / 25 / 60
#'     )
#'   )
#' 
#' # check the result for the first fragment
#' str(TrackList2[["1"]])
#' 
#' # plot the frequency plot of the particles' speed
#' par(mfrow = c(1, 2))
#' hist(MoveR::convert2list(TrackList2)[["speed"]],
#'      main = "Frequency plot of the particles' speed")
#' 
#' # plot the frequency plot of the particles' turning angle
#' Ht = circular::circular(
#'   MoveR::convert2list(TrackList2)[["TurnAngle"]],
#'   type = "angle",
#'   units = "radians",
#'   zero = 0
#' )
#' circular::rose.diag(
#'   Ht,
#'   bins = 24,
#'   shrink = 0.89,
#'   xlim = c(-1, 1),
#'   ylim = c(-1, 1),
#'   prop = 2,
#'   col = "gray",
#'   border = "black",
#'   units = 'radians',
#'   ticks = TRUE,
#'   main = "Frequency plot of the particles' turning angle"
#' )
#'
#' @export

analyseFrags <- function(trackDat,
                         customFunc,
                         progress = TRUE) {
  if (is.null(customFunc)) {
    stop("customFunc argument is missing, a customFunc is needed to compute metric")
  }
  # if customFunc is a unnamed list of function, retrieve function names
  if (is.list(customFunc)) {
    if (is.null(names(customFunc))) {
      VarName <-
        lapply(customFunc, function(x)
          strsplit(sub("\\(.*", "", deparse(x)), " ")[[2]])
      names(customFunc) <- unlist(VarName)
    }
    # if customFunc is a function retrieve function names and transformed it to a named list
  } else if (is.function(customFunc)) {
    VarName <- strsplit(sub("\\(.*", "", deparse(customFunc)), " ")[[2]]
    customFunc <- list(customFunc)
    names(customFunc) <- VarName
  }
  # loop trough the tracklet list and append the result to each list by using the name of the custom function
  if (isTRUE(progress)) {
    total = length(trackDat)
    pb <-
      progress::progress_bar$new(format = "Processing Tracklets [:bar] :current/:total (:percent)", total = total)
    pb$tick(0)
  }
  # if trackDat is an unnamed list of tracklets use the position of the tracklets in the list as tracklet name
  if (is.null(names(trackDat))) {
    names(trackDat) <- seq(length(trackDat))
  }
  for (i in names(trackDat)) {
    for (j in names(customFunc)) {
      if (!inherits(try(customFunc[[j]](trackDat[[i]]), silent = T)
                    , "try-error") &
          !class(customFunc[[j]](trackDat[[i]])) == "function") {
        trackDat[[i]][[j]] <- customFunc[[j]](trackDat[[i]])
      } else if (inherits(try(customFunc[[j]](trackDat[[i]]), silent = T)
                          , "try-error") |
                 class(customFunc[[j]](trackDat[[i]])) == "function") {
        trackDat[[i]][[j]] <- NA
        warning(
          "For customFunc ",
          j ,
          " in tracklet ",
          i ,
          " : \nanalyseFrags returned NA, perhaps check customFunc argument"
        )
      }
    }
    if (isTRUE(progress)) {
      # progress bar
      pb$tick(1)
    }
  }
  return(trackDat)
}