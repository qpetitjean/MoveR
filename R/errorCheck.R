#' @title Check some conditions.
#'
#' @description Given various arguments specific to the functions used in the MoveR package, the function test the condition and returns an error message accordingly.
#'
#' @return this function returns an error message if some condition are not met; the function is not exported.
#'
#' @author Quentin PETITJEAN
#'
#' @keywords internal

.errorCheck <-
  function(trackDat = NA,
           df = NA,
           customFunc = NA,
           x.pos = NA,
           y.pos = NA,
           speedCol = NA,
           minSpeed = NA,
           timeCol = NA,
           imgHeight = NA,
           Bstate = NA,
           pattern = NA,
           refDat = NA,
           turnAngle = NA,
           distTraveled = NA,
           Tstep = NA) {
    if (!identical(df, NA)) {
      if (is.null(df)) {
        return(
          "[df] argument is missing, \nthe function need a data frame containing x and y coordinates of a given particules along a trajectory"
        )
      }
      if (!is.null(df) && !identical(df, NA) && !is.data.frame(df)) {
        return(
          "[df] is not a dataframe, \nconsider transforming the data, the function need a data frame containing x and y coordinates of a given particules along a trajectory"
        )
      }
      if (!is.na(x.pos) && is.null(MoveR::listGet(df, "x.pos"))) {
        return("x.pos column is missing or might be misspelled")
      }
      if (!is.na(y.pos) && is.null(MoveR::listGet(df, "y.pos"))) {
        return("y.pos column is missing or might be misspelled")
      }
      if (!is.na(speedCol) && is.null(MoveR::listGet(df, speedCol))) {
        return("[speedCol] argument is misspelled or is absent from the input df")
      }
      if (!is.na(timeCol) && is.null(MoveR::listGet(df, timeCol))) {
        return(
          paste0(
            "[timeCol] argument [",
            timeCol,
            "] is not found in the provided dataset, [timeCol] might be misspelled"
          )
        )
      }
    }
    if (is.null(speedCol)) {
      return("[speedCol] argument is missing")
    }
    if (is.null(minSpeed)) {
      return(
        "[minSpeed] argument is missing: impossible to determine whether the particle is active or not without a minimum speed treshold"
      )
    }
    if (is.null(imgHeight)) {
      return(
        "[imgHeight] argument is missing, the height of the image (i.e., resolution) is needed to flip y coordinates"
      )
    }
    if (is.null(Bstate)) {
      return(
        "[Bstate] argument is missing, a column containing behavioral state information is needed to find the specified pattern"
      )
    }
    if (is.null(pattern)) {
      return(
        "[pattern] argument is missing, a regular expression is needed to specify which part of the tracklets have to be extracted"
      )
    }
    if (is.null(refDat)) {
      return(
        "[refDat] argument is missing, the function need a reference matrix to retrieve the location of the given particle"
      )
    }
    if (is.null(distTraveled)) {
      return(
        "[distTraveled] argument is missing, a column containing the distance traveled by the particles is needed to compute net square displacement"
      )
    }
    if (is.null(turnAngle)) {
      return(
        "[turnAngle] argument is missing, a column containing the values of particles' turning angle is needed to compute net square displacement"
      )
    }
    if (is.null(Tstep)) {
      return("[Tstep] argument is missing")
    }
    if (!identical(customFunc, NA) && is.null(customFunc)) {
      return("[customFunc] argument is missing, a customFunc is needed to compute metric")
    }
    if (!identical(trackDat, NA)) {
      if (!identical(trackDat, NA) && !inherits(trackDat, "tracklets")) {
        return(
          "Invalid 'tracklets' object, please import your data with the 'read' functions included in the MoveR package or use `trackletsClass()` function to turn your data into 'tracklets' object"
        )
      }
      if (!is.na(Bstate) &&
          is.null(unlist(lapply(trackDat, function(x) {
            MoveR::listGet(x, Bstate)
          })))) {
        return(
          "Bstate column is misspelled or is missing from tracklet list, a column containing behavioral state information is needed to find the specified pattern"
        )
      }
      if (!is.na(distTraveled) &&
          is.null(unlist(lapply(trackDat, function(x) {
            MoveR::listGet(x, distTraveled)
          })))) {
        return("[distTraveled] is not found in [trackdat], [distTraveled] might be misspelled")
      }
      if (!is.na(turnAngle) &&
          is.null(unlist(lapply(trackDat, function(x) {
            MoveR::listGet(x, turnAngle)
          })))) {
        return("[turnAngle] is not found in [trackdat], [turnAngle] might be misspelled")
      }
      if (!is.na(timeCol) &&
          is.null(unlist(lapply(trackDat, function(x) {
            MoveR::listGet(x, timeCol)
          })))) {
        return("[timeCol] is not found in [trackdat], [timeCol] might be misspelled")
      }
    }
  }
