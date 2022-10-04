#' @title Compute the Net square displacement (Turchin 1998)
#'
#' @description Given a list of data frames containing tracking informations and including the value of turning angles,
#' distance traveled and behavioral states (either active or inactive), this function compute the Net square displacement value
#' used to infer populations spread according to Turchin (1998).
#'
#'
#' @param trackDat A list of data frame containing tracking informations for each fragment, including a vector
#' containing behavioral patterns (e.g., behavioral states, location in areas).
#'
#' @param turnAngle A character string indicating the name of the variable specifying the turning angles over each trajectories.
#'
#' @param distTraveled A character string indicating the name of the variable specifying the distance traveled by the particles.
#'
#' @param behavStates A character string indicating the name of the variable specifying behavioral states.
#'
#' @return this function returns the Net square displacement value used to infer populations spread according to Turchin (1998).
#'
#' @authors Quentin PETITJEAN
#'
#' @references
#' Turchin, P. (1998). Quantitative Analysis of Movement: Measuring and Modeling Population Redistribution in Animals and Plants. Sinauer.
#'
#' @examples
#'
#' #TODO
#'

turchinD <-
  function(turnAngle = NULL,
           distTraveled = NULL,
           behavStates = NULL) {
    # retrieve var1 and var2 from the dataset and transform them if needed
    trackdatL <- MoveR::convert2list(trackDat)
    turnAngle <- trackdatL[["turnAngle"]]
    distTraveled <- trackdatL[["distTraveled"]]
    behavStates <- trackdatL[["behavStates"]]
    
    # compute the mean sinus of the turning angle
    Msin <-
      mean(sin(turnAngle[which(behavStates == "active")]), na.rm = T)
    
    # compute the mean cosinus of the turning angle
    Mcos <-
      mean(cos(turnAngle[which(behavStates == "active")]), na.rm = T)
    
    # compute the mean distance traveled for over actives states
    MDist <-
      mean(distTraveled[which(behavStates == "active")], na.rm = T)
    
    # compute the mean squared distance traveled for over actives states
    Mdistsquared <-
      mean(distTraveled[which(behavStates == "active")] ^ 2, na.rm = T)
    
    # Compute corrected net square displacement according to Turchin 1998
    # the correction depends on the activity rate as follow: activityrate * (m2 + 2m1 * phi / (1-phi))
    D <- 1 * act * (Mdistsquared + 2 * MDist * Mcos / (1 - Mcos))
    return(D)
  }
