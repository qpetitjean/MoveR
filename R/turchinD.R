#' @title Compute the net square displacement (Turchin 1998)
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
#' @return this function returns the net square displacement value used to infer populations spread according to Turchin (1998).
#'
#' @authors Quentin PETITJEAN
#'
#' @references
#' Turchin, P. (1998). Quantitative Analysis of Movement: Measuring and Modeling Population Redistribution in Animals and Plants. Sinauer.
#'
#' @examples
#'
#'# simulate a correlated random walk with known parameter to verify Turchin D computation
#'## specify some parameters
#'n = 1000
#'stepLength = 2
#'angularErrorSd = 0.5
#'linearErrorSd = 0.2
#'angularErrorDist = stats::rnorm(n, sd = angularErrorSd)
#'linearErrorDist = stats::rnorm(n, sd = linearErrorSd)
#'
#'## simulate a trajectory
#'sim <- trajr::TrajGenerate(
#'  n = n,
#'  random = TRUE,
#'  stepLength = stepLength,
#'  angularErrorSd = angularErrorSd,
#'  angularErrorDist = function(x) angularErrorDist,
#'  linearErrorSd = linearErrorSd,
#'  linearErrorDist = function(x) linearErrorDist,
#'  fps = 1)
#'
#'## convert it to a data frame to allow MoveR computation
#'sim <- data.frame(
#'  x.pos = sim[["x"]] - min(sim[["x"]]),
#'  y.pos = sim[["y"]] - min(sim[["y"]] ),
#'  frame = sim[["time"]]
#')
#'
#'# take a look at the simulated data (here we use list(sim) because the function expect a list of trajectories)
#'MoveR::drawFrags(list(sim), imgRes = c(500,500))
#'
#'# compute the needed metric on the simulated dataset (here we use list(sim) because the function expect a list of trajectories)
#'simComp <-
#'  MoveR::analyseFrags(
#'    list(sim),
#'    customFunc = list(
#'      ## compute turning angle in radians over each fragment (a modulus present within the MoveR package)
#'      TurnAngle = function(x)
#'        MoveR::turnAngle(x, unit = "radians"),
#'      ## compute distance traveled
#'      distTraveled = function(x)
#'        MoveR::distTraveled(x, step = 1)
#'    )
#'  )
#'
#'# add behavioral state (consider as active all the time)
#'simComp[[1]]["behavStates"] <- "active"
#'
#'# compute the Turchin net square displacement from the simulated data
#'D <- MoveR::turchinD(
#'  simComp,
#'  turnAngle = "TurnAngle",
#'  distTraveled = "distTraveled",
#'  behavStates = "behavStates"
#')
#'
#'# check the net square displacement value
#'D
#'
#' @export

turchinD <-
  function(trackDat,
           turnAngle = NULL,
           distTraveled = NULL,
           behavStates = NULL) {
    # retrieve var1 and var2 from the dataset and transform them if needed
    trackdatL <- MoveR::convert2list(trackDat)
    turnAngle <- trackdatL[[turnAngle]]
    distTraveled <- trackdatL[[distTraveled]]
    behavStates <- trackdatL[[behavStates]]
    
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
    
    # compute the mean activity
    act <-
      length(which(behavStates == "active")) / ifelse(NA %in% behavStates,
                                                      length(behavStates[-c(which(is.na(behavStates)))]),
                                                      length(behavStates))
    
    # Compute corrected net square displacement according to Turchin 1998
    # the correction depends on the activity rate as follow: activityrate * (m2 + 2m1 * phi / (1-phi))
    D <- 1 * act * (Mdistsquared + 2 * MDist * Mcos / (1 - Mcos))
    return(D)
  }
