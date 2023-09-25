#' @title Compute Mean Square Displacement (MSD).
#'
#' @description Given a list of data frames containing tracking informations and including the value of turning angles,
#' distance traveled and activity states (either 1 or 0 for active or inactive state respectively), this function compute the mean square displacement value
#' corresponding to a measure of the deviation of a particle's position according to a reference position over time. Computation can be performed
#' through the use of the equations derived from Kareiva & Shigesada (1983) or Turchin (2015). In a nutshell the Turchin's equation for MSD is a simplified version of the 
#' Kareiva & Shigesada' equation, assuming symmetric turning angles. 
#'
#'
#' @param trackDat A list of data frame containing tracking informations for each tracklet.
#'
#' @param turnAngle A character string indicating the name of the variable specifying the turning angles over each trajectories.
#'
#' @param distTraveled A character string indicating the name of the variable specifying the distance traveled by the particles.
#'
#' @param activityStates A character string indicating the name of the variable specifying activity state (coded as 1 or 0 for active and inactive state, respectively).
#'
#' @param method A character string indicating the method used to compute the net square displacement, either "Turchin" or "KS" to use the computation derived from the equations of Turchin's book 2015 edition (corrected version of the chapter 5.3 from 1998) or from Kareiva & Shigesada (1983) (default = "Turchin").
#'
#'
#' @return this function returns the Mean Square Displacement (MSD) value according to the equations derived from Turchin (2015) or Kareiva & Shigesada (1983).
#'
#' @author Quentin PETITJEAN, Vincent CALCAGNO
#'
#' @references
#'  \itemize{
#'          \item{Kareiva, P.M., Shigesada, N., (1983). Analyzing insect movement as correlated random walk. Oecologia 56,234–238. \href{https://doi.org/10.1007/BF00379695}{https://doi.org/10.1007/BF00379695}}
#'          \item{Turchin, P., (2015). Quantitative Analysis of Movement: Measuring and Modeling Population Redistribution in Animals and Plants. Beresta Books, Storrs, Connecticut.}
#'          }
#'
#' @examples
#'
#' # simulate a correlated random walk with known parameter to verify Turchin D computation
#' ## specify some parameters
#' nn = 1000
#' stepLength = 1
#' angularErrorSd = pi / 4
#'
#' ## create a function to simulate correlated random walk
#' myccrw <- function() {
#'   disps = runif(nn, min = 0, max = 2 * stepLength)
#'   angulos = runif(nn, min = -angularErrorSd, max = angularErrorSd)
#'   lesx = rep(0, nn)
#'   lesy = rep(0, nn)
#'   curangle = runif(1, min = -pi, max = pi)
#'   for (t in 2:nn) {
#'     curangle = curangle + angulos[t]
#'     lesx[t] = lesx[t - 1] + sin(curangle)
#'     lesy[t] = lesy[t - 1] + cos(curangle)
#'   }
#'   return(data.frame(
#'     x.pos = lesx + 250,
#'     y.pos = lesy + 250,
#'     frame = 1:nn
#'   ))
#' }
#'
#' # simulated 30 tracklets
#' nbr = 30
#' simus <- lapply(1:nbr,  function(rrr)
#'   myccrw())
#'
#' # take a look at the simulated data
#' MoveR::drawTracklets(simus)
#'
#' # Manually compute diffusion coefficient through time over all tracklets
#' arda = sapply(1:nbr, function(t) {
#'   ff = simus[[t]]
#'   mimi = sapply(1:length(ff$x.pos), function(d)
#'     ((ff$x.pos[d] - 250) ^ 2 + (ff$y.pos[d] - 250) ^ 2))
#'   return(mimi)
#' })
#' mimiz = apply(arda, 1, function(x) mean(x))
#'
#' # plot it
#' plot(
#'   x = 1:nn,
#'   mimiz,
#'   ylim = c(0, 30000),
#'   xlab = "Number of steps",
#'   ylab = "mean MSD"
#' )
#'
#' # compute the needed metrics on simulated tracklets
#' simComp <-
#'   MoveR::analyseTracklets(
#'     simus,
#'     customFunc = list(
#'       ## compute turning angle in radians over each tracklet (a modulus present within the MoveR package)
#'       turnAngle = function(x)
#'         MoveR::turnAngle(
#'           x,
#'           timeCol = "frame",
#'           unit = "radians",
#'           scale = 1
#'         ),
#'       ## compute distance traveled
#'       distTraveled = function(x)
#'         MoveR::distTraveled(x, step = 1),
#'       ## add behavioral states (consider as active all the time)
#'       activity = function(x)
#'         rep(1, nrow(x))
#'     )
#'   )
#'
#' MSD <- MoveR::MSD(
#'   simComp,
#'   turnAngle = "turnAngle",
#'   distTraveled = "distTraveled",
#'   activityStates = "activity",
#'   method = "Turchin"
#' )
#' 
#' # retrieve the total number of steps to add the computed MSD trend to the plot
#' nsteps <- nn * nbr
#' abline(0, MSD/nsteps , col = "red")
#'
#' @export

MSD <-
  function(trackDat,
           turnAngle = NULL,
           distTraveled = NULL,
           activityStates = NULL,
           method = c("Turchin", "KS")) {
    if (length(method) > 1) {
      method = "KS"
      warning(
        "No method selected to compute the mean square displacement (MSD), default method is ['Turchin']"
      )
    }
    trackdatL <- MoveR::convert2List(trackDat)
    if (is.null(turnAngle) |
        is.null(MoveR::listGet(trackdatL, turnAngle))) {
      stop(
        "turnAngle argument is missing or misspelled, a column containing the values of particles' turning angle is needed to compute net square displacement"
      )
    } else {
      turnAngle <- trackdatL[[turnAngle]]
    }
    if (is.null(distTraveled) |
        is.null(MoveR::listGet(trackdatL, distTraveled))) {
      stop(
        "distTraveled argument is missing or misspelled, a column containing the distance traveled by the particles is needed to compute net square displacement"
      )
    } else {
      distTraveled <- trackdatL[[distTraveled]]
    }
    if (is.null(activityStates) |
        is.null(MoveR::listGet(trackdatL, activityStates))) {
      warning(
        "activityStates argument is missing or misspelled, particles will be assumed active all the time"
      )
      ## create vector with 1 (active) everywhere
      activityStates <-
        rep(1, unique(unlist(lapply(
          trackdatL, length
        ))))
    } else {
      activityStates <- trackdatL[[activityStates]]
    }
    # identify active state
    activeParts <- which(activityStates == 1)
    # compute the mean sinus of the turning angle
    Msin <-
      mean(sin(turnAngle[activeParts]), na.rm = T)
    # compute the mean cosinus of the turning angle
    Mcos <-
      mean(cos(turnAngle[activeParts]), na.rm = T)
    # compute the mean distance traveled for over actives states
    MDist <-
      mean(distTraveled[activeParts], na.rm = T)
    # compute the mean squared distance traveled for over actives states
    Mdistsquared <-
      mean(distTraveled[activeParts] ^ 2, na.rm = T)
    # compute the mean activity
    act <- length(which(activityStates == 1)) / ifelse(NA %in%
                                                         activityStates,
                                                       length(activityStates[-c(which(is.na(activityStates)))]),
                                                       length(activityStates))
    # specify a power function
    P <- function(a, b) a ^ b
    # retrieve the total number of states (including all particles and time) and correct it by the activity rate
    nsteps <- length(activityStates) * act
    if (method == "Turchin") {
      # compute mean square displacement according to Turchin's book 2015 edition
      MSD <- Mdistsquared * nsteps + 2 * P(MDist, 2) *
        Mcos / P(1 - Mcos, 2) *
        (P(Mcos, nsteps / 2) - nsteps * Mcos + nsteps - 1)
      
    } else if (method == "KS") {
      # compute mean square displacement according to Kareiva & Shigesada 1983
      alpha <- atan(Msin / Mcos)
      MSD <- Mdistsquared * nsteps + 2 * Power(MDist, 2) *
        (nsteps * (Mcos - P(Msin, 2) - P(Mcos, 2)) - Mcos) / (P(Msin, 2) + P(1 - Mcos, 2)) +
        (P(P(Msin, 2) + Mcos, ((nsteps + 1) / 2)) + 2 * P(Msin, 2)) / (P(P(Msin, 2) + P(1 - Mcos, 2), 2)) *
        ((P(1 - Mcos, 2) - P(Msin, 2)) * cos(alpha * (nsteps + 1)) - 2 * Msin * (1 - Mcos) * sin(alpha * (nsteps + 1)))
    }
    return(MSD)
  }
