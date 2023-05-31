#' @title Compute Average Nearest Neighbour Distance (ANND) among tracklets.
#'
#' @description Given a list of data frames containing tracking information for each tracklet (including the timeline)
#' this function returns the Average Nearest Neighbour Distance among tracklets along a specified timeline. Also, 
#' if bootn parameter is above 0, the function compute studentize 95 % CI using bootstrapping method.
#'
#'
#' @param trackDat A list of data frame containing tracking information for each tracklet (including a timeline).
#'
#' @param timeCol A character string corresponding to the name of the column containing Time information (e.g., "frame").
#'
#' @param Tinterval A vector containing two numeric values expressed in the timeline unit and
#' specifying the time window on which the computation is performed (default is null, meaning the computation will be performed on the whole timeline).
#'
#' @param sampling A numeric value expressed in the timeline unit and specifying the subsampling step used to
#' to perform the computation (it allow to make computation faster). In other words, it determine the resolution of the
#' returned results (e.g., a value of 5000 mean that values will be computed every 5000 time unit).
#'
#' @param bootn A numeric value corresponding to the number of bootstrap sampling used to compute studentize 95% confidence interval (default = 0, meaning bootstrap will be not performed).
#'
#' @param progress A logical value (i.e., TRUE or FALSE) indicating whether a progress bar should be displayed to inform process progression (default = TRUE).
#'
#' @return this function returns a list containing three elements:
#'  \itemize{
#'    \item{"ANND": }{a data frame containing 6 columns:
#'       \itemize{
#'          \item{"ANNDmean": }{the averaged Nearest Neighbour Distance for each sampling step.}
#'          \item{"97.5%": }{the upper limit of the confidence interval (97.5%).}
#'          \item{"2.5%": }{the lower limit of the confidence interval (2.5%).}
#'          \item{"ANNDsd": }{the standard deviation of the Average Nearest Neighbour Distance for each sampling step.}
#'          \item{"nInd": }{the number of particules used to compute the Averaged Nearest Neighbour Distance for each sampling step.}
#'          \item{"timeCol": }{the timeline according to timeCol and sampling arguments.}
#'       }}
#'    
#'    \item{"RawND": }{a list containing the raw neighbour distance computed among each particles across each time unit:
#'       \itemize{
#'          \item{"timeCol_index": }{a list of vector corresponding to the tracklet identity containing the distance to each detected particles in the time unit.}
#'       }
#'    }
#'     \item{"BootSampling": }{a list of lists corresponding to each time unit sampled during the bootstrap and containing: 
#'       \itemize{
#'          \item{"sampledTracks": }{the identity of the tracklet sampled at each bootstrap iteration (the length of the list is equal to bootn argument).}
#'          \item{"sampledValues": }{the nearest neighbour distance values for each sampled tracklet at each bootstrap iteration (the length of the list is equal to bootn argument).}
#'       }}
#' }
#'
#' @author Quentin PETITJEAN
#'
#'
#' @examples
#'
#' set.seed(2023)
#' # generate some dummy tracklets
#' ## start to specify some parameters to generate tracklets
#' TrackN <- 50 # the number of tracklet to simulate
#' TrackL <-
#'   100:1000 # the length of the tracklets or a sequence to randomly sample tracklet length
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
#' # compute the ANND with a sampling of 100 time unit
#' ANNDRes <-
#'   MoveR::ANND(
#'     TrackList,
#'     timeCol = "frame",
#'     sampling = 100,
#'     scale = 1,
#'     bootn = 500
#'   )
#' 
#' # the results can be retrieved by accessing the "ANND" data frame
#' str(ANNDRes[["ANND"]])
#' 
#' # And hence you can plot it against the timeCol (here "frame")
#' ## remove the NA to avoid problem when plotting the confidence interval
#' ANNDRes[["ANND"]] <-
#'   ANNDRes[["ANND"]][!is.na(ANNDRes[["ANND"]]$ANNDmean), ]
#' plot(
#'   ANNDRes[["ANND"]]$ANNDmean ~ ANNDRes[["ANND"]]$frame,
#'   type = "l",
#'   ylim = c(0, signif(max(
#'     ANNDRes[["ANND"]]$`2.5%`, na.rm = T
#'   ), digits = 3)),
#'   col = "red",
#'   xlab = "Time (frame)",
#'   ylab = "Average Nearest Neighbour Distance (ANND) and 95% CI"
#' )
#' lines(ANNDRes[["ANND"]]$`97.5%` ~ ANNDRes[["ANND"]]$frame)
#' lines(ANNDRes[["ANND"]]$`2.5%` ~ ANNDRes[["ANND"]]$frame)
#' polygon(
#'   x = c(ANNDRes[["ANND"]]$frame, rev(ANNDRes[["ANND"]]$frame)),
#'   y = c(ANNDRes[["ANND"]]$`2.5%`, rev(ANNDRes[["ANND"]]$`97.5%`)),
#'   col = rgb(1, 0, 0, 0.1),
#'   border = NA,
#'   density = NA
#' )
#' 
#' # It is also possible to add the number of particles on which ANND was computed across time on the plot
#' # here we can see that the less particles detected, the higher the ANND which make sense.
#' par(new = TRUE)
#' plot(
#'   NULL,
#'   yaxt = "n",
#'   ylab = "",
#'   xlab = "",
#'   axes = FALSE,
#'   xlim = c(0, max(ANNDRes[["ANND"]]$frame)),
#'   ylim = c(0, signif(max(
#'     ANNDRes[["ANND"]]$nInd, na.rm = T
#'   ), digits = 3)),
#' )
#' axis(side = 4,
#'      at = c(
#'        0,
#'       round(max(ANNDRes[["ANND"]]$nInd, na.rm = T)) / 4,
#'        round(max(ANNDRes[["ANND"]]$nInd, na.rm = T)) / 2 ,
#'        round(max(ANNDRes[["ANND"]]$nInd, na.rm = T)) / 4 + round(max(ANNDRes[["ANND"]]$nInd, na.rm =
#'                                                                        T)) / 2,
#'        round(max(ANNDRes[["ANND"]]$nInd, na.rm = T))
#'      ))
#' lines(ANNDRes[["ANND"]]$nInd ~ ANNDRes[["ANND"]]$frame, col = "purple")
#'
#' @export

ANND <- function(trackDat,
                 timeCol = NULL,
                 Tinterval = NULL,
                 sampling = NULL,
                 scale = NULL,
                 bootn = 0,
                 progress = TRUE) {
  # define a timeline according to the max duration of the video and a step parameter.
  # Nearest neighbour distance will be computed at each point of the timeline and then averaged
  if (is.null(timeCol) |
      !(timeCol %in% unlist(lapply(trackDat, names)))) {
    stop(
      "timeCol argument is missing or is not found in the provided dataset, timeCol might be misspelled"
    )
  }
  if (is.null(scale)) {
    warning("scale argument is missing, default is 1")
    scale = 1 / 1
  }
  if (is.null(sampling)) {
    sampling = 1
    warning(
      "sampling argument is NULL, default value is 1 meaning that ANND will be computed for each time unit"
    )
  }
  
  # define the timeline
  ## check the time step of the timeline across the dataset
  TimelineStep <- unique(unlist(lapply(trackDat, function (w)
    apply(w[timeCol], 2, function(x) signif(diff(x), 4))
  )))
  ## in case the time step is not constant use the minimum value and print a warning message
  if(length(TimelineStep) == 1){
    TimeLStep <- TimelineStep
  }else if(length(TimelineStep) > 1) {
    TimeLStep <- min(TimelineStep, na.rm = T)
    warning(
      "In timeCol : \n the time step is not constant across trackDat and returns the following values: ",
      TimelineStep,
      "\n here the function used ",
      min(TimelineStep, na.rm = T),
      ", but perhaps consider resampling the tracklets to better control the behavior of the function", 
      "\n see MoveR::resampTracklets()"
    )
  }
  if (is.null(Tinterval)) {
    # create the timeline
    timeline <- seq(min(unlist(lapply(trackDat, function (w)
      min(w[timeCol], na.rm = T))), na.rm = T),
      max(unlist(lapply(trackDat, function (w)
        max(w[timeCol], na.rm = T))), na.rm = T), by = TimeLStep)
    if(timeline[length(timeline)] != max(unlist(lapply(trackDat, function (w)
      max(w[timeCol], na.rm = T))), na.rm = T)){
      timeline[length(timeline)+1] <- max(unlist(lapply(trackDat, function (w)
        max(w[timeCol], na.rm = T))), na.rm = T)
    }
    # resample the timeline according to "sampling" parameter (computation will be made at these time values)
    Newtimeline <- seq(from = timeline[1],
                       to = timeline[length(timeline)],
                       by = sampling)
    
    if(Newtimeline[length(Newtimeline)] != timeline[length(timeline)]){
      Newtimeline[length(Newtimeline) + 1] <- timeline[length(timeline)]
    }
    Newtimeline[which(Newtimeline == 0)] <- 1
  } else {
    timeline <-
      seq(
        from = Tinterval[1] - ((Tstep - TimeLStep) / 2),
        to = Tinterval[2] + ((Tstep - TimeLStep) / 2),
        by = TimeLStep
      )
    
    # resample the timeline according to "sampling" parameter (computation will be made at these time values)
    Newtimeline <- seq(from = Tinterval[1],
                       to = Tinterval[2],
                       by = sampling)
    if(Newtimeline[length(Newtimeline)] != Tinterval[2]){
      Newtimeline[length(Newtimeline)+1] <- Tinterval[2]
    }
    Newtimeline[which(Newtimeline == 0)] <- 1
  }
  if(length(which(duplicated(Newtimeline))) > 0){
    Newtimeline <- Newtimeline[-which(duplicated(Newtimeline))]}
  
  # initialize progress bar
  if (isTRUE(progress)) {
  total = length(Newtimeline)
  pb <-
    progress::progress_bar$new(format = "Processing [:bar] :current/:total (:percent)", total = total)
  pb$tick(0)
  }
  # initialize result Df
  if (bootn > 0) {
    ANNDRes <-
      data.frame(matrix(NA, nrow = length(Newtimeline), ncol = 6))
    colnames(ANNDRes) <-
      c("ANNDmean", "97.5%", "2.5%", "ANNDsd", "nInd", timeCol)
    BootSampling <- list()
  } else{
    ANNDRes <-
      data.frame(matrix(NA, nrow = length(Newtimeline), ncol = 4))
    colnames(ANNDRes) <-
      c("ANNDmean", "ANNDsd", "nInd", timeCol)
  }
  ANNDRes[[timeCol]] <- Newtimeline
  # initialize Raw result Df
  RAWND <- list()
  for (t in Newtimeline) {
    # select the tracklets that are detected in the selected timeline part and
    # Cut them according the selected part of the timeline
    WhoWhen <- MoveR::cutTracklets(trackDat, function (x)
      x[[timeCol]] %in% t)
    # identify the number of tracklets present at t
    # it assume that each tracklet is independant from other
    # because they are sampled at the same time unit
    maxId <- length(WhoWhen)
    # in case there is duplicated values at the same time in whowhen
    if(max(unlist(lapply(WhoWhen, function(x) nrow(x))), na.rm = T) > 1){
      WhoWhentemp <-
        stats::setNames(lapply(names(WhoWhen), function(x)
          WhoWhen[[x]][-which(duplicated(WhoWhen[[x]][["x.pos"]])), ]), names(WhoWhen))
      WhoWhen <- WhoWhentemp
    }
    # in case there is only one tracklet detected at t
    if(maxId <= 1){ 
      ND <- NA
      names(ND) <- names(WhoWhen)
      NND <- NA
      names(NND) <- names(WhoWhen)
      # compute the mean distance to the nearest neighbour for the present time t (ANND - average nearest neighbour distance)
      ANNDmean <- NA
      ANNDsd <- NA
      warning("for time unit = ", "[", t, "]", ", only 1 or no tracklet detected, the returned neighbour distance is NA")
      } else{
    # compute distance to each neighbour for each present tracklet (ND - neighbour distance)
    ND <- stats::setNames(
      lapply(seq(maxId), function(x) {
        array(unlist(lapply(seq(maxId)[-x], function(y) {
          sqrt((WhoWhen[[x]]["x.pos"] - WhoWhen[[y]]["x.pos"]) ^ 2 + (WhoWhen[[x]]["y.pos"] -
                                                                        WhoWhen[[y]]["y.pos"]) ^ 2) * scale
        })), dimnames = list(names(WhoWhen)[-x]))
      }), names(WhoWhen))

    # compute the distance to the nearest neighbour for each present tracklet (NND - nearest neighbour distance)
    NND <- array(unlist(lapply(seq(length(
      ND
    )), function(x) {
      min(ND[[x]], na.rm = T)
    })), dimnames = list(names(WhoWhen)))
    
    # compute the mean distance to the nearest neighbour for the present time t (ANND - average nearest neighbour distance)
    ANNDmean <- mean(NND)
    ANNDsd <- sd(NND)
      }
    if (bootn > 0) {
      # create a matrix to store the names of the sampled tracklets
      bootsamples <-
        matrix(
          sample(
            names(NND),
            size = length(NND) * bootn,
            replace = T
          ),
          nrow = length(NND),
          ncol = bootn
        )
      # Match names of the sampled tracklets with computed values
      bootsamplesVal <- stats::setNames(
        lapply(seq(bootn), function(x) {
          array(unlist(lapply(seq(
            length(bootsamples[, x])
          ), function(y) {
            NND[match(bootsamples[, x][y], names(NND))]
          })), dim = c(length(bootsamples[, x]), 1))
        }), seq(bootn))
      ## compute the means
      UbWtd.mean <- unlist(lapply(seq(bootn), function(x) {
        mean(bootsamplesVal[[x]], na.rm = T)
      }))
      meanBoot <- mean(UbWtd.mean, na.rm = T)
      ## compute the sd
      UnWtd.sd <- unlist(lapply(seq(bootn), function(x) {
        sd(bootsamplesVal[[x]], na.rm = T)
      }))
      ## compute the estimates
      bootEstNum <- unlist(lapply(seq(bootn), function(x) {
        mean(bootsamplesVal[[x]], na.rm = T) - ANNDmean
      }))
      bootEstDenom <- unlist(lapply(seq(bootn), function(x) {
        sd(bootsamplesVal[[x]], na.rm = T) / sqrt(maxId)
      }))
      bootEst <- bootEstNum / bootEstDenom
      # compute the studentize CI 95%
      boot.ci.student <-
        data.frame(t(c((
          ANNDmean - quantile(
            bootEst,
            probs = c(0.975, 0.025),
            na.rm = T
          ) *
            ANNDsd / sqrt(maxId)
        ), ANNDmean, t
        )))
      # Append the result to the result DF
      ANNDRes[which(ANNDRes[[timeCol]] == t), "97.5%"] <-
        boot.ci.student$X97.5.
      ANNDRes[which(ANNDRes[[timeCol]] == t), "2.5%"] <-
        boot.ci.student$X2.5.
      # Append the sampling to BootSampling list
      bootsamplesL <- stats::setNames(lapply(seq(bootn), function(x) {bootsamples[,x]}), seq(bootn))
      BootSampling[[paste(timeCol, as.character(t), sep = "_")]][["sampledTracklets"]] <- bootsamplesL
      BootSampling[[paste(timeCol, as.character(t), sep = "_")]][["sampledValues"]] <- bootsamplesVal
    }
    # Append the result to the result DF
    ANNDRes[which(ANNDRes[[timeCol]] == t), "ANNDmean"] <- ANNDmean
    ANNDRes[which(ANNDRes[[timeCol]] == t), "ANNDsd"] <- ANNDsd
    ANNDRes[which(ANNDRes[[timeCol]] == t), "nInd"] <- maxId
    # create a list with raw neighbour distance computed for each tracklet at each time unit
    RAWND[[paste(timeCol, as.character(t), sep = "_")]] <- ND
    if (isTRUE(progress)) {
    # progress bar
    pb$tick(1)
    }
  }
  if (bootn > 0) {
    return(list(
      ANND = ANNDRes,
      RawND = RAWND,
      BootSampling = BootSampling))
  } else{
    return(list(ANND = ANNDRes, RawND = RAWND))
  }
}
