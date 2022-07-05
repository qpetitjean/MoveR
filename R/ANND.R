#' @title Compute Average Nearest Neighbour Distance (ANND) among fragments
#'
#'
#' @description Given a list of data frames containing tracking informations for each fragment (including the timeline)
#' this function returns the Average Nearest Neighbour Distance among fragments along a specified timeline. Also, 
#' if bootn parameter is above 0, the function compute studentize 95 % CI using bootstrapping method
#'
#'
#' @param trackDat A list of data frame containing tracking informations for each fragment (including a timeline).
#'
#' @param timeCol A character string corresponding to the name of the column containing Time information (e.g., "frame").
#'
#' @param Tinterval A vector containing two numeric values expressed in the timeline unit and
#' specifying the time interval on which the computation is performed
#' (default is null, meaning the computation will be performed on the whole timeline).
#'
#' @param sampling A numeric value expressed in the timeline unit and specifying the subsampling step used to
#' to perform the computation (it allow to make computation faster). In other words, it determine the resolution of the
#' returned results (e.g., a value of 5000 mean that values will be computed every 5000 frames).
#'
#' @param bootn A numeric value corresponding to the number of bootstrap sampling to compute studentize 95% confidence interval.
#'
#' @return this function returns a list containing three sublist, the first sublist contains a dataframe with
#' bootstrap results (mean ANND, CI 97.5%, CI 2.5%, Sd, number of detected individual 
#' and the resampled timeline according to timeCol argument),
#' the second sublist contains raw neighbour distances computed across each time unit. 
#' The third list returns two sublists, the first one containing, containing dataframes 
#' with the fragments sampled over the bootstrap iterations, while the second contains the values of the nearest
#' neighbour distance for each of the sampled fragments.
#'
#'
#' @authors Quentin PETITJEAN
#'
#'
#' @examples
#'
#' #TODO
#'
#' @export

ANND <- function(trackDat,
                 timeCol = NULL,
                 Tinterval = NULL,
                 sampling = NULL,
                 scale = NULL,
                 bootn = 0) {
  # define a timeline according to the max duration of the video and a step parameter.
  # Nearest neighbour distance will be computed at each point of the timeline and then averaged
  if (is.null(timeCol) |
      !(timeCol %in% unlist(lapply(trackDat, names)))) {
    stop(
      "timeCol argument is missing or is not found in the provided dataset, timeCol might be misspelled"
    )
  }
  if (is.null(scale)) {
    warning("scale argument is missing, default is 1/1")
    scale = 1 / 1
  }
  if (is.null(Tinterval)) {
    # define the timeline
    timeline <- seq(min(unlist(lapply(trackDat, function (w)
      min(w[timeCol], na.rm = T))), na.rm = T),
      max(unlist(lapply(trackDat, function (w)
        max(w[timeCol], na.rm = T))), na.rm = T), by = sampling)
  } else {
    # define the timeline
    timeline <-
      seq(
        from = round(Tinterval[1]),
        to = round(Tinterval[2]),
        by = sampling
      )
    timeline[length(timeline) + 1] <- Tinterval[2]
    timeline[which(timeline == 0)] <- 1
    timeline <- timeline[!duplicated(timeline)]
  }
  if (is.null(sampling)) {
    sampling = 1
    warning(
      "sampling argument is NULL, default value is 1 meaning that ANND will be computed for each time unit"
    )
  }
  # initialize progress bar
  total = length(timeline)
  pb <-
    progress::progress_bar$new(format = "sample processing [:bar] :current/:total (:percent)", total = total)
  pb$tick(0)
  # initialize result Df
  if (bootn > 0) {
    ANNDRes <-
      data.frame(matrix(NA, nrow = length(timeline), ncol = 6))
    colnames(ANNDRes) <-
      c("ANNDmean", "97.5%", "2.5%", "ANNDsd", "nInd", timeCol)
    BootSampling <- list()
  } else{
    ANNDRes <-
      data.frame(matrix(NA, nrow = length(timeline), ncol = 4))
    colnames(ANNDRes) <-
      c("ANNDmean", "ANNDsd", "nInd", timeCol)
  }
  ANNDRes[[timeCol]] <- timeline
  # initialize Raw result Df
  RAWND <- list()
  for (t in timeline) {
    # select the fragment that are detected in the selected timeline part and
    # Cut them according the selected part of the timeline
    WhoWhen <- cutFrags(trackDat, function (x)
      x[[timeCol]] %in% t)
    # identify the number of fragments present at t
    # it assume that each fragment is independant from other
    # because they are sampled at the same time unit (frame)
    maxId <- length(WhoWhen)
    #in case there is duplicated values at the same time in whowhen
    if(max(unlist(lapply(WhoWhen, function(x) nrow(x))), na.rm = T) > 1){
      WhoWhentemp <- lapply(names(WhoWhen), function(x) WhoWhen[[x]][-which(duplicated(WhoWhen[[x]][["x.pos"]])),])
      names(WhoWhentemp) <- names(WhoWhen)
      WhoWhen <- WhoWhentemp
    }
    # compute distance to each neighbour for each present fragment (ND - neighbour distance)
    ND <-
      lapply(seq(maxId), function(x) {
        array(unlist(lapply(seq(maxId)[-x], function(y) {
          sqrt((WhoWhen[[x]]["x.pos"] - WhoWhen[[y]]["x.pos"]) ^ 2 + (WhoWhen[[x]]["y.pos"] -
                                                                        WhoWhen[[y]]["y.pos"]) ^ 2) * scale
        })), dimnames = list(names(WhoWhen)[-x]))
      })
    
    names(ND) <- names(WhoWhen)
    # compute the distance to the nearest neighbour for each present fragment (NND - nearest neighbour distance)
    NND <- array(unlist(lapply(seq(length(
      ND
    )), function(x) {
      min(ND[[x]], na.rm = T)
    })), dimnames = list(names(WhoWhen)))
    # compute the mean distance to the nearest neighbour for the present time t (ANND - average nearest neighbour distance)
    ANNDmean <- mean(NND)
    ANNDsd <- sd(NND)
    if (bootn > 0) {
      # create a matrix to store the names of the sampled fragments
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
      # Match names of the sampled fragments with computed values
      bootsamplesVal <-
        lapply(seq(bootn), function(x) {
          array(unlist(lapply(seq(
            length(bootsamples[, x])
          ), function(y) {
            NND[match(bootsamples[, x][y], names(NND))]
          })), dim = c(length(bootsamples[, x]), 1))
        })
      names(bootsamplesVal) <- seq(bootn)
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
      bootsamplesL <- lapply(seq(bootn), function(x) {bootsamples[,x]})
      names(bootsamplesL) <- seq(bootn)
      BootSampling[[paste(timeCol, as.character(t), sep = "_")]][["sampled.Frags"]] <- bootsamplesL
      BootSampling[[paste(timeCol, as.character(t), sep = "_")]][["sampled.Values"]] <- bootsamplesVal
    }
    # Append the result to the result DF
    ANNDRes[which(ANNDRes[[timeCol]] == t), "ANNDmean"] <- ANNDmean
    ANNDRes[which(ANNDRes[[timeCol]] == t), "ANNDsd"] <- ANNDsd
    ANNDRes[which(ANNDRes[[timeCol]] == t), "nInd"] <- maxId
    # create a list with raw neighbour distance computed for each fragment at each time unit
    RAWND[[paste(timeCol, as.character(t), sep = "_")]] <- ND
    # progress bar
    pb$tick(1)
  }
  if (bootn > 0) {
    return(list(
      ANND = ANNDRes,
      Raw.ND = RAWND,
      BootSampling = BootSampling))
  } else{
    return(list(ANND = ANNDRes, Raw.ND = RAWND))
  }
}