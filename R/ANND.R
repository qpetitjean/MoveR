#' @title Compute Average Nearest Neighbour Distance (ANND) among fragments
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
#' specifying the time window on which the computation is performed (default is null, meaning the computation will be performed on the whole timeline).
#'
#' @param sampling A numeric value expressed in the timeline unit and specifying the subsampling step used to
#' to perform the computation (it allow to make computation faster). In other words, it determine the resolution of the
#' returned results (e.g., a value of 5000 mean that values will be computed every 5000 time unit).
#'
#' @param bootn A numeric value corresponding to the number of bootstrap sampling used to compute studentize 95% confidence interval (default = 0, meaning bootstrap will be not performed).
#'
#' @return this function returns a list containing three elements:
#'  \itemize{
#'    \item{"ANND": }{a data frame containing 6 columns:
#'       \itemize{
#'          \item{"ANNDmean": }{the averaged Nearest Neighbour Distance for each sampling step.}
#'          \item{"97.5%": }{the upper limit of the confidence interval (97.5%).}
#'          \item{"2.5%": }{the lower limit of the confidence interval (97.5%).}
#'          \item{"ANNDsd": }{the standard deviation of the Average Nearest Neighbour Distance for each sampling step.}
#'          \item{"nInd": }{the number of particules used to compute the Averaged Nearest Neighbour Distance for each sampling step.}
#'          \item{"timeCol": }{the sampling step, expressed in the same unit that the timeCol argument.}
#'       }}
#'    
#'    \item{"RawND": }{a list containing the raw neighbour distance computed among each particles across each time unit:
#'       \itemize{
#'          \item{"timeCol_index": }{a list of vector corresponding to the fragment identity containing the distance to each detected particles in the time unit.}
#'       }
#'    }
#'     \item{"BootSampling": }{a list of lists corresponding to each time unit sampled during the bootstrap and containing: 
#'       \itemize{
#'          \item{"sampledFrags": }{the identity of the fragments sampled at each bootstrap sampling (the length of the list is equal to bootn argument).}
#'          \item{"sampledValues": }{the nearest neighbour distance values for each sampled fragment at each bootstrap sampling (the length of the list is equal to bootn argument).}
#'       }}
#' }
#'
#' @authors Quentin PETITJEAN
#'
#'
#' @examples
#'
#'# generate some dummy fragments
#'## start to specify some parameters to generate fragments
#'Fragn <- 500 # the number of fragment to simulate
#'FragL <- 100:1000 # the length of the fragments or a sequence to randomly sample fragment length
#'
#'fragsList <- stats::setNames(lapply(lapply(seq(Fragn), function(i)
#'  trajr::TrajGenerate(sample(FragL, 1), random = TRUE, fps = 1)), function(j)
#'    data.frame(
#'      x.pos = j$x - min(j$x),
#'      y.pos = j$y - min(j$y),
#'      frame = j$time
#'    )), seq(Fragn))
#'
#'# compute the ANND
#'ANNDRes <- ANND(fragsList, timeCol = "frame", sampling = 100, scale = 1, bootn = 500)
#'
#'# the results can be retrieved by accessing the "ANND" data frame
#'str(ANNDRes[["ANND"]])
#'
#'# And hence you can plot it against the timeCol (here "frame")
#'par(mfrow = c(1, 1))
#'plot(
#'  ANNDRes[["ANND"]]$ANNDmean ~ ANNDRes[["ANND"]]$frame,
#'  type = "l",
#'  ylim = c(0, signif(max(
#'    ANNDRes[["ANND"]]$ANNDmean, na.rm = T
#'  ), digits = 3)),
#'  col = "red",
#'  xlab = "Time (frame)",
#'  ylab = "Average Nearest Neighbour Distance (ANND) and 95% CI"
#')
#'lines(ANNDRes[["ANND"]]$`97.5%` ~ ANNDRes[["ANND"]]$frame)
#'lines(ANNDRes[["ANND"]]$`2.5%` ~ ANNDRes[["ANND"]]$frame)
#'polygon(
#'  x = c(ANNDRes[["ANND"]]$frame, rev(ANNDRes[["ANND"]]$frame)),
#'  y = c(ANNDRes[["ANND"]]$`2.5%`, rev(ANNDRes[["ANND"]]$`97.5%`)),
#'  col = rgb(1, 0, 0, 0.1),
#'  border = NA
#'  ,
#'  density = NA
#')
#'# It is also possible to add the number of individual on which ANND was computed across time on the plot
#'par(new = TRUE)
#'plot(
#'  NULL,
#'  yaxt = "n",
#'  ylab = "",
#'  xlab = "",
#'  axes = FALSE,
#'  xlim = c(0, max(ANNDRes[["ANND"]]$frame)),
#'  ylim = c(0, signif(max(ANNDRes[["ANND"]]$nInd, na.rm=T), digits = 3)),
#')
#'axis(side=4, at = c(0, round(max(ANNDRes[["ANND"]]$nInd, na.rm=T))/4, round(max(ANNDRes[["ANND"]]$nInd, na.rm=T))/2 , 
#'                    round(max(ANNDRes[["ANND"]]$nInd, na.rm=T))/4 + round(max(ANNDRes[["ANND"]]$nInd, na.rm=T))/2, 
#'                    round(max(ANNDRes[["ANND"]]$nInd, na.rm=T))))
#'
#'lines(ANNDRes[["ANND"]]$nInd ~ ANNDRes[["ANND"]]$frame, col = "purple")
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
    # because they are sampled at the same time unit
    maxId <- length(WhoWhen)
    # in case there is duplicated values at the same time in whowhen
    if(max(unlist(lapply(WhoWhen, function(x) nrow(x))), na.rm = T) > 1){
      WhoWhentemp <- lapply(names(WhoWhen), function(x) WhoWhen[[x]][-which(duplicated(WhoWhen[[x]][["x.pos"]])),])
      names(WhoWhentemp) <- names(WhoWhen)
      WhoWhen <- WhoWhentemp
    }
    # in case there is only one fragment detected at t
    if(maxId <= 1){ 
      ND <- NA
      names(ND) <- names(WhoWhen)
      NND <- NA
      names(NND) <- names(WhoWhen)
      # compute the mean distance to the nearest neighbour for the present time t (ANND - average nearest neighbour distance)
      ANNDmean <- NA
      ANNDsd <- NA
      warning("for time unit = ", t, ": only 1 or no fragment detected, the returned neighbour distance is NA")
      } else{
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
      }
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
      BootSampling[[paste(timeCol, as.character(t), sep = "_")]][["sampledFrags"]] <- bootsamplesL
      BootSampling[[paste(timeCol, as.character(t), sep = "_")]][["sampledValues"]] <- bootsamplesVal
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
      RawND = RAWND,
      BootSampling = BootSampling))
  } else{
    return(list(ANND = ANNDRes, RawND = RAWND))
  }
}