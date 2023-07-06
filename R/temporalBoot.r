#' @title Compute 95% studentized CI across tracklets and time.
#'
#' @description Given a list of data frames containing tracking informations for each tracklet (including the timeline)
#' and a custom function, this function perform the computation specified by the custom function across time and compute studentized 95% 
#' confidence interval (CI) by bootstrapping the results over the tracklets.
#'
#' @param trackDat A list of data frame containing tracking informations for each tracklet.
#'
#' @param timeCol A character string specifying the name of the timeline column.
#'
#' @param customFunc A function or a list of functions used to perform the computation across time.
#' NB: in case customFunc is a list of unnamed function it will try to retrieve their names by returning the first character string
#' following the function() call as the name of the results column.
#'
#' @param Tinterval A vector containing two numeric values expressed in the timeline unit and
#' specifying the time interval on which the computation is performed
#' (default is null, meaning the computation will be performed on the whole timeline).
#'
#' @param Tstep A numeric value expressed in the timeline unit and specifying the size of the
#' sliding window used to perform the computation (e.g., a value of 200, mean that for each sampling point, the computation
#' is performed using the 100 previous and the 100 next values).
#'
#' @param sampling A numeric value expressed in the timeline unit and specifying a subsampling used to
#' to perform the computation, allow to make computation faster, it hence determine the resolution of the
#' returned results (e.g., a value of 5000 mean that values will be computed every 5000 time units).
#'
#' @param bootn A numeric value indicating the number of bootstrap sampling used to compute studentize 95%IC.
#'
#' @param wtd A logical value (i.e., TRUE or FALSE) indicating whether the function should compute a weighed metric according to the length of the tracklets (default is FALSE).
#'
#' @param progress A logical value (i.e., TRUE or FALSE) indicating whether a progress bar should be displayed to inform process progression (default = TRUE).
#'
#' @return this function returns a list containing two elements:
#'  \itemize{
#'    \item{"BootCiStudent": }{a list containing as much data frame as the number of custom functions specified by the customFunc argument. Each dataframe contains:
#'       \itemize{
#'          \item{"97.5%": }{the upper limit of the studentized confidence interval (97.5%).}
#'          \item{"2.5%": }{the lower limit of the studentized confidence interval (2.5%).}
#'          \item{"mean": }{the result of the computation performed according to the customFunc and averaged over the sliding window specified by the Tstep argument.}
#'          \item{"timeCol": }{the timeline according to timeCol and sampling arguments.}
#'          \item{"nbTracklets": }{the number of tracklets included within each computation.}
#'       }}
#'     \item{"BootSampling": }{a list of sublists corresponding to the sampling points according to timeCol and sampling arguments. 
#'     Each sublist contains: 
#'       \itemize{
#'          \item{"UnWtd_Result" or "wtd_Result": }{a dataframe containing either unweighted or weighted mean and sd for each customFunc according to the wtd argument.}
#'          \item{"sampled_Tracklets": }{a list of matrices (one matrix per customFunc) containing the identity of the sampled tracklets (rows) over each bootstrap sampling (columns).}
#'          \item{"sampled_Values": }{a list of matrices (one matrix per customFunc) containing the value returned by a given customFunc (rows) over each bootstrap sampling (columns).}
#'          \item{"omitted_Tracklets": }{a list of matrices (one matrix per customFunc) containing the identity of the omitted tracklets (rows) over each bootstrap sampling (columns). 
#'          Tracklet omission may occur when a given customFunc returns NA. This list is only included in the output if at least one tracklets have been omitted.}
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
#' TrackN <- 25 # the number of tracklet to simulate
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
#' # check the tracklets
#' MoveR::drawTracklets(TrackList,
#'                      timeCol = "frame")
#' 
#' # add some metric to the dataset (speed and turning angle) and time unit conversion
#' TrackListV1 <-
#'   MoveR::analyseTracklets(
#'     TrackList,
#'     customFunc = list(
#'       # specify a first function to compute speed over each tracklet (a modulus present within the MoveR package)
#'       speed = function(x)
#'         MoveR::speed(x,
#'                      timeCol = "frame",
#'                      scale = 1),
#'       # compute turning angle in radians over each tracklet (a modulus present within the MoveR package)
#'       TurnAngle = function(x)
#'         MoveR::turnAngle(
#'           x,
#'          unit = "radians",
#'           timeCol = "frame",
#'          scale = 1
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
#' # smooth the speed and the turning angle across tracklets and time, and compute studentize 95% CI using bootstrap with 999 sampling.
#' # Here the computation is performed every 50 time unit and over an interval of 100 values, 
#' # 50 values are taken before and 50 values after the given time unit.
#' SmoothedtracksBoot <- MoveR::temporalBoot(
#'   trackDat = TrackListV1,
#'   timeCol = "frame",
#'   Tstep = 100,
#'   sampling = 50,
#'   wtd = TRUE,
#'   bootn = 999,
#'   customFunc = list(
#'     MeanSpeed = function(x)
#'       mean(x[["speed"]], na.rm = T),
#'     MeanTurnAngle = function(x)
#'       mean(x[["TurnAngle"]], na.rm = T)
#'   )
#' )
#' 
#' # plot the results
#' ## need to remove the NA introduced during smoothing to plot the 95% CI envelope
#' SmoothedtracksBootCInoNA <-
#'   lapply(SmoothedtracksBoot[["BootCiStudent"]], function(x)
#'     x[!is.na(x[["mean"]]),])
#' 
#' ## plot the mean and the 95% CI envelope by looping through the list containing the smoothed results for the speed and the turning angle
#' par(mfrow = c(1, 2))
#' for (i in seq(length(SmoothedtracksBootCInoNA))) {
#'   plot(
#'     SmoothedtracksBootCInoNA[[i]]$mean ~ SmoothedtracksBootCInoNA[[i]]$frame,
#'     type = "l",
#'     ylab = names(SmoothedtracksBootCInoNA)[[i]],
#'     xlab = "Time (frame)",
#'     ylim = c(round(min(
#'       c(
#'         SmoothedtracksBootCInoNA[[i]]$`2.5%`,
#'         SmoothedtracksBootCInoNA[[i]]$`97.5%`
#'       ) ,
#'       na.rm = T
#'     ), digits = 5),
#'     round(max(
#'       c(
#'         SmoothedtracksBootCInoNA[[i]]$`2.5%`,
#'         SmoothedtracksBootCInoNA[[i]]$`97.5%`
#'       ),
#'       na.rm = T
#'     ), digits = 5))
#'   )
#'   polygon(
#'     x = c(
#'       SmoothedtracksBootCInoNA[[i]]$frame,
#'       rev(SmoothedtracksBootCInoNA[[i]]$frame)
#'     ),
#'     y = c(
#'       SmoothedtracksBootCInoNA[[i]]$`2.5%`,
#'       rev(SmoothedtracksBootCInoNA[[i]]$`97.5%`)
#'     ),
#'     col = rgb(1, 0, 0, 0.1),
#'     border = NA,
#'     density = NA
#'   )
#' }
#'
#' @export

temporalBoot <-
  function(trackDat,
           timeCol = NULL,
           customFunc = NULL,
           Tinterval = NULL,
           Tstep = 1,
           sampling = 1,
           bootn = 500,
           wtd = FALSE,
           progress = TRUE) {
    if (is.null(timeCol) |
        !(timeCol %in% unlist(lapply(trackDat, names)))) {
      stop(
        "timeCol argument is missing or is not found in the provided dataset, timeCol might be misspelled"
      )
    }
    if (is.null(customFunc)) {
      stop("customFunc argument is missing, a customFunc is needed to compute metric")
    }
    
    # define the timeline
    ## check the time step of the timeline across the dataset
    TimelineStep <- unique(unlist(lapply(trackDat, function (w)
      apply(w[timeCol], 2, function(x) signif(diff(x), 4))
    )))
    ## in case the time step is not constant used the minimum value and print a warning message
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
    
    # if customFunc is a unnamed list of function, retrieve function names
    if (is.list(customFunc)) {
      if (is.null(names(customFunc))) {
        VarName <- stats::setNames(
          lapply(customFunc, function(x)
            strsplit(sub("\\(.*", "", deparse(x)), " ")[[2]]), unlist(VarName))
      }
      # if customFunc is a function retrieve function names and transform it to a named list
    } else if (is.function(customFunc)) {
      VarName <- strsplit(sub("\\(.*", "", deparse(customFunc)), " ")[[2]]
      customFunc <- stats::setNames(list(customFunc), VarName)
    }
    # initialize bootstrap result vector
    BootSampling <- list()
    boot.ci.student <- list()
    for (name in names(customFunc)) {
      temp_df <-
        data.frame(matrix(NA, nrow = length(Newtimeline), ncol = 5))
      colnames(temp_df) <- c("97.5%", "2.5%", "mean", timeCol, "nbTracklets")
      boot.ci.student[[name]] <- temp_df
    }
    # loop trough tracklets part (the timeline) to compute metrics according to customFunc
    if (isTRUE(progress)) {
      # initialize progress bar
      total = length(Newtimeline)
      pb <-
        progress::progress_bar$new(format = "Time processing [:bar] :current/:total (:percent)", total = total)
      pb$tick(0)
    }
    for (i in Newtimeline) {
      # Select Time interval according to the specified Tstep and extract the concerned tracklets part
      
      if (suppressWarnings(
        # the minimun timeline value below i and above the inferior substep is NOT lower than the minimum timeCol value within the dataset
        !(min(timeline[which(timeline < i &
                             timeline > (i - ((Tstep - TimeLStep) / 2)))]) <
          min(unlist(lapply(trackDat, function (w)
            min(w[timeCol], na.rm = T))), na.rm = T))
        &
        # AND the inferior substep is NOT below the first value of the timeline 
        !((i - ((Tstep - TimeLStep) / 2)) < timeline[1]) 
        &
        # AND the superior substep is NOT higher than the maximum timeCol value within the dataset
        !((i + ((Tstep - TimeLStep) / 2)) >
          max(unlist(lapply(trackDat, function (w)
            max(w[timeCol], na.rm = T))), na.rm = T))
      )) {
        # Specify the limit of the subampling step accordingly
        subStepInf <- abs(timeline - (i - ((Tstep - TimeLStep)/2)))
        subStepSup <- abs(timeline - (i + ((Tstep - TimeLStep)/2)))
        # create a vector containing the selected value of the timeline for the given substep
        selVal <-
          timeline[min(which(abs((
            min(subStepInf, na.rm = T) - subStepInf
          )) == 0 )):max(which(abs((
            min(subStepSup, na.rm = T) - subStepSup
          )) == 0))]
        
        # select the tracklet that are detected in the selected timeline part 
        WhoWhen <-
          MoveR::cutTracklets(trackDat, function(x)
            x[[timeCol]] >= min(selVal, na.rm = T) &
              x[[timeCol]] <= max(selVal, na.rm = T))
        # compute the metrics specified through customFunc on the selected tracklet part
        # initialize result list for a given Time interval
        Res <- list()
        len <- list()
        # loop trough tracklets on a given Time interval
        for (j in names(WhoWhen)) {
          df <- WhoWhen[[j]]
          for (n in seq(length(customFunc))) {
            #store the result of each computation in a list
            Res_temp <- customFunc[[n]](df)
            Res[[names(customFunc)[n]]] <-
              c(Res[[names(customFunc)[n]]], Res_temp)
            #store the length of each tracklet used in the computation in a list
            len_temp <- length(df[[timeCol]])
            len[[names(customFunc)[n]]] <-
              c(len[[names(customFunc)[n]]], len_temp)
          }
        }
        # Group Res and len in a list of df and append tracklet Id to it
        Reslen <- stats::setNames(
          lapply(names(Res), function(x)
            data.frame(
              Res = Res[[x]],
              len = len[[x]],
              TrackId = names(WhoWhen)
            )), names(Res))
        # create an equivalent of na.rm = T, useful to compute the weighed mean for each metric and
        # to know how many tracklets are used for computation for each metrics
        Reslen <- lapply(Reslen, function(x)
          na.omit(x))
        samplen <- lapply(Reslen, nrow)
        meanx <- list()
        sdx <- list()
        # in case weighed argument is FALSE, compute a simple mean
        if (wtd == FALSE) {
          meanx <- lapply(Reslen, function(x)
            mean(x$Res, na.rm = T))
          sdx <- lapply(Reslen, function(x)
            sd(x$Res, na.rm = T))
        } else if (wtd == TRUE) {
          # in case weighed argument is TRUE, compute a weighed mean according to tracklet length (Timecol)
          meanx <-
            lapply(Reslen, function(x)
              sum(x$len * x$Res) / sum(x$len))
          V1 <- lapply(Reslen, function(x)
            sum(x$len))
          V2 <- lapply(Reslen, function(x)
            sum(x$len ^ 2))
          sdx <- stats::setNames(as.list(lapply(names(Reslen), function(x) {
            sqrt(sum(Reslen[[x]]$len * ((
              Reslen[[x]]$Res - meanx[[x]]
            ) ^ 2)) / (V1[[x]] - V2[[x]] / V1[[x]]))
          })), names(meanx))
        }
        # create a list of the tracklet that could be sampled for each customfunc (after removing tracklets which return NA)
        toSample <- lapply(Reslen, function (x)
          x$TrackId)
        # check the number of tracklets that could be sampled for each customfunc (after removing tracklets which return NA)
        toSampleLen <- lapply(toSample, length)
        # in case the number of tracklets that could be sampled differ among customfunc, identify for which customfunc it is the case
        # and list the omitted tracklet to return it in the result list, see the sublists "BootSampling" -> "omitted.Tracklets"
        shorterMetric <-
          do.call("rbind", lapply(toSampleLen, function(x)
            x < max(unlist(toSampleLen), na.rm = T)))
        if (length(shorterMetric[which(shorterMetric == TRUE)]) > 0 &
            length(toSampleLen) > 1) {
          omittedTracks <-
            lapply(toSample[rownames(shorterMetric)[which(shorterMetric == TRUE)]], function(x) {
              !names(WhoWhen) %in% x
            })
          omittedTracksL <- list()
          for (o in names(omittedTracks)) {
            omittedTracksL[[o]] <-
              names(WhoWhen)[lapply(omittedTracks, function(x)
                which(x == TRUE))[[o]]]
          }
          # specify which metrics (1 metric is chosen for each number of tracklet) will be used for tracklet sampling
          samplingList <-
            c(rownames(shorterMetric)[which(shorterMetric == TRUE)], rownames(shorterMetric)[which(shorterMetric == FALSE)][1])
          if (length(rownames(shorterMetric)[which(shorterMetric == FALSE)]) > 1) {
            warning(
              "At Time=",
              i,
              ", the following custom functions returned NA for some tracklets : ",
              list(rownames(shorterMetric)[which(shorterMetric == TRUE)])
              ,
              "\ntracklet sampling is thus different among customfunc, check in the BootSampling -> omitted.Tracklets sublist \nto see which tracklets were omitted from the computation"
            )
          } else if (length(rownames(shorterMetric)[which(shorterMetric == FALSE)]) == 1) {
            warning(
              "At Time=",
              i,
              ", All the custom functions returned NA for some tracklets, \ntracklet sampling is thus different among the customfunc.\ncheck in BootSampling -> omitted.Tracklets sublist to see which tracklets were omitted from the computation"
            )
          }
        } else if (!length(shorterMetric[which(shorterMetric == TRUE)]) > 0 &
                   length(toSampleLen) == 1) {
          omittedTracksL <- NULL
          samplingList <- names(toSampleLen)
        } else if (!length(shorterMetric[which(shorterMetric == TRUE)]) > 0 &
                   length(toSampleLen) > 1) {
          omittedTracksL <- NULL
          samplingList <- names(toSampleLen)[1]
        }
        # in case there is no data at this time, end the computation here (no tracklet detected at this moment of the timeline)
        if(length(WhoWhen)==0){
          # append the results (95%CI, mean and time) to the boot.ci.student list
          for (name in names(customFunc)) {
            boot.ci.student[[name]][which(Newtimeline == i), ] <-
              c(NA, NA, NA, i, 0)
          }
        }else{
        # create a matrix to store the names of the sampled tracklets
        bootsamples <- stats::setNames(lapply(samplingList, function(x) {
          matrix(
            sample(
              toSample[[x]],
              size = samplen[[x]] * bootn,
              replace = T
            ),
            nrow = samplen[[x]],
            ncol = bootn
          )
        }), samplingList)
        if ((is.null(omittedTracksL) &
             length(toSampleLen) > 1) |
            (!is.null(omittedTracksL) &
             length(shorterMetric[which(shorterMetric == FALSE)]) > 0)) {
          bootsamplesRep <-
            rep(list(bootsamples[[rownames(shorterMetric)[which(shorterMetric == FALSE)][1]]]),
                length(rownames(shorterMetric)[which(shorterMetric == FALSE)]) - 1)
          names(bootsamplesRep) <-
            rownames(shorterMetric)[which(shorterMetric == FALSE)][-1]
          bootsamples <- c(bootsamples, bootsamplesRep)
        }
        
        # Match names of the sampled tracklets with computed values and tracklet length for each customfunc
        bootsamplesVal <- stats::setNames(
          lapply(names(bootsamples), function(x) {
            array(unlist(lapply(seq(bootn), function(y) {
              Reslen[[x]]$Res[match(bootsamples[[x]][, y], Reslen[[x]]$TrackId)]
            })), dim = c(length(bootsamples[[x]]) / bootn, bootn))
          }), names(bootsamples))
        
        bootsampleslen <- stats::setNames(
          lapply(names(bootsamples), function(x) {
            array(unlist(lapply(seq(bootn), function(y) {
              Reslen[[x]]$len[match(bootsamples[[x]][, y], Reslen[[x]]$TrackId)]
            })), dim = c(length(bootsamples[[x]]) / bootn, bootn))
          }), names(bootsamples))
        
        # compute the bootstrap estimates
        # in case user want to compute unweighed bootstrap according to tracklet length (Timecol)
        if (wtd == FALSE) {
          ## compute the means
          UbWtd.mean <- stats::setNames(
            lapply(names(customFunc), function(x) {
              unlist(lapply(seq(bootn), function(y) {
                mean(bootsamplesVal[[x]][, y], na.rm = T)
              }))
            }), paste(names(customFunc), "mean", sep = "_"))
          meanBoot <-
            lapply(UbWtd.mean, function(x)
              mean(x, na.rm = T))
          ## compute the sd
          UnWtd.sd <- stats::setNames(lapply(names(customFunc), function(x)
            unlist(lapply(seq(bootn), function(y) {
              sd(bootsamplesVal[[x]][, y], na.rm = T)
            }))), paste(names(customFunc), "sd", sep = "_"))
          ## compute the estimates
          bootEstNum <- stats::setNames(lapply(names(customFunc), function(x)
            unlist(lapply(seq(bootn), function(y) {
              mean(bootsamplesVal[[x]][, y], na.rm = T) - meanx[[x]]
            }))), names(customFunc))
          
          bootEstDenom <- stats::setNames(lapply(names(customFunc), function(x)
            unlist(lapply(seq(bootn), function(y) {
              sd(bootsamplesVal[[x]][, y], na.rm = T) / sqrt(samplen[[x]])
            }))), names(customFunc))
          
          bootEst <- stats::setNames(
            lapply(names(customFunc), function(z) {
              bootEstNum[[z]] / bootEstDenom[[z]]
            }), names(customFunc))
          ## append the list of Unweighted mean and sd
          BootSampling[[paste(timeCol, as.character(i), sep = "_")]][["UnWtd_Result"]] <-
            data.frame(UbWtd.mean, UnWtd.sd)
        } else if (wtd == TRUE) {
          # in case user want to compute weighed bootstrap according to tracklet length (Timecol)
          ## compute weighted mean according to the size of the sampled tracklet (timecol)
          wtd.mean <- stats::setNames(lapply(names(customFunc), function(x) {
            unlist(lapply(seq(bootn), function(y) {
              sum(bootsamplesVal[[x]][, y] * bootsampleslen[[x]][, y]) / sum(bootsampleslen[[x]][, y])
            }))
          }), paste(names(customFunc), "mean", sep = "_"))
          meanBoot <-
            lapply(wtd.mean, function(x)
              mean(x, na.rm = T))
          ## compute weighted sd according to the size of the sampled tracklet (timecol)
          V1 <- stats::setNames(lapply(names(customFunc), function(x)
            unlist(lapply(seq(bootn), function(y) {
              sum(bootsampleslen[[x]][, y], na.rm = T)
            }))), names(customFunc))

          V2 <- stats::setNames(lapply(names(customFunc), function(x)
            unlist(lapply(seq(bootn), function(y) {
              sum(bootsampleslen[[x]][, y] ^ 2, na.rm = T)
            }))), names(customFunc))
          
          wtd.sd_temp <- stats::setNames(lapply(names(customFunc), function(z) {
            unlist(lapply(seq(bootn), function(y) {
              sum(bootsampleslen[[z]][, y] * ((
                bootsamplesVal[[z]][, y] - wtd.mean[[paste(z, "mean", sep = "_")]][y]
              ) ^ 2))
            }))
          }), names(customFunc))

          wtd.sd <- stats::setNames(lapply(names(customFunc), function(z) {
            sqrt(wtd.sd_temp[[z]] / (V1[[z]] - V2[[z]] / V1[[z]]))
          }), paste(names(customFunc), "sd", sep = "_"))
          # append the list of weighted mean and sd
          BootSampling[[paste(timeCol, as.character(i), sep = "_")]][["wtd_Result"]] <-
            data.frame(wtd.mean, wtd.sd)
          # compute the bootstrap estimates
          bootEst <- stats::setNames(lapply(names(customFunc), function(x) {
            (wtd.mean[[paste(x, "mean", sep = "_")]] - meanx[[x]]) / (wtd.sd[[paste(x, "sd", sep = "_")]] / sqrt(samplen[[x]]))
          }), names(customFunc))
        }
        # append the list of sampled tracklets, values as well as the omitted tracklets
        BootSampling[[paste(timeCol, as.character(i), sep = "_")]][["sampled_Tracklets"]] <-
          bootsamples
        BootSampling[[paste(timeCol, as.character(i), sep = "_")]][["sampled_Values"]] <-
          bootsamplesVal
        BootSampling[[paste(timeCol, as.character(i), sep = "_")]][["omitted_Tracklets"]] <-
          omittedTracksL
        # compute the studentized 95%CI 
        boot.ci.student_temp <- stats::setNames(
          lapply(names(customFunc), function(z) {
            data.frame(t(c((
              meanx[[z]] - quantile(
                bootEst[[z]],
                probs = c(0.975, 0.025),
                na.rm = T
              ) *
                sdx[[z]] / sqrt(samplen[[z]])
            ), meanBoot[[paste(z, "mean", sep="_")]], i, length(WhoWhen)
            )))
          }), names(customFunc))
        # append the results (95%CI, mean and time) to the boot.ci.student list
        for (name in names(customFunc)) {
          boot.ci.student[[name]][which(Newtimeline == i), ] <-
            boot.ci.student_temp[[name]]
        } 
        }
      } else {
        for (name in names(customFunc)) {
          boot.ci.student[[name]][which(Newtimeline == i), timeCol] <- i
        }
      }
      if (isTRUE(progress)) {
        # progress bar
        pb$tick(1)
      }
    }
    return(list(BootCiStudent = boot.ci.student, BootSampling = BootSampling))
  }
