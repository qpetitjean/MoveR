#' @title perform analysis across fragments and time
#'
#'
#' @description Given a list of data frames containing tracking informations for each fragment (including the timeline)
#' and a custom function, this function perform the computation specified by the custom function across time
#' and smooth it before returning values
#'
#'
#' @param trackDat A list of data frame containing tracking informations for each fragment (including a timeline)
#'
#' @param timeCol A character string specifying the name of the timeline column
#'
#' @param customFunc A function used to perform the computation across time
#'
#' @param Tinterval A vector containing two numeric values expressed in the timeline unit and
#' specifying the time interval on which the computation is performed
#' (default is null, meaning the computation will be performed on the whole timeline)
#'
#' @param Tstep A numeric value expressed in the timeline unit and specifying the size of the
#' sliding window used to perform the computation
#'
#' @param sampling A numeric value expressed in the timeline unit and specifying a subsampling used to
#' to perform the computation, allow to make computation faster, it hence determine the resolution of the
#' returned results (e.g., 5000 mean that values will be computed every 5000 frames)
#'
#' @param bootn A numeric value corresponding to the number of bootstrap sampling to compute studentize 95%IC
#'
#' @param wtd TRUE or FALSE, compute a weighed metric (TRUE) or not (FALSE), (default is FALSE)
#'
#'
#' @return this function returns a list containing two sublist, the first sublist contains a dataframe with
#' bootstrap results (CI 97.5%, CI 2.5%, mean and a the resampled timeline according to timeCol argument),
#' the second sublist contains various results depending on wtd argument.
#' If wtd is FALSE, the second list returns a list of the sampled time point (e.g., frame), each one
#' containing a dataframe with values sampled over the bootstrap.
#' If wtd is TRUE, the second list returns a list of the sampled time point (e.g., frame), each one
#' containing a dataframe with values of weighed mean and sd computed over the bootstrap sampling.
#'
#'
#' @authors Quentin Petitjean
#'
#'
#' @examples
#'# generate some dummy fragments
#'## start to specify some parameters to generate fragments
#'Fragn <- 25 # the number of fragment to simulate
#'FragL <-
#'  100:1000 # the length of the fragments or a sequence to randomly sample fragment length
#'
#'fragsList <- stats::setNames(lapply(lapply(seq(Fragn), function(i)
#'  trajr::TrajGenerate(sample(FragL, 1), random = TRUE, fps = 1)), function(j)
#'    data.frame(
#'      x.pos = j$x - min(j$x),
#'      y.pos = j$y - min(j$y),
#'      frame = j$time
#'    )), seq(Fragn))
#'
#'# check the fragments
#'MoveR::drawFrags(fragsList,
#'                imgRes = c(max(MoveR::convert2list(fragsList)[["x.pos"]]),
#'                           max(MoveR::convert2list(fragsList)[["y.pos"]])),
#'                timeCol = "frame")
#'
#'# add some metric to the dataset (speed and turning angle) and time unit conversion
#'fragsListV1 <-
#'  MoveR::analyseFrags(
#'    fragsList,
#'    customFunc = list(
#'      # specify a first function to compute speed over each fragment (a modulus present within the MoveR package)
#'      speed = function(x)
#'        MoveR::speed(
#'          x,
#'          TimeCol = "frame",
#'          scale = 1,
#'          unit = "pixels"
#'        ),
#'      # compute turning angle in radians over each fragment (a modulus present within the MoveR package)
#'      TurnAngle = function(x)
#'        MoveR::turnAngle(x, unit = "radians"),
#'      # convert the time expressed in frame in second using a conversion factor of 25 frame per second
#'      TimeSec = function(x)
#'        x[["frame"]] / 25,
#'      # or in minutes
#'      TimeMin = function(x)
#'        x[["frame"]] / 25 / 60
#'    )
#'  )
#'
#'# smooth the speed and the turning angle across fragments and time, here we perform the computation 
#'# every 50 time unit and on an interval of 100 values, 50 values are taken before and 50 values after the given time unit. 
#'# and compute studentize 95% CI using bootstrap with 999 sampling. 
#'SmoothedtracksBoot <- MoveR::analyseTimeBoots(
#'  trackDat = fragsListV1,
#'  timeCol = "frame",
#'  Tstep = 100,
#'  sampling = 50,
#'  wtd = TRUE,
#'  bootn = 999,
#'  customFunc = list(
#'    MeanSpeed = function(x)
#'      mean(x[["speed"]], na.rm = T),
#'    MeanTurnAngle = function(x)
#'      mean(x[["TurnAngle"]], na.rm = T)
#'  )
#')
#'
#'# plot the results
#'## need to remove the NA introduced during smoothing to plot the 95% CI envelope
#'SmoothedtracksBootCInoNA <-
#'  lapply(SmoothedtracksBoot[["BootCiStudent"]], function(x)
#'    x[!is.na(x[["mean"]]), ])
#'
#'## plot the mean and the 95% CI envelope by looping through the list containing the smoothed results for the speed and the turning angle
#'par(mfrow = c(1, 2))
#'for (i in seq(length(SmoothedtracksBootCInoNA))) {
#'  plot(
#'    SmoothedtracksBootCInoNA[[i]]$mean ~ SmoothedtracksBootCInoNA[[i]]$frame,
#'    type = "l",
#'    ylab = names(SmoothedtracksBootCInoNA)[[i]],
#'    xlab = "Time (frame)",
#'    ylim = c(round(min(
#'      c(
#'        SmoothedtracksBootCInoNA[[i]]$`2.5%`,
#'        SmoothedtracksBootCInoNA[[i]]$`97.5%`
#'      ) ,
#'      na.rm = T
#'    ), digits = 5),
#'    round(max(
#'      c(
#'        SmoothedtracksBootCInoNA[[i]]$`2.5%`,
#'        SmoothedtracksBootCInoNA[[i]]$`97.5%`
#'      ),
#'      na.rm = T
#'    ), digits = 5))
#'  )
#'  polygon(
#'    x = c(
#'      SmoothedtracksBootCInoNA[[i]]$frame,
#'      rev(SmoothedtracksBootCInoNA[[i]]$frame)
#'    ),
#'    y = c(
#'      SmoothedtracksBootCInoNA[[i]]$`2.5%`,
#'      rev(SmoothedtracksBootCInoNA[[i]]$`97.5%`)
#'    ),
#'    col = rgb(1, 0, 0, 0.1),
#'    border = NA,
#'    density = NA
#'  )
#'}
#'
#' @export

analyseTimeBoots <-
  function(trackDat,
           timeCol = NULL,
           customFunc = NULL,
           Tinterval = NULL,
           Tstep = 1,
           sampling = 1,
           bootn = 500,
           wtd = FALSE) {
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
        "In TimeCol : \n the time step is not constant across trackDat and returns the following values: ",
        TimelineStep,
        "\n here the function used ",
        min(TimelineStep, na.rm = T),
        ", but perhaps consider resampling the fragments to better control the behavior of the function", 
        "\n see MoveR::resampleFrags()"
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
    # initialize bootstrap result vector
    BootSampling <- list()
    boot.ci.student <- list()
    for (name in names(customFunc)) {
      temp_df <-
        data.frame(matrix(NA, nrow = length(Newtimeline), ncol = 4))
      colnames(temp_df) <- c("97.5%", "2.5%", "mean", timeCol)
      boot.ci.student[[name]] <- temp_df
    }
    # initialize progress bar
    total = length(Newtimeline)
    pb <-
      progress::progress_bar$new(format = "sample processing [:bar] :current/:total (:percent)", total = total)
    pb$tick(0)
    # loop trough fragments part to compute metrics according to customFunc
    for (i in Newtimeline) {
      # Select Time interval according to the specified Tstep and extract the concerned fragments part
      
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
        
        # select the fragment that are detected in the selected timeline part 
        WhoWhen <-
          cutFrags(trackDat, function(x)
            x[[timeCol]] >= min(selVal, na.rm = T) &
              x[[timeCol]] <= max(selVal, na.rm = T))
        # compute the metrics specified through customFunc on the selected fragment part
        # initialize result list for a given Time interval
        Res <- list()
        len <- list()
        # loop trough fragments on a given Time interval
        for (j in names(WhoWhen)) {
          df <- WhoWhen[[j]]
          for (n in seq(length(customFunc))) {
            #store the result of each computation in a list
            Res_temp <- customFunc[[n]](df)
            Res[[names(customFunc)[n]]] <-
              c(Res[[names(customFunc)[n]]], Res_temp)
            #store the length of each fragment used in the computation in a list
            len_temp <- length(df[[timeCol]])
            len[[names(customFunc)[n]]] <-
              c(len[[names(customFunc)[n]]], len_temp)
          }
        }
        # Group Res and len in a list of df and append fragment Id to it
        Reslen <-
          lapply(names(Res), function(x)
            data.frame(
              Res = Res[[x]],
              len = len[[x]],
              fragId = names(WhoWhen)
            ))
        names(Reslen) <- names(Res)
        # create an equivalent of na.rm = T, useful to compute the weighed mean for each metric and
        # to know how many fragments are used for computation for each metrics
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
          # in case weighed argument is TRUE, compute a weighed mean according to fragment length (Timecol)
          meanx <-
            lapply(Reslen, function(x)
              sum(x$len * x$Res) / sum(x$len))
          V1 <- lapply(Reslen, function(x)
            sum(x$len))
          V2 <- lapply(Reslen, function(x)
            sum(x$len ^ 2))
          sdx <- as.list(lapply(names(Reslen), function(x) {
            sqrt(sum(Reslen[[x]]$len * ((
              Reslen[[x]]$Res - meanx[[x]]
            ) ^ 2)) / (V1[[x]] - V2[[x]] / V1[[x]]))
          }))
          names(sdx) <- names(meanx)
        }
        # create a list of the fragment that could be sampled for each customfunc (after removing fragments which return NA)
        toSample <- lapply(Reslen, function (x)
          x$fragId)
        # check the number of fragments that could be sampled for each customfunc (after removing fragments which return NA)
        toSampleLen <- lapply(toSample, length)
        # in case the number of fragments that could be sampled differ among customfunc, identify for which customfunc it is the case
        # and list the omitted fragment to return it in the result list, see the sublists "BootSampling" -> "omitted.Frags"
        shorterMetric <-
          do.call("rbind", lapply(toSampleLen, function(x)
            x < max(unlist(toSampleLen), na.rm = T)))
        if (length(shorterMetric[which(shorterMetric == TRUE)]) > 0 &
            length(toSampleLen) > 1) {
          omittedFrags <-
            lapply(toSample[rownames(shorterMetric)[which(shorterMetric == TRUE)]], function(x) {
              !names(WhoWhen) %in% x
            })
          omittedFragsL <- list()
          for (o in names(omittedFrags)) {
            omittedFragsL[[o]] <-
              names(WhoWhen)[lapply(omittedFrags, function(x)
                which(x == TRUE))[[o]]]
          }
          # specify which metrics (1 metric is chosen for each number of fragment) will be used for fragment sampling
          samplingList <-
            c(rownames(shorterMetric)[which(shorterMetric == TRUE)], rownames(shorterMetric)[which(shorterMetric == FALSE)][1])
          if (length(rownames(shorterMetric)[which(shorterMetric == FALSE)]) > 1) {
            warning(
              "At Time=",
              i,
              ", the following custom functions returned NA for some fragments : ",
              list(rownames(shorterMetric)[which(shorterMetric == TRUE)])
              ,
              "\nfragment sampling is thus different among customfunc, check in the BootSampling -> omitted.Frags sublist \nto see which fragments were omitted from the computation"
            )
          } else if (length(rownames(shorterMetric)[which(shorterMetric == FALSE)]) == 1) {
            warning(
              "At Time=",
              i,
              ", All the custom functions returned NA for some fragments, \nfragment sampling is thus different among every customfunc.\ncheck in the BootSampling -> omitted.Frags sublist to see which fragments were omitted from the computation"
            )
          }
        } else if (!length(shorterMetric[which(shorterMetric == TRUE)]) > 0 &
                   length(toSampleLen) == 1) {
          omittedFragsL <- NULL
          samplingList <- names(toSampleLen)
        } else if (!length(shorterMetric[which(shorterMetric == TRUE)]) > 0 &
                   length(toSampleLen) > 1) {
          omittedFragsL <- NULL
          samplingList <- names(toSampleLen)[1]
        }
        # create a matrix to store the names of the sampled fragments
        bootsamples <- lapply(samplingList, function(x) {
          matrix(
            sample(
              toSample[[x]],
              size = samplen[[x]] * bootn,
              replace = T
            ),
            nrow = samplen[[x]],
            ncol = bootn
          )
        })
        names(bootsamples) <- samplingList
        if ((is.null(omittedFragsL) &
             length(toSampleLen) > 1) |
            (!is.null(omittedFragsL) &
             length(shorterMetric[which(shorterMetric == FALSE)]) > 0)) {
          bootsamplesRep <-
            rep(list(bootsamples[[rownames(shorterMetric)[which(shorterMetric == FALSE)][1]]]),
                length(rownames(shorterMetric)[which(shorterMetric == FALSE)]) - 1)
          names(bootsamplesRep) <-
            rownames(shorterMetric)[which(shorterMetric == FALSE)][-1]
          bootsamples <- c(bootsamples, bootsamplesRep)
        }
        
        # Match names of the sampled fragments with computed values and fragment length for each customfunc
        bootsamplesVal <-
          lapply(names(bootsamples), function(x) {
            array(unlist(lapply(seq(bootn), function(y) {
              Reslen[[x]]$Res[match(bootsamples[[x]][, y], Reslen[[x]]$fragId)]
            })), dim = c(length(bootsamples[[x]]) / bootn, bootn))
          })
        names(bootsamplesVal) <- names(bootsamples)
        
        bootsampleslen <-
          lapply(names(bootsamples), function(x) {
            array(unlist(lapply(seq(bootn), function(y) {
              Reslen[[x]]$len[match(bootsamples[[x]][, y], Reslen[[x]]$fragId)]
            })), dim = c(length(bootsamples[[x]]) / bootn, bootn))
          })
        names(bootsampleslen) <- names(bootsamples)
        
        # compute the bootstrap estimates
        # in case user want to compute unweighed bootstrap according to fragment length (Timecol)
        if (wtd == FALSE) {
          ## compute the means
          UbWtd.mean <-
            lapply(names(customFunc), function(x) {
              unlist(lapply(seq(bootn), function(y) {
                mean(bootsamplesVal[[x]][, y], na.rm = T)
              }))
            })
          names(UbWtd.mean) <- names(customFunc)
          meanBoot <-
            lapply(UbWtd.mean, function(x)
              mean(x, na.rm = T))
          ## compute the sd
          UnWtd.sd <- lapply(names(customFunc), function(x)
            unlist(lapply(seq(bootn), function(y) {
              sd(bootsamplesVal[[x]][, y], na.rm = T)
            })))
          names(UnWtd.sd) <- names(customFunc)
          ## compute the estimates
          bootEstNum <- lapply(names(customFunc), function(x)
            unlist(lapply(seq(bootn), function(y) {
              mean(bootsamplesVal[[x]][, y], na.rm = T) - meanx[[x]]
            })))
          names(bootEstNum) <- names(customFunc)
          bootEstDenom <- lapply(names(customFunc), function(x)
            unlist(lapply(seq(bootn), function(y) {
              sd(bootsamplesVal[[x]][, y], na.rm = T) / sqrt(samplen[[x]])
            })))
          names(bootEstDenom) <- names(customFunc)
          bootEst <-
            lapply(names(customFunc), function(z) {
              bootEstNum[[z]] / bootEstDenom[[z]]
            })
          names(bootEst) <- names(customFunc)
          ## append the list of Unweighted mean and sd
          BootSampling[[paste(timeCol, as.character(i), sep = "_")]][["UnWtd.Result"]] <-
            data.frame(cbind(UbWtd.mean, UnWtd.sd))
        } else if (wtd == TRUE) {
          # in case user want to compute weighed bootstrap according to fragment length (Timecol)
          ## compute weighted mean according to the size of the sampled fragment (timecol)
          wtd.mean <- lapply(names(customFunc), function(x) {
            unlist(lapply(seq(bootn), function(y) {
              sum(bootsamplesVal[[x]][, y] * bootsampleslen[[x]][, y]) / sum(bootsampleslen[[x]][, y])
            }))
          })
          names(wtd.mean) <- names(customFunc)
          meanBoot <-
            lapply(wtd.mean, function(x)
              mean(x, na.rm = T))
          ## compute weighted sd according to the size of the sampled fragment (timecol)
          V1 <- lapply(names(customFunc), function(x)
            unlist(lapply(seq(bootn), function(y) {
              sum(bootsampleslen[[x]][, y], na.rm = T)
            })))
          names(V1) <- names(customFunc)
          V2 <- lapply(names(customFunc), function(x)
            unlist(lapply(seq(bootn), function(y) {
              sum(bootsampleslen[[x]][, y] ^ 2, na.rm = T)
            })))
          names(V2) <- names(customFunc)
          wtd.sd_temp <- lapply(names(customFunc), function(z) {
            unlist(lapply(seq(bootn), function(y) {
              sum(bootsampleslen[[z]][, y] * ((
                bootsamplesVal[[z]][, y] - wtd.mean[[z]][y]
              ) ^ 2))
            }))
          })
          names(wtd.sd_temp) <- names(customFunc)
          wtd.sd <- lapply(names(customFunc), function(z) {
            sqrt(wtd.sd_temp[[z]] / (V1[[z]] - V2[[z]] / V1[[z]]))
          })
          names(wtd.sd) <- names(customFunc)
          # append the list of weighted mean and sd
          BootSampling[[paste(timeCol, as.character(i), sep = "_")]][["wtd.Result"]] <-
            data.frame(cbind(wtd.mean, wtd.sd))
          # compute the bootstrap estimates
          bootEst <- lapply(names(customFunc), function(x) {
            (wtd.mean[[x]] - meanx[[x]]) / (wtd.sd[[x]] / sqrt(samplen[[x]]))
          })
          names(bootEst) <- names(customFunc)
        }
        # append the list of sampled fragments, values as well as the omitted fragments
        BootSampling[[paste(timeCol, as.character(i), sep = "_")]][["sampled.Frags"]] <-
          bootsamples
        BootSampling[[paste(timeCol, as.character(i), sep = "_")]][["sampled.Values"]] <-
          bootsamplesVal
        BootSampling[[paste(timeCol, as.character(i), sep = "_")]][["omitted.Frags"]] <-
          omittedFragsL
        # compute the studentize CI 95%
        boot.ci.student_temp <-
          lapply(names(customFunc), function(z) {
            data.frame(t(c((
              meanx[[z]] - quantile(
                bootEst[[z]],
                probs = c(0.975, 0.025),
                na.rm = T
              ) *
                sdx[[z]] / sqrt(samplen[[z]])
            ), meanBoot[[z]], i
            )))
          })
        names(boot.ci.student_temp) <- names(customFunc)
        # append the results (CI 95%, mean and time) to the boot.ci.student list
        for (name in names(customFunc)) {
          boot.ci.student[[name]][which(Newtimeline == i), ] <-
            boot.ci.student_temp[[name]]
        }
      } else {
        for (name in names(customFunc)) {
          boot.ci.student[[name]][which(Newtimeline == i), timeCol] <- i
        }
      }
      # progress bar
      pb$tick(1)
    }
    return(list(BootCiStudent = boot.ci.student, BootSampling = BootSampling))
  }
