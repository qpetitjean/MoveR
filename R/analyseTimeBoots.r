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
#'
#' @examples
#'
#' #TODO
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
    
    if (is.null(Tinterval)) {
      # define the timeline
      timeline <- seq(min(unlist(lapply(trackDat, function (w)
        min(w[timeCol], na.rm = T))), na.rm = T),
        max(unlist(lapply(trackDat, function (w)
          max(w[timeCol], na.rm = T))), na.rm = T), by = 1)
      # compute sliding mean every n time unit (according to "sampling" parameter) allow to make computation faster
      Newtimeline <- seq(from = timeline[1],
                         to = timeline[length(timeline)],
                         by = sampling)
      Newtimeline[length(Newtimeline)+1] <- timeline[length(timeline)]
      Newtimeline[which(Newtimeline == 0)] <- 1
      Newtimeline <- Newtimeline[!duplicated(Newtimeline)]
    } else {
      # define the timeline
      timeline <- seq(from = round(Tinterval[1] - ((Tstep - 1) / 2)),
                      to = round(Tinterval[2] + ((Tstep - 1) / 2)),
                      by = 1)
      # compute sliding mean every n time unit (according to "sampling" parameter) allow to make computation faster
      Newtimeline <- seq(from = Tinterval[1],
                         to = Tinterval[2],
                         by = sampling)
      Newtimeline[length(Newtimeline)+1] <- Tinterval[2]
      Newtimeline[which(Newtimeline == 0)] <- 1
      Newtimeline <- Newtimeline[!duplicated(Newtimeline)]
    }
    # initialize progress bar
    total = length(Newtimeline)
    pb <-
      progress::progress_bar$new(format = "sample processing [:bar] :current/:total (:percent)", total = total)
    pb$tick(0)
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
    for(name in names(customFunc)){ 
      temp_df <- data.frame(matrix(NA, nrow = length(Newtimeline), ncol = 4))
      colnames(temp_df) <- c("97.5%", "2.5%", "mean", timeCol)
      boot.ci.student[[name]] <- temp_df
      }
    # loop trough fragments part to compute metrics according to customFunc
    for (i in Newtimeline) {
      # Select Time interval according to the specified Tstep and extract the concerned fragments part
      # since we use a sliding mean, the time values below the minimum time value detected in the dataset as well as
      # when i - (Tstep-1)/2 is below the first value of the timeline or i + (Tstep-1)/2 is above the maximum 
      # time value detected in the dataset, it result in NA
      if (suppressWarnings(!(min(timeline[which(timeline < i &
                                                timeline > round((i - ((
                                                  Tstep - 1
                                                ) / 2))))]) <
                             min(unlist(
                               lapply(trackDat, function (w)
                                 min(w[timeCol], na.rm = T))
                             ), na.rm = T)) &
                           !(round((i - ((Tstep - 1) / 2
                           ))) < timeline[1]) &
                           !(round((i + ((Tstep - 1) / 2
                           ))) > max(unlist(
                             lapply(trackDat, function (w)
                               max(w[timeCol], na.rm = T))
                           ), na.rm = T)))) {
        selVal <-
          timeline[which(timeline ==  round((i - ((
            Tstep - 1
          ) / 2)))):which(timeline == round((i + ((
            Tstep - 1
          ) / 2))))]
        # select the fragment that are detected in the selected timeline part and 
        # Cut them according the selected part of the timeline 
        WhoWhen <- cutFrags(trackDat, function (x)
          x[[timeCol]] %in% selVal)
        # compute the metrics specified through customFunc on the selected fragment part
        # initialize result list for a given Time interval
        Res <- list()
        len <- vector()
        # loop trough fragments on a given Time interval
        for (j in names(WhoWhen)) {
          df <- WhoWhen[[j]]
          for (n in seq(length(customFunc))) {
            Res_temp <- customFunc[[n]](df)
              Res[[names(customFunc)[n]]] <-
                c(Res[[names(customFunc)[n]]], Res_temp)
          }
          len_temp <- length(df[[timeCol]])
          len <- c(len, len_temp)
        }
        # in case for some fragment the specified computation result in NA,
        # identify NA fragments to avoid their sampling during bootstrap
        Nadetect <-  lapply(Res, function(x) data.frame(names(WhoWhen), x))
        Nafrags <- unlist(lapply(Nadetect, function(y) y[which(is.na(y$x)), 1]))
        Nafrags <- Nafrags[!duplicated(Nafrags)]
        if(length(Nafrags) > 0) {
        NafragsPos <- unlist(lapply(Nadetect, function(x) which(x$names.WhoWhen.%in%Nafrags)))
        NafragsPos <- NafragsPos[!duplicated(NafragsPos)]
        ## perform the bootstrap
        # compute confidence interval using studentize bootstrap on this part of the dataset
        # create an equivalent of na.rm = T,to remove fragments returning NA after custom computation
        Res <- lapply(Res, function(z) z[-c(NafragsPos)])
        len <- len[-c(NafragsPos)]
        }
        samplen <- length(len)
        meanx <- list()
        sdx <- list()
        # in case weighed argument is FALSE, compute a simple mean
        if (wtd == FALSE) {
          meanx <- lapply(Res, function(x) mean(x, na.rm = T))
          sdx <- lapply(Res, function(x) sd(x, na.rm = T))
        } else if (wtd == TRUE) {
          # in case weighed argument is TRUE, compute a weighed mean according to fragment length (Timecol)
          meanx <- lapply(Res, function(x) sum(len * x) / sum(len))
          V1 <- sum(len)
          V2 <- sum(len ^ 2)
          sdx <- as.list(mapply(function(x,y) sqrt(sum(len * ((x - y) ^ 2)) / (V1 - V2 / V1)), Res, meanx))
        }
        # create an empty matrix to store the mean value computed for each sampled fragment
        #bootsamplesVal <- data.frame(matrix(NA, samplen, bootn))
        bootsamplesVal <- list()
        # create an empty matrix to store the length of each sampled fragment (needed to compute wheighed mean)
        #bootsampleslen <- data.frame(matrix(NA, samplen, bootn))
        bootsampleslen <- list()
        # create a matrix to store the names of the sampled fragments
        toSample <-
          names(WhoWhen)[which(!(names(WhoWhen) %in% Nafrags))]
        bootsamples <-
          matrix(
            sample(toSample, size = samplen * bootn, replace = T),
            nrow = samplen,
            ncol = bootn
          )
        # fill the matrix with the value corresponding to each sampled fragment
        for (k in seq(bootn)) {
          Res <- list()
          len <- vector()
          for (j in bootsamples[, k]) {
            df <- WhoWhen[[j]]
            for (n in seq(length(customFunc))) {
              Res_temp <- customFunc[[n]](df)
              Res[[names(customFunc)[n]]] <-
                c(Res[[names(customFunc)[n]]], Res_temp)
            }
            len_temp <- length(df[[timeCol]])
            len <- c(len, len_temp)
          }
          bootsamplesVal[[k]] <- Res
          bootsampleslen[[k]] <- len
        }
        
          # compute the bootstrap estimates
          meanBoot <- list()
          bootEst <- list()
          wtd.mean <- list()
          wtd.sd <- list()
          boot.ci.student_temp <- list()
          for (name in names(customFunc)) {
            # in case user want to compute unweighed bootstrap according to fragment length (Timecol)
            if (wtd == FALSE) {
            meanBoot[[name]] <-
              mean(unlist(lapply(lapply(X = bootsamplesVal, FUN = "[[", name), function(x)
                mean(x, na.rm = T))), na.rm = T)
            bootEst[[name]] <-
              unlist(lapply(lapply(lapply(X = bootsamplesVal, FUN = "[[", name), function(x)
                mean(x, na.rm = T)), function (y)
                  y - meanx[[name]])) /
              unlist((lapply(lapply(X = bootsamplesVal, FUN = "[[", name), function(x)
                sd(x, na.rm = T)))) / sqrt(samplen)
            BootSampling[[paste(timeCol, as.character(i), sep = "_")]] <-
            bootsamplesVal
          } else if (wtd == TRUE) {
          # in case user want to compute weighed bootstrap according to fragment length (Timecol)
          # compute weighted mean according to the size of the sampled fragment (timecol)
          wtd.mean[[name]] <-
            mapply(
              bootsampleslen,
              lapply(X = bootsamplesVal, FUN = "[[", name),
              FUN = function(x, y)
                sum(x * y)
            ) / unlist(lapply(bootsampleslen, sum))
          meanBoot[[name]] <- mean(wtd.mean[[name]], na.rm = T)
          # compute weighted sd according to the size of the sampled fragment (timecol)
          V1 <- unlist(lapply(bootsampleslen, sum))
          V2 <- unlist(lapply(bootsampleslen, function(x)
            sum(x ^ 2)))
          wtd.sd_temp <- list()
          for (l in seq(bootn)) {
            temp <- list()
            temp[[name]] <-
              sum(bootsampleslen[[l]] * ((bootsamplesVal[[l]][[name]] - wtd.mean[[name]][l]) ^ 2))
            wtd.sd_temp[[name]] <- c(wtd.sd_temp[[name]], temp[[name]])
          }
          wtd.sd[[name]] <- sqrt(unlist(wtd.sd_temp[[name]]) / (V1 - V2 / V1))
          
          BootSampling[[paste(timeCol, as.character(i), sep = "_")]][["sampling"]] <- bootsamplesVal
          BootSampling[[paste(timeCol, as.character(i), sep = "_")]][["wtd.Result"]] <-
            data.frame(cbind(wtd.mean, wtd.sd))
          
          # compute the bootstrap estimates
          bootEst[[name]] <-
            (wtd.mean[[name]] - meanx[[name]]) / (wtd.sd[[name]] / sqrt(samplen))
          }
            # compute the studentize CI 95%
            boot.ci.student_temp[[name]] <-
              meanx[[name]] - quantile(bootEst[[name]],
                                       probs = c(0.975, 0.025),
                                       na.rm = T) *
              sdx[[name]] / sqrt(samplen)
            
            boot.ci.student_temp[[name]] <-
              data.frame(t(c(boot.ci.student_temp[[name]], meanBoot[[name]], i)))
            colnames(boot.ci.student_temp[[name]]) <-
              c("97.5%", "2.5%", "mean", timeCol)
            boot.ci.student[[name]][which(Newtimeline == i),] <- boot.ci.student_temp[[name]]
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
