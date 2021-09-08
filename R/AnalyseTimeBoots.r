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

analyseTimeBoots.wtd <-
  function(trackDat,
           timeCol = NULL,
           customFunc = NULL,
           Tinterval = NULL,
           Tstep = 1,
           sampling = 1,
           bootn = 500,
           wtd = FALSE) {
    if (is.null(timeCol) |
        is.null("runTimelinef" %in% unlist(lapply(trackDat, names)))) {
      stop(
        "timeCol argument is missing or is not found in the provided dataset, timeCol might be misspelled"
      )
    }
    if (is.null(customFunc)) {
      stop("customFunc argument is missing, a customFunc is needed to compute metric")
    }
    
    if (is.null(Tinterval)) {
      # define the timeline
      timeline <- seq(max(unlist(lapply(trackDat, function (w)
        max(w[timeCol], na.rm = T))), na.rm = T))
    } else {
      # define the timeline
      timeline <- seq(from = Tinterval[1],
                      to = Tinterval[2],
                      by = 1)
    }
    # compute sliding mean every n time unit (according to "sampling" parameter) allow to make computation faster
    Newtimeline <- seq(from = timeline[1],
                       to = timeline[length(timeline)],
                       by = sampling)
    Newtimeline[which(Newtimeline == 0)] <- 1
    
    # initialize progress bar
    total = length(Newtimeline)
    pb <-
      progress::progress_bar$new(format = "sample processing [:bar] :current/:total (:percent)", total = total)
    pb$tick(0)
    # initialize bootstrap result vector
    BootSampling <- list()
    boot.ci.student <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(boot.ci.student) <- c("97.5%", "2.5%", "mean", "frame")
    # loop trough fragments part to compute metrics according to customFunc
    for (i in Newtimeline) {
      # Select Time interval according to the specified Tstep and extract the concerned fragments part
      # since we use a sliding mean, the time values below Tstep/2 result in NA
      if (!(i - ((Tstep - 1) / 2)) < Newtimeline[1] &
          !(i + ((Tstep - 1) / 2)) > Newtimeline[length(Newtimeline)]) {
        selVal <-
          timeline[which(timeline == (i - round(((
            Tstep - 1
          ) / 2)))):which(timeline == (i + round(((
            Tstep - 1
          ) / 2))))]
        # identify part of fragment detected in the selected Time interval
        When <-
          lapply(trackDat, function(x)
            x$runTimelinef %in% selVal)
        # identify which fragment are detected in the selected Time interval
        Who <-
          which(unlist(lapply(When, function(y)
            TRUE %in% y)) == TRUE)
        # isolate part of detected fragment included in time interval
        WhoWhen <-
          lapply(When[c(names(Who))], function (z)
            which(z == TRUE))
        # compute the metrics specified through customFunc on the selected fragment part
        # initialize result vector for a given Time interval
        Res <- vector()
        len <- vector()
        # loop trough fragments on a given Time interval
        for (j in names(WhoWhen)) {
          df <- trackDat[[j]][c(WhoWhen[[j]]),]
          Res_temp <- customFunc(df)
          Res <- c(Res, Res_temp)
          len_temp <- length(df[[timeCol]])
          len <- c(len, len_temp)
        }
        # in case for some fragment the specified computation result in NA,
        # identify NA fragments to avoid their sampling during bootstrap
        Nadetect <-  data.frame(names(WhoWhen), Res)
        Nafrags <- Nadetect[which(is.na(Nadetect$Res)), 1]
        
        ## perform the bootstrap
        # compute confidence interval using studentize bootstrap on this part of the dataset
        # create an equivalent of na.rm = T,to remove fragments returning NA after custom computation
        na.rm <- !is.na(Res) & !is.na(len)
        Res <- Res[na.rm]
        len <- len[na.rm]
        samplen <- length(Res)
        # in case weighed argument is FALSE, compute a simple mean
        if (wtd == FALSE) {
          meanx <- mean(Res, na.rm = T)
          sdx <- sd(Res, na.rm = T)
        } else if (wtd == TRUE) {
          # in case weighed argument is TRUE, compute a weighed mean according to fragment length (Timecol)
          meanx <- sum(len * Res) / sum(len)
          V1 <- sum(len)
          V2 <- sum(len ^ 2)
          sdx <- sqrt(sum(len * ((Res - meanx) ^ 2)) / (V1 - V2 / V1))
        }
        # create an empty matrix to store the mean value computed for each sampled fragment
        bootsamplesVal <- data.frame(matrix(NA, samplen, bootn))
        # create an empty matrix to store the length of each sampled fragment (needed to compute wheighed mean)
        bootsampleslen <- data.frame(matrix(NA, samplen, bootn))
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
          Res <- vector()
          len <- vector()
          for (j in bootsamples[, k]) {
            df <- trackDat[[j]][c(WhoWhen[[j]]),]
            Res_temp <- customFunc(df)
            len_temp <- length(df[[timeCol]])
            Res <- c(Res, Res_temp)
            len <- c(len, len_temp)
          }
          bootsamplesVal[[k]] <- Res
          bootsampleslen[[k]] <- len
        }
        
        # in case user want to compute unweighed bootstrap according to fragment length (Timecol)
        if (wtd == FALSE) {
          # compute the bootstrap estimates
          meanBoot <-
            mean(apply(bootsamplesVal, 2, mean), na.rm = T)
          bootEst <-
            (apply(bootsamplesVal, 2, mean) - meanx) / (apply(bootsamplesVal, 2, sd) / sqrt(samplen))
          BootSampling[[paste(timeCol, as.character(i), sep = "_")]] <-
            bootsamplesVal
        } else if (wtd == TRUE) {
          # in case user want to compute weighed bootstrap according to fragment length (Timecol)
          # compute weighted mean according to the size of the sampled fragment (timecol)
          wtd.mean <-
            mapply(
              bootsampleslen,
              bootsamplesVal,
              FUN = function(x, y)
                sum(x * y)
            ) / unlist(lapply(bootsampleslen, sum))
          meanBoot <- mean(wtd.mean, na.rm = T)
          # compute weighted sd according to the size of the sampled fragment (timecol)
          V1 <- unlist(lapply(bootsampleslen, sum))
          V2 <- unlist(lapply(bootsampleslen, function(x)
            sum(x ^ 2)))
          wtd.sd_temp <- list()
          for (l in seq(length(bootsamplesVal))) {
            temp <-
              sum(bootsampleslen[[l]] * ((bootsamplesVal[[l]] - wtd.mean[l]) ^ 2))
            wtd.sd_temp <- c(wtd.sd_temp, temp)
          }
          wtd.sd <- sqrt(unlist(wtd.sd_temp) / (V1 - V2 / V1))
          BootSampling[[paste(timeCol, as.character(i), sep = "_")]] <-
            data.frame(cbind(wtd.mean, wtd.sd))
          # compute the bootstrap estimates
          bootEst <-
            (wtd.mean - meanx) / (wtd.sd / sqrt(samplen))
        }
        # compute the studentize CI 95%
        boot.ci.student_temp <-
          meanx - quantile(bootEst,
                           probs = c(0.975, 0.025),
                           na.rm = T) *
          sdx / sqrt(samplen)
        boot.ci.student_temp <-
          data.frame(t(c(boot.ci.student_temp, meanBoot, i)))
        colnames(boot.ci.student_temp) <-
          c("97.5%", "2.5%", "mean", timeCol)
        boot.ci.student <-
          rbind(boot.ci.student, boot.ci.student_temp)
      } else {
        boot.ci.student[nrow(boot.ci.student) + 1, ] <- NA
      }
      # progress bar
      pb$tick(1)
    }
    return(list(BootCiStudent = boot.ci.student, BootSampling = BootSampling))
  }
