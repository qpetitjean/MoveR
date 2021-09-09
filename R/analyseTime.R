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
#' @param wtd TRUE or FALSE, compute a weighed metric (TRUE) or not (FALSE), (default is FALSE)
#'
#' @return this function returns a vector containing the smoothed values
#' computed according to the custom function across time
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

analyseTime <-
  function(trackDat,
           timeCol = NULL,
           customFunc = NULL,
           Tinterval = NULL,
           Tstep = 1,
           sampling = 1, 
           wtd = FALSE) {
  
    if (is.null(timeCol) | is.null("runTimelinef" %in% unlist(lapply(trackDat, names)))) {
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
    }
    
    # initialize result vector
    smoothed <- vector()
    # initialize progress bar
    total = length(Newtimeline)
    pb <-
      progress::progress_bar$new(format = "frame processing [:bar] :current/:total (:percent)", total = total)
    pb$tick(0)
    
    # loop trough fragments part to compute metrics according to customFunc
    for (i in Newtimeline) {
      # Select Time interval according to the specified Tstep and extract the concerned fragments part
      # since we use a sliding mean, the time values below Tstep/2 result in NA
      if (!round((i - ((Tstep - 1) / 2))) < timeline[1] &
          !round((i + ((Tstep - 1) / 2))) > Newtimeline[length(Newtimeline)]) {
        selVal <-
          timeline[which(timeline ==  round((i -((
            Tstep - 1
          ) / 2)))):which(timeline == round((i + ((
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
          df <- trackDat[[j]][c(WhoWhen[[j]]), ]
          Res_temp <- customFunc(df)
          Res <- c(Res, Res_temp)
          len_temp <- length(df[[timeCol]])
          len <- c(len, len_temp)
        }
        # compute the metric mean on a given Time interval (across fragments)
        ## in case weighed argument is FALSE, compute a simple mean
        if(wtd == FALSE){ 
          smoothed_temp <- mean(Res, na.rm = T)
        } else if (wtd == TRUE) { 
          ## in case weighed argument is TRUE, compute a weighed mean according to fragment length (Timecol)
          # create an equivalent of na.rm = T and compute the weighed mean
          na.rm <- !is.na(Res) & !is.na(len)
          Res <- Res[na.rm]
          len <- len[na.rm]
          smoothed_temp <- sum(len * Res) / sum(len) 
            }
        smoothed <- c(smoothed, smoothed_temp)
      } else {
        smoothed <- c(smoothed, NA)
      }
      # progress bar
      pb$tick(1)
    }
    return(smoothed)
  }