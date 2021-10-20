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
    if (is.null(timeCol) |
        is.null(timeCol %in% unlist(lapply(trackDat, names)))) {
      stop(
        "timeCol argument is missing or is not found in the provided dataset, timeCol might be misspelled"
      )
    }
    if (is.null(customFunc)) {
      stop("customFunc argument is missing, a customFunc is needed to compute metric")
    }
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
      Newtimeline[length(Newtimeline) + 1] <-
        timeline[length(timeline)]
      Newtimeline[which(Newtimeline == 0)] <- 1
    } else {
      # define the timeline
      timeline <-
        seq(
          from = round(Tinterval[1] - ((Tstep - 1) / 2)),
          to = round(Tinterval[2] + ((Tstep - 1) / 2)),
          by = 1
        )
      # compute sliding mean every n time unit (according to "sampling" parameter) allow to make computation faster
      Newtimeline <- seq(from = Tinterval[1],
                         to = Tinterval[2],
                         by = sampling)
      Newtimeline[length(Newtimeline) + 1] <- Tinterval[2]
      Newtimeline[which(Newtimeline == 0)] <- 1
    }
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
    # initialize result list
    smoothed <- list()
    for (name in names(customFunc)) {
      temp_df <-
        data.frame(matrix(NA, nrow = length(Newtimeline), ncol = 2))
      colnames(temp_df) <- c(name, timeCol)
      smoothed[[name]] <- temp_df
    }
    # initialize progress bar
    total = length(Newtimeline)
    pb <-
      progress::progress_bar$new(format = "frame processing [:bar] :current/:total (:percent)", total = total)
    pb$tick(0)
    
    # loop trough fragments part to compute metrics according to customFunc
    for (i in Newtimeline) {
      # Select Time interval according to the specified Tstep and extract the concerned fragments part
      # since we use a sliding mean, the time values below the minimum time value detected in the dataset as well as
      # when i - (Tstep-1)/2 is below the first value of the timeline or i + (Tstep-1)/2 is above the last value
      # of the timeline, it result in NA
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
                           ))) > timeline[length(timeline)]))) {
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
        # compute the metric mean on a given Time interval (across fragments)
        ## in case weighed argument is FALSE, compute a simple mean
        if (wtd == FALSE) {
          smoothed_temp <- lapply(Res, function(x)
            mean(x, na.rm = T))
        } else if (wtd == TRUE) {
          ## in case weighed argument is TRUE, compute a weighed mean according to fragment length (Timecol)
          # create an equivalent of na.rm = T and compute the weighed mean for each metric
          Reslen <-
            lapply(names(Res), function(x)
              data.frame(Res = Res[[x]], len = len[[x]]))
          names(Reslen) <- names(Res)
          Reslen <- lapply(Reslen, function(x)
            na.omit(x))
          smoothed_temp <-
            lapply(Reslen, function(x)
              sum(x$len * x$Res) / sum(x$len))
        }
        # append the results in the smoothed list
        for (n in names(smoothed)) {
          smoothed[[n]][which(Newtimeline == i), ] <- c(smoothed_temp[[n]], i)
        }
      } else {
        # append the results in the smoothed list
        for (n in names(smoothed)) {
          smoothed[[n]][which(Newtimeline == i), ] <- c(NA, i)
        }
      }
      # progress bar
      pb$tick(1)
    }
    return(smoothed)
  }