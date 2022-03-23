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
        "\n see MovR::resampleFrags()"
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