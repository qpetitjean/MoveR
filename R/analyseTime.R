#' @title perform analysis across fragments and time
#'
#'
#' @description Given a list of data frames containing tracking informations for each fragment (including the timeline)
#' and a custom function, this function perform the computation specified by the custom function(s) across time
#' and smooth it and returns a list containing as much data frame as the number of custom functions specified by
#' the customFunc argument. Each data frame includes a column indicating the timeline and the result of the computation across time.
#'
#'
#' @param trackDat A list of data frame containing tracking informations for each fragment (including a timeline).
#'
#' @param timeCol A character string corresponding to the name of the column containing time information.
#'
#' @param customFunc A function or a list of functions used to perform the computation across time.
#' NB: in case customFunc is a list of unnamed function it will try to retrieve their names by returning the first character string
#' following the function() call as the name of the results column.
#' 
#' @param Tinterval A vector containing two numeric values expressed in the timeline unit and
#' specifying the time interval on which the computation is performed (default is null, meaning the computation will be performed on the whole timeline).
#'
#' @param Tstep A numeric value expressed in the timeline unit and specifying the size of the
#' sliding window used to perform the computation (e.g., a value of 200, mean that for each sampling point, the computation
#' is performed using the 100 previous and the 100 next values).
#'
#' @param sampling A numeric value expressed in the timeline unit and specifying a subsampling used to
#' to perform the computation, allow to make computation faster, it hence determine the resolution of the
#' returned results (e.g., a value of 5000 mean that values will be computed every 5000 time units).
#'
#' @param wtd TRUE or FALSE, compute a weighed metric (TRUE) or not (FALSE) according to the length of the fragments (default is FALSE).
#'
#' @return this function returns a list containing as much data frame as the number of custom functions specified by
#' the customFunc argument. Each dataframe includes a column indicating the timeline according to timeCol and sampling arguments 
#' as well as the result of the computation performed according to the customFunc.
#'
#'
#' @authors Quentin PETITJEAN
#'
#'
#'
#' @examples
#'
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
#'drawFrags(fragsList,
#'          imgRes = c(max(MoveR::convert2list(fragsList)[["x.pos"]]),
#'                     max(MoveR::convert2list(fragsList)[["y.pos"]])),
#'          timeCol = "frame")
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
#'Smoothedtracks <- MoveR::analyseTime(
#'  trackDat = fragsListV1,
#'  timeCol = "frame",
#'  Tstep = 100,
#'  sampling = 50,
#'  wtd = TRUE,
#'  customFunc = list(
#'    MeanSpeed = function(x)
#'      mean(x[["speed"]], na.rm = T),
#'    MeanTurnAngle = function(x)
#'      mean(x[["TurnAngle"]], na.rm = T)
#'  )
#')
#'
#'# plot the results
#'par(mfrow = c(1, 2))
#'plot(Smoothedtracks[["MeanSpeed"]]$MeanSpeed ~ Smoothedtracks[["MeanSpeed"]]$frame, type = "l")
#'plot(Smoothedtracks[["MeanTurnAngle"]]$MeanTurnAngle ~ Smoothedtracks[["MeanTurnAngle"]]$frame, type = "l")
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
        "\n see resampleFrags()"
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
