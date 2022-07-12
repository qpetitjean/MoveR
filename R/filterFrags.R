#' @title apply a custom filter on tracking fragments
#'
#' @description  Given a list of data frame containing tracking information for each fragment and the result
#' of a specified condition' test as returned by filterFunc this function remove the values that do not meet the condition
#' of the test and split the fragments accordingly. Also by specifying an additional "minDur" argument, 
#' filtered fragment that have a low number of records can be removed. 
#' The function hence returns two sublist, the first containing a summary of the informations about the fragments before and after filtering
#' and, second a list containing the filtered fragments according to the condition test and "minDur" argument.
#'
#' @param trackDat A list of data frames containing tracking informations for each fragment.
#'
#' @param filter A list of vector as returned by filterFunc and containing the result of a condition test.
#'
#' @param splitCond The result of the condition test for which values have to be removed and fragments have to be splitted (Default: TRUE).
#'
#' @param minDur The minimum duration (i.e., number of records) to consider a fragment valid after the filtering (default = 1).
#'
#' @return This function returns a list containing two sublists: 
#'    \itemize{
#'          \item{"Summary_filtering": }{a list containing a summary of the informations about the fragments before and after filtering:
#'       \itemize{
#'          \item{"Fragnb_before_filter": }{the initial number of fragment.}
#'          \item{"Fragnb_after_filter": }{the number of remaining fragment after filtering, according to the "filter" argument.}
#'          \item{"Fragnb_after_minDur": }{the number of remaining fragment after filtering that have a number of records above "minDur" argument.}
#'          \item{"TotFragDuration_before_filter": }{the sum of the records belonging to each fragment before the filtering.}
#'          \item{"TotFragDuration_after_filter": }{the sum of the records belonging to each fragment after the filtering.}
#'          \item{"TotFragDuration_after_minDur": }{the sum of the records belonging to each fragment that have a number of records above "minDur" argument after the filtering.}
#'          \item{"%Data_kept_after_filter": }{the percent of records remaining afer the filtering.}
#'          \item{"%Data_kept_after_minDur": }{the percent of records remaining afer the filtering for the fragments that have a number of records above "minDur" argument only.}
#'       }}
#'          \item{"Cleaned_frags": }{a list of data frames containing the filtered fragments according to the condition test specified by filterFunc and "minDur" argument.}
#'         }
#'
#' @authors Quentin PETITJEAN
#'
#' @seealso \code{\link{filterFunc}}
#'
#' @examples
#' 
#'# load the sample data
#'Data <-
#'  readTrex(
#'    system.file("sampleData/sample_1/TREXOutput", package = "MovR"),
#'    mirrorY = T,
#'    imgHeight = 2160,
#'    rawDat = F
#'  )
#'# convert it to a list of fragments
#'trackDat <- convert2frags(Data[1:7], by = "identity")
#'
#'## Exemple 1: test for the presence of infinite value in x.pos,
#'  # if infinite values are detected, the result is TRUE 
#'
#'FiltInf <- filterFunc(trackDat, toFilter = "x.pos", customFunc = function(x) is.infinite(x))
#'
#'  # Then remove infinite values and split the fragments when an infinite value is found
#'  # here we keep every remaining fragment, whatever its duration (the number of record within each fragment)
#'
#'trackDat_NoInf <- filterFrags(trackDat, filter = FiltInf, splitCond = TRUE, minDur = 1)
#'
#'  # Check the summary of the filtering
#'  
#'str(trackDat_NoInf$Summary_filtering)
#'
#'  # access to the filtered fragment list
#'  
#'  trackDat_NoInf$Cleaned_frags
#'
#'  # alternatively, we can only keep the fragments with a duration above 10 (the number of record within each fragment) by modifying the minDur argument
#'
#'trackDat_NoInf_Dur10 <- filterFrags(trackDat, filter = FiltInf, splitCond = TRUE, minDur = 10)
#'
#'  # Check the summary of the filtering
#'  
#'str(trackDat_NoInf_Dur10$Summary_filtering)
#'
#'  # access to the filtered fragment list
#'  
#'  trackDat_NoInf_Dur10$Cleaned_frags
#'
#'## Exemple 2: test for the length of the individuals, 
#'  # if individual size is ranging between 1 and 20 pixels, the result is TRUE 
#'  
#'FiltSize <- filterFunc(trackDat, toFilter = "maj.ax", customFunc = function(x) x >= 1 & x <= 20)
#'
#'  # Then remove values that are not included within the [1:20] interval and split the fragments accordingly
#'  # here we keep every remaining fragment, whatever its duration (the number of record within each fragment)
#'
#'trackDat_Size20 <- filterFrags(trackDat, filter = FiltSize, splitCond = TRUE, minDur = 1)
#'
#'  # Check the summary of the filtering
#'  
#'str(trackDat_Size20$Summary_filtering)
#'
#'  # access to the filtered fragment list
#'  
#'  trackDat_Size20$Cleaned_frags
#'  
#'  # alternatively, we can only keep the fragments with a duration above 10 (the number of record within each fragment) by modifying the minDur argument
#'
#'trackDat_Size20_Dur10 <- filterFrags(trackDat, filter = FiltSize, splitCond = TRUE, minDur = 10)
#'
#'  # Check the summary of the filtering
#'  
#'str(trackDat_Size20_Dur10$Summary_filtering)
#'
#'  # access to the filtered fragment list
#'  
#'  trackDat_Size20_Dur10$Cleaned_frags
#'
#'
#' @export

filterFrags = function(trackDat,
                       filter,
                       splitCond = TRUE,
                       minDur = 1) {
  # compute some basic summary about fragments
  ## compute the number of fragments
  frags <- names(trackDat)
  nbFrag <- length(trackDat)
  ## compute the length (duration) of each fragment before filtering (expressed as the number of records)
  FragsDuration <- lapply(trackDat, function (x)
    nrow(x))
  ## compute total frags duration before filtering (expressed as the number of records)
  totFragsDuration <-  sum(unlist(FragsDuration))
  
  # create an empty list to append filtered fragment
  allCorrFrags <- list()
  # create an empty vector to store the number of new fragments made from the original
  nbNew <- vector()
  # create an empty vector to store the number of new fragments made from the original after filtering for minimum duration
  nbGood <- vector()
  # create an empty vector to store the duration of new fragments made from the original
  FragsDurationNew <- vector()
  # create an empty vector to store the duration of new fragments made from the original after filtering for minimum duration
  FragsDurationGood <- vector()
  
  # use custom function to filter fragments
  
  if (length(trackDat) > length(filter)) {
    stop(
      "trackDat and filter have different length: the result of the condition test is missing for some fragments"
    )
  } else if (length(trackDat) < length(filter)) {
    stop(
      "trackDat and filter have different length: the result of the condition test is longer than the number of fragments"
    )
  } else if (length(trackDat) == length(filter)) {
    # initialize progress bar
    total = length(frags)
    pb <-
      progress::progress_bar$new(format = "fragments processing [:bar] :current/:total (:percent)", total = total)
    pb$tick(0)
    Sys.sleep(0.001)
    
    for (i in frags) {
      ## in case there is NA in replace it by the opposite of splitCond
      filter[[i]][is.na(filter[[i]])] <- !splitCond
      ## split each fragment according to the filter and the specified condition
      newFrags <-
        split(trackDat[[i]], cumsum(filter[[i]] == splitCond))
      ## remove the first row of each new fragment when it correspond to an element which does not respect condition
        if (length(newFrags) > 1) {
        newfrags_l1 <- newFrags[[1]]
        newFrags <- lapply(newFrags[2:max(length((newFrags)))], function(x)
          x[-1, ])
        newFrags[["0"]] <- newfrags_l1
        newFrags <- newFrags[sort(names(newFrags))]
      }
      ## in case there is empty new fragment removed it
      newFrags <-
        newFrags[sapply(newFrags, function(x)
          nrow(x)) > 0]
      ## filter the new fragments by minDur and append them to the output
      ### in case the filtering step does not return new fragment
      if (length(newFrags) == 0) {
        nbNew_temp <- 0
        nbNew <- c(nbNew, nbNew_temp)
        FragsDurationNew_temp <- 0
        FragsDurationNew <-
          c(FragsDurationNew, FragsDurationNew_temp)
        allCorrFrags <- allCorrFrags
        
        w1 <-
          paste("For", i, sep = " ")
        w2 <-
          "\nno remaining fragment after filtering, no fragment returned"
        warning(w1, ": ", w2)
        
      }
      ### in case the filtering step does return new fragments
      else {
        names(newFrags) <- seq(length(newFrags))
        nbNew_temp <- length(newFrags)
        nbNew <- c(nbNew, nbNew_temp)
        newSizes <- lapply(newFrags, function(x)
          nrow(x))
        FragsDurationNew_temp <- sum(unlist(newSizes))
        FragsDurationNew <-
          c(FragsDurationNew, FragsDurationNew_temp)
        
        ## keep only new fragments with a sufficient duration
        goodFrags <- which(newSizes >= minDur)
        
        #### in case the filtering by minDur does not return new fragments
        if (length(goodFrags) == 0) {
          nbGood_temp <- 0
          nbGood <- c(nbGood, nbGood_temp)
          allCorrFrags <- allCorrFrags
          
          w1 <-
            paste("For", i, sep = " ")
          w2 <-
            "\nAll the new fragments created after filtering are shorter than minDur, no fragments returned"
          warning(w1, ": ", w2)
          
        }
        #### in case the filtering by minDur does return new fragments
        else {
          nbGood_temp <- length(goodFrags)
          nbGood <- c(nbGood, nbGood_temp)
          FragsDurationGood_temp2 <- vector()
          for (j in goodFrags) {
            allCorrFrags <- c(allCorrFrags, list(newFrags[[j]]))
            FragsDurationGood_temp <- c(dim(newFrags[[j]])[1])
            FragsDurationGood_temp2 <-
              c(FragsDurationGood_temp2,
                FragsDurationGood_temp)
          }
          FragsDurationGood <-
            c(FragsDurationGood, FragsDurationGood_temp2)
        }
      }
      
      # progress bar
      pb$tick(1)
      Sys.sleep(1 / 1000)
    }
    
    # rename new fragments
    if (length(allCorrFrags) == 0) {
      allCorrFrags <- NULL
    } else{
      names(allCorrFrags) <-
        paste("frags", (seq(length(allCorrFrags))), sep = "_")
    }
  }
  
  return(c(
    list(
      Summary_filtering = list(
        Fragnb_before_filter = nbFrag,
        Fragnb_after_filter = sum(nbNew),
        Fragnb_after_minDur = sum(nbGood),
        TotFragDuration_before_filter = totFragsDuration,
        TotFragDuration_after_filter = sum(FragsDurationNew),
        TotFragDuration_after_minDur = sum(FragsDurationGood),
        "%Data_kept_after_filter" =  sum(FragsDurationNew) / totFragsDuration * 100,
        "%Data_kept_after_minDur" =  sum(FragsDurationGood) / totFragsDuration * 100
      ),
      Cleaned_frags = allCorrFrags
    )
  ))
}