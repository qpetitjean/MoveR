#' @title apply custom filter on tracking data
#'
#'
#' @description Given a list of data frame containing tracking informations for each fragment and the result
#' of a specific condition test as returned by filterFunc this function returns a list of dataframes
#' corresponding to new fragments
#'
#' @seealso \code{\link{filterFunc}}
#'
#' @param trackDat a list of data frame containing tracking informations for each fragment
#' (e.g., maj.ax, angle, min.ax, x.pos, y.pos, ...)
#'
#' @param filter a list of vector as returned by filterFunc and containing the result of a condition test
#'
#' @param splitCond the result of the condition test for which fragments have to be splitted (TRUE or FALSE)
#'
#' @param minDur the minimum duration of the fragment to keep after splitting, expressed in frame
#'
#' @return this function returns a new list of dataframe corresponding to each new fragments created based on the result of filterFunc
#'
#'
#' @authors Quentin Petitjean, Vincent Calcagno
#'
#'
#'
#' @examples
#'
#' #TODO
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
  ## compute the length (duration) of each fragment before filtering (in frames)
  FragsDuration <- lapply(trackDat, function (x)
    dim(x)[1])
  ## compute total frags duration before filtering (in frame)
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
      "trackDat and filter have different length: condition test is missing for some fragments"
    )
  } else if (length(trackDat) < length(filter)) {
    stop(
      "trackDat and filter have different length: there is more condition test than fragments"
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
          dim(x)[1]) > 0]
      ## append filter the new fragments by mindur and append them to the output
      ### in case the filtration step does not return new fragment
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
          "\nno new fragment left after filtration, no fragments returned"
        warning(w1, ": ", w2)
        
      }
      ### in case the filtration step does return new fragments
      else {
        names(newFrags) <- seq(length(newFrags))
        nbNew_temp <- length(newFrags)
        nbNew <- c(nbNew, nbNew_temp)
        newSizes <- lapply(newFrags, function(x)
          dim(x)[1])
        FragsDurationNew_temp <- sum(unlist(newSizes))
        FragsDurationNew <-
          c(FragsDurationNew, FragsDurationNew_temp)
        
        ## keep only new fragments with a sufficient duration
        goodFrags <- which(newSizes >= minDur)
        
        #### in case the filtration by minDur does not return new fragments
        if (length(goodFrags) == 0) {
          nbGood_temp <- 0
          nbGood <- c(nbGood, nbGood_temp)
          allCorrFrags <- allCorrFrags
          
          w1 <-
            paste("For", i, sep = " ")
          w2 <-
            "\nAll the new fragments created after filtration are shorter than minDur, no fragments returned"
          warning(w1, ": ", w2)
          
        }
        #### in case the filtration by minDur does return new fragments
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
      Summary_filtration = list(
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
