#' @title Bouts analyses
#'
#'
#' @description Given a list of data frames containing tracking informations and a vector 
#' containing behavioral patterns (e.g., behavioral states, location in areas) for each fragment, 
#' the function will look for the specified pattern and compute some simple metrics about it
#' 
#'
#' @param trackDat A list of data frame containing tracking informations for each fragment
#' 
#' @param Bstate The name of a vector/df column present in each df of trackDat and indicating the state of the individual
#' along a fragment
#' 
#' @param Time The name of a vector/df column present in each df of trackDat and indicating the time
#' of each Bstate along the fragment (optional)
#' 
#' @param pattern A character string containing a regular expression (or character string for fixed = TRUE) 
#' to be matched in the Bstate vector. see gregexpr()
#'
#' @return this function returns a new list of dataframe corresponding to each new fragments with the result of customFunc appended
#'
#'
#' @authors Quentin Petitjean
#'
#'
#' @examples
#'
#' #TODO
#'
#' @export

analyseBouts <- function(trackDat, Bstate = NA, Time = NA, pattern = NA) {
  # initialize results list 
  boutsList <- list()
  # initialize progress bar
  total = length(trackDat)
  pb <-
    progress::progress_bar$new(format = "fragments processing [:bar] :current/:total (:percent)", total = total)
  pb$tick(0)
  Sys.sleep(0.001)
  # loop trough the fragment list
  for(i in seq(length(trackDat))) {
    if(is.na(pattern)){ 
      stop("Pattern argument is missing: 
           please specify a pattern to look for in Bstate vector")
    }
    if(is.null(list_get(trackDat[[i]], Bstate))){ 
      stop("Bstate argument is missing or is not found in the fragments list:
          No vector where to look for the pattern")
    }
    
    if(0 %in% list_get(trackDat[[i]], Bstate)){ 
      stop("Bstate must not contain zeros (0), as it is an internal value used by the function
           to specify when the pattern is missing, consider recoding Bstate")
    }
    # specify the pattern and identify its location
    PattRmatch <-
      regmatches(trackDat[[i]][[Bstate]],
                 gregexpr(pattern, trackDat[[i]][[Bstate]]))
    # when the pattern is not found length = 0 replace per NA
    PattRmatch <-
      unlist(lapply(PattRmatch, function(x)
        if (length(x) == 0) {
          0
        } else {
          x
        }))
    # paste the frame to the pattern identification in order to detect when changes occur
    if(is.null(list_get(trackDat[[i]], Time))){ 
      warning("In ", names(trackDat[i]), ": ", 
              "Time argument is missing or is not found in the fragments list, 
      No time vector to identify starting and ending time for the bouts")
      PattRmatch <-
        data.frame(bouts = PattRmatch, time = rep(NA, length(PattRmatch)))
    } else { 
      PattRmatch <-
        data.frame(bouts = PattRmatch, time = trackDat[[i]][[Time]])}
    # split the different patterns
    bouts <- split(PattRmatch,
                   cumsum(c(0, diff(
                     as.numeric(PattRmatch$bouts)
                   ) != 0)))
    # extract patterns ID
    boutsId <- unlist(lapply(bouts, function (x)
      unique(x$bouts)))
    # extract patterns length
    boutsL <- lapply(bouts, nrow)
    # extract starting time for each pattern
    boutsSart <- lapply(bouts, function(x)
      x$time[1])
    # extract ending time for each pattern
    boutsEnd <- lapply(bouts, function(x)
      x$time[nrow(x)])
    # groups patterns Id, starting and ending time as well as patterns length in a df
    boutsSEL <-
      data.frame(
        boutsId = boutsId,
        Start = unlist(boutsSart),
        End = unlist(boutsEnd),
        Length = unlist(boutsL)
      )
    # compute total length of each pattern category
    boutsDur <-
      stats::setNames(data.frame(matrix(
        ncol = 2, nrow = length(unique(boutsSEL$boutsId))
      )), c("boutsId", "TotLength"))
    for (j in seq(length(unique(boutsSEL$boutsId)))) {
      boutsDur[j, 1] <- unique(boutsId)[j]
      boutsDur[j, 2] <-
        sum(boutsSEL[boutsSEL$boutsId == unique(boutsSEL$boutsId)[j],]$Length)
    }
    # compute the occurrence of each pattern
    boutsn <-
      data.frame(table(boutsId))
    # groups patterns Id, occurrence and total length in a df
    boutsC <-
      data.frame(boutsId = boutsn[1],
                 n = boutsn[2],
                 TotLength = boutsDur$TotLength)
    # groups all the results in a list
    boutsTot <- list(bouts_count = data.frame(boutsC),
                     bouts_summary = boutsSEL)
    
    boutsList[[names(trackDat[i])]] <- boutsTot
    # progress bar
    pb$tick(1)
    Sys.sleep(1 / 1000)
  }
  return(boutsList)
}