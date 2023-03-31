#' @title Identify and extract specified patterns/sequences from tracklets trajectories.
#'
#' @description Given a list of data frames containing tracking informations including a vector
#' containing behavioral patterns (e.g., behavioral states, location in areas) for each tracklet,
#' the function identify and extract tracklets parts that contains the specified pattern.
#'
#'
#' @param trackDat A list of data frame containing tracking informations for each tracklet, including a vector
#' containing behavioral patterns (e.g., behavioral states, location in areas).
#'
#' @param Bstate The name of the column indicating the state of the particle along each tracklet.
#'
#' @param pattern A character string containing a regular expression to look for in the Bstate vector (see \code{\link[base]{grep}}).
#'
#' @param perl A logical value (i.e., TRUE or FALSE) indicating whether Perl-like regular expression should be used (default = FALSE, see \code{\link[base]{regex}}).
#'
#' @param fixed A logical value (i.e., TRUE or FALSE) indicating whether a literal regular expression (exact match) should be used (default = FALSE, see \code{\link[base]{regex}}).
#'
#' @return This function returns a list containing the part of the tracklets corresponding to the specified pattern.
#' The part of the tracklets corresponding to the specified pattern are grouped into a list named according to the id of the original tracklet
#' (e.g. the first detected pattern extracted from the first tracklet is located in the sublist named frags_1 and is named frags_1.1).
#'
#' @author Quentin PETITJEAN
#'
#' @examples
#'
#'# TODO
#'
#' @export

IdStateSeq <-
  function(trackDat,
           Bstate = NULL,
           pattern = NULL,
           perl = FALSE,
           fixed = FALSE) {
    if (is.null(Bstate)) {
      stop(
        "Bstate argument is missing, a column containing behavioral state information is needed to find the specified pattern"
      )
    }
    if (is.null(unlist(lapply(trackDat, function(x) {
      MoveR::listGet(x, Bstate)
    })))) {
      stop(
        "Bstate column is misspelled or is missing from tracklet list, a column containing behavioral state information is needed to find the specified pattern"
      )
    }
    if (is.null(pattern)) {
      stop(
        "pattern argument is missing, a regular expression is needed to specify which part of the tracklets have to be extracted"
      )
    }
    # transform the vector indicating the state of the individual as character
    # string for each tracklet
    toMatch <-
      lapply(trackDat, function(x) {
        paste(x[[Bstate]], collapse = "")
      })
    # use the specified regex to look for the pattern within the toMatch list of vectors belonging to each tracklet
    matchPos <- lapply(toMatch, function(x) {
      gregexpr(pattern,
               x,
               fixed = fixed,
               perl = perl)
    })
    
    if (length(unlist(lapply(matchPos, function(x) {
      which(x[[1]][[1]] > 0)
    }))) == 0) {
      stop(
        "The specified pattern is not found in ",
        "'",
        Bstate,
        "'",
        " perhaps the pattern should be modified"
      )
    }
    # extract the position of the start of each detected pattern and their length (including the separator)
    # for each tracklet
    starts <- lapply(matchPos, function(x) {
      unlist(lapply(x, function(y)
        as.list(y)),
        recursive = F)
    })
    
    len <- lapply(matchPos, function(x) {
      unlist(lapply(x, function(y)
        as.list(attr(
          y, 'match.length'
        ))),
        recursive = F)
    })
    # For each tracklet, retrieve the matched pattern and corresponding data for each tracklet
    Positions <-
      stats::setNames(lapply(names(starts), function(x) {
        lapply(seq(length(starts[[x]])), function(y) {
          Pos <- c(starts[[x]][[y]]:(starts[[x]][[y]] + (len[[x]][[y]] - 1)))
          TruePos <-
            cumsum(unlist(lapply(strsplit(trackDat[[x]][[Bstate]], ""), length)))
          Res <- trackDat[[x]][(TruePos %in% Pos),]
          return(Res)
        })
      }), names(starts))
    # rename the sublists
    stateSeq <-
      stats::setNames(lapply(names(Positions), function(x) {
        stats::setNames(Positions[[x]],
                        paste0(x,
                               ".",
                               seq(length(Positions[[x]]))))
      }), names(Positions))
    # remove empty list before returning the result
    stateSeq <- lapply(stateSeq, function(x) Filter(nrow, x))
    stateSeq <- Filter(length, stateSeq)
    return(stateSeq)
  }
