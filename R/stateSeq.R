#' @title Extract specified patterns/sequences from fragments.
#'
#' @description Given a list of data frames containing tracking informations including a vector
#' containing behavioral patterns (e.g., behavioral states, location in areas) for each fragment,
#' the function detect and extracts the fragment parts that contains the specified pattern.
#'
#'
#' @param trackDat A list of data frame containing tracking informations for each fragment, including a vector
#' containing behavioral patterns (e.g., behavioral states, location in areas).
#'
#' @param Bstate The name of the column indicating the state of the particle along each fragment.
#'
#' @param pattern A character string containing a regular expression to look for in the Bstate vector (see \code{\link[base]{grep}}).
#'
#' @param perl A logical value indicating whether Perl-like regular expression should be used (default = FALSE, see \code{\link[base]{regex}}).
#'
#' @param fixed A logical value indicating whether a literal regular expression (exact match) should be used (default = FALSE, see \code{\link[base]{regex}}).
#'
#' @return This function returns a list containing the part of the fragments corresponding to the specified pattern.
#' The part of the fragments corresponding to the specified pattern are grouped into a list named according to the id of the original fragment
#' (e.g. the first detected pattern extracted from the first fragment is located in the sublist named frags_1 and is named frags_1.1).
#'
#' @author Quentin PETITJEAN
#'
#' @examples
#'
#'# TODO
#'
#' @export

stateSeq <-
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
        "Bstate column is misspelled or is missing from fragment list, a column containing behavioral state information is needed to find the specified pattern"
      )
    }
    if (is.null(pattern)) {
      stop(
        "pattern argument is missing, a regular expression is needed to specify which part of the fragments have to be extracted"
      )
    }
    # transform the vector indicating the state of the individual as character
    # string for each fragment
    toMatch <-
      lapply(trackDat, function(x) {
        paste(x[[Bstate]], collapse = "")
      })
    # use the specified regex to look for the pattern within the toMatch list of vectors belonging to each fragment
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
    # for each fragment
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
    # For each fragment, retrieve the matched pattern and corresponding data for each fragment
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
    return(stateSeq)
  }