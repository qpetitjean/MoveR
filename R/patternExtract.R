#' @title Extract specified patterns from fragments.
#'
#' @description Given a list of data frames containing tracking informations including a vector 
#' containing behavioral patterns (e.g., behavioral states, location in areas) for each fragment, 
#' the function extracts fragments part that contains the specified pattern.
#' 
#'
#' @param trackDat A list of data frame containing tracking informations for each fragment, including a vector 
#' containing behavioral patterns (e.g., behavioral states, location in areas).
#' 
#' @param Bstate The name of the column indicating the state of the individual along each fragment.
#' 
#' @param pattern A character string containing a regular expression to be matched in the Bstate vector. see gregexpr().
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

patternExtract <-
  function(trackDat,
           Bstate = NULL,
           pattern = NULL) {
    if (is.null(Bstate)) {
      stop("Bstate argument is missing, a column containing behavioral state information is needed to find the specified pattern")
    }
    if (is.null(unlist(lapply(trackDat, function(x) {listGet(x, Bstate)})))) {
      stop("Bstate column is misspelled or is missing from fragment list, a column containing behavioral state information is needed to find the specified pattern")
    }
    if (is.null(pattern)) {
      stop("pattern argument is missing, a regular expression is needed to specify which part of the fragments have to be extracted")
    }
    # transform the vector indicating the state of the individual as character
    # string for each fragment
    toMatch <-
      lapply(trackDat, function(x) {
        paste(x[[Bstate]], collapse = "")
      })
    # use the specified regex to look for the pattern within the toMatch vector
    # for each fragment
    matchPos <- lapply(toMatch, function(x) {
      gregexpr(pattern, x)
    })
    if(length(unlist(lapply(matchPos, function(x) {
      which(x[[1]][[1]] > 0)}))) == 0) {
      stop("The specified pattern is not found in ", "'",Bstate,"'", " perhaps the pattern should be modified")
    }
    # extract the position of the start of each detected pattern and their length
    # for each fragment
    starts <- lapply(matchPos, function(x) {
      x[[1]][1:length(x[[1]])]
    })
    len <- lapply(matchPos, function(x) {
      attr(x[[1]], 'match.length')
    })
    # For each fragment, create a bunch of sublists containing the position of each value to extract
    Positions <-
      lapply(names(starts), function(x) {
        lapply(seq(length(starts[[x]])), function(y) {
          c(starts[[x]][y]:(starts[[x]][y] + (len[[x]][y] - 1)))
        })
      })
    names(Positions) <- names(starts)
    # For each fragment, extract the part that match the pattern
    extractPattern <- lapply(names(Positions), function(x) {
      lapply(seq(length(Positions[[x]])), function(y) {
        trackDat[[x]][c(Positions[[x]][[y]]),]
      })
    })
    # name each sublist according to the fragments they belong to
    names(extractPattern) <- names(Positions)
    sublistNames <- lapply(names(extractPattern), function(y){paste(y, ".", seq(length(extractPattern[[y]])), sep="")})
   for(i in seq(length(extractPattern))){
     names(extractPattern[[i]]) <- sublistNames[[i]]
   }
    return(extractPattern)
  }
