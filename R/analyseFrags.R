#' @title Fragments analyses
#'
#'
#' @description Given a list of data frames containing tracking informations for each fragment and
#' a custom function, this function iterate trough the fragments lists to perform the specified computation 
#' and returns the original list of data frames with the result of the analysis appended.
#'
#' @param trackDat A list of data frame containing tracking informations for each fragment.
#'
#' @param customFunc A function or a list of functions used to perform the computation along all fragments
#' NB: in case customFunc is a list of unnamed function it will try to retrieve their names by returning the first character string
#' following the function() call as the name of the results column.
#'
#' @return this function returns the original list of data frames (i.e., fragments) 
#' with the result of the specified computation appended.
#'
#'
#' @authors Quentin PETITJEAN
#'
#'
#' @examples
#'
#'# generate some dummy fragments
#'## start to specify some parameters to generate fragments
#'Fragn <- 50 # the number of fragment to simulate
#'FragL <- 100:1000 # the length of the fragments or a sequence to randomly sample fragment length
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
#'          imgRes = c(max(convert2list(fragsList)[["x.pos"]]),
#'                     max(convert2list(fragsList)[["y.pos"]])),
#'          timeCol = "frame")
#'
#'# Run some computation on the dataset using analyseFrags
#'fragsListV1 <-
#'  analyseFrags(
#'    fragsList,
#'    customFunc = list(
#'      # specify a first function to compute speed over each fragment (a modulus present within the MoveR package)
#'      speed = function(x)
#'        MoveR::speed(
#'          x,
#'          TimeCol = "frame",
#'         scale = 1,
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
#'# check the result for the first fragment
#'str(fragsListV1[["1"]])
#'
#'# plot the histogram of the speed
#'hist(convert2list(fragsListV1)[["speed"]])
#'
#'# plot the histogram of the turning angle
#'Ht = circular::circular(
#'  convert2list(fragsListV1)[["TurnAngle"]],
#'  type = "angle",
#'  units = "radians",
#'  zero = 0
#')
#'circular::rose.diag(
#'  Ht,
#'  bins = 24,
#'  shrink = 0.89,
#'  xlim = c(-1, 1),
#'  ylim = c(-1, 1),
#'  prop = 2,
#'  col = "gray",
#'  border = "black",
#'  units = 'radians',
#'  ticks = TRUE
#')
#'
#' @export

analyseTracks<- function(trackDat, customFunc) {
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
  # loop trough the fragment list and append the result to each list by using the name of the custom function
  total = length(trackDat)
  pb <-
    progress::progress_bar$new(format = "fragments processing [:bar] :current/:total (:percent)", total = total)
  pb$tick(0)
  Sys.sleep(0.001)
  # if trackDat is an unnamed list of fragment use the position of the fragment in the list as fragment name
  if (is.null(names(trackDat))) {
    names(trackDat) <- seq(length(trackDat))
  }
  for (i in names(trackDat)) {
    for (j in names(customFunc)) {
      if (!inherits(try(customFunc[[j]](trackDat[[i]]), silent = T)
                    , "try-error") &
          !class(customFunc[[j]](trackDat[[i]])) == "function") {
        trackDat[[i]][[j]] <- customFunc[[j]](trackDat[[i]])
      } else if (inherits(try(customFunc[[j]](trackDat[[i]]), silent = T)
                          , "try-error") |
                 class(customFunc[[j]](trackDat[[i]])) == "function") {
        trackDat[[i]][[j]] <- NA
        warning(
          "For customFunc ",
          j ,
          " in fragment ",
          i ,
          " : \nanalyseFrags returned NA, perhaps check customFunc argument"
        )
      }
    }
    # progress bar
    pb$tick(1)
    Sys.sleep(1 / 1000)
  }
  
  return(trackDat)
}
