#' @title Compute the total surface explored and display the corresponding heatmap.
#'
#' @description Given a list of tracking tracklets containing cartesian coordinates, and a value of
#'  reaction distance this function displays an heatmap (i.e., hexbin plot) of the explored areas and returns the
#'  total surface explored in a geometrically consistent and scalable manner.
#'
#' @param trackDat A list of data frame containing tracking information for each tracklet (including a timeline)
#' or a data frame containing tracking information for one tracklet.
#'
#' @param binRad A numeric value, expressed in pixels, corresponding to the diameter of the typical surface a particle can "explore" around its position.
#'
#' @param imgRes A vector of 2 numeric values, the resolution of the video used as x and y limit of the plot (i.e., the number of pixels in image width and height).
#' If imgRes is unspecified, the function retrieve it using x and y maximum values + 5%.
#'
#' @param scale A ratio corresponding to the scaling factor to be applied to the trajectory coordinates (optional).
#'
#' @param timeCol A character string corresponding to the name of the column containing Time information (e.g., "frame").
#'
#' @param timeWin  A list of one or several vector containing 2 numeric values separated by a comma
#' corresponding to the time interval between which the tracklets have to be drawn according to timeCol (optional).
#'
#' @param graph A logical value (i.e., TRUE or FALSE) indicating whether the various diagnostics plots should be displayed or not (default = TRUE).
#'
#' @param saveGraph A logical value (i.e., TRUE or FALSE) indicating whether the heatmap should be saved as a .tiff file in the hardrive - within the working directory (default = FALSE).
#' Alternatively, the user can specify the saving function and hence the extension and the
#' directory where the heatmap should be saved (see for instance \code{\link[grDevices]{png}}, \code{\link[grDevices]{jpeg}} or \code{\link[grDevices]{tiff}}.
#'
#' @return This function returns the total surface explored in a geometrically consistent and scalable manner and if
#' graph argument is TRUE, it displays an heatmap (i.e., hexbin plot) of the explored areas.
#'
#'
#' @author Quentin PETITJEAN
#'
#' @examples
#'
#' set.seed(2023)
#' # generate some dummy tracklets
#' ## start to specify some parameters to generate tracklets
#' TrackN <- 40 # the number of tracklet to simulate
#' TrackL <-
#'   1:1000 # the length of the tracklets or a sequence to randomly sample tracklet length
#' id <- 0
#' TrackList <- stats::setNames(lapply(lapply(seq(TrackN), function(i)
#'   trajr::TrajGenerate(sample(TrackL, 1), random = TRUE, fps = 1)), function(j) {
#'     id <<- id + 1
#'     data.frame(
#'       x.pos = j$x - min(j$x),
#'       y.pos = j$y - min(j$y),
#'       frame = j$time,
#'       identity = paste("Tracklet", id, sep = "_")
#'     )
#'   }), seq(TrackN))
#'
#' # check the tracklets
#' MoveR::drawTracklets(TrackList,
#'                  timeCol = "frame")
#'
#' # compute the total surface explored and displays the heatmap for all tracklets
#' # NB: Plot can be displayed and/or saved using graph and saveGraph arguments
#' MoveR::exploredArea(TrackList,
#'                     binRad = 20,
#'                     scale = 1,
#'                     timeCol = "frame")
#'
#' # compute the surface explored for each tracklet and display (but do not save) the heatmap for the 2 first tracklets
#' # by combining exploredArea and analyseTracklets, the surface explored for each tracklet is appended to the data
#' # of the corresponding tracklets.
#' ExplorTest <- MoveR::analyseTracklets(TrackList[1:2],
#'                                   customFunc = list(
#'                                     exploredArea = function(x)
#'                                       MoveR::exploredArea(
#'                                         x,
#'                                         binRad = 20,
#'                                         scale = 1,
#'                                         timeCol = "frame"
#'                                       )
#'                                   ))
#' str(ExplorTest)
#'
#' @export

exploredArea <-
  function(trackDat,
           binRad = NULL,
           imgRes = c(NA, NA),
           scale = 1,
           timeCol = "frame",
           timeWin = list(c(0, Inf)),
           graph = TRUE,
           saveGraph = FALSE) {
    if (is.null(binRad)) {
      stop(
        "binRad argument is missing, \na numeric value specifying the diameter of the cell a particle can explore is needed to compute the surface explored"
      )
    }
    
    # if imgRes is unspecified retrieve it approximately using the maximum value in x and y coordinates
    if (class(trackDat) == "list" & length(trackDat) >= 1) {
      if (TRUE %in% is.na(imgRes)) {
        xCoords <- unlist(lapply(trackDat, function(x)
          MoveR::listGet(x, "x.pos")))
        if (length(which(is.infinite(xCoords)) > 0)) {
          xCoords <- xCoords[!is.infinite(xCoords)]
        }
        width <- round(max(xCoords) + 5 * max(xCoords) / 100, 0)
        
        yCoords <- unlist(lapply(trackDat, function(x)
          MoveR::listGet(x, "y.pos")))
        if (length(which(is.infinite(yCoords)) > 0)) {
          yCoords <- yCoords[!is.infinite(yCoords)]
        }
        height <- round(max(yCoords) + 5 * max(yCoords) / 100, 0)
        imgRes <- c(width, height)
      }
    } else{
      if (TRUE %in% is.na(imgRes)) {
        xCoords <- trackDat[["x.pos"]]
        if (length(which(is.infinite(xCoords)) > 0)) {
          xCoords <- xCoords[!is.infinite(xCoords)]
        }
        width <- round(max(xCoords) + 5 * max(xCoords) / 100, 0)
        
        yCoords <- trackDat[["y.pos"]]
        if (length(which(is.infinite(yCoords)) > 0)) {
          yCoords <- yCoords[!is.infinite(yCoords)]
        }
        height <- round(max(yCoords) + 5 * max(yCoords) / 100, 0)
        imgRes <- c(width, height)
      }
    }
    # Select only the part of the tracklets included in the selected time Window
    if (!is.list(timeWin)) {
      stop(
        "timeWin argument should be a list of vector(s) containing starting and ending value of each time window interval"
      )
    }
    if (max(unlist(lapply(timeWin, length))) > 2) {
      stop(
        "timeWin argument contains a vector of length > 2, \ntimeWin should be a list of vector(s) containing 2 values (start and end of the time window interval)"
      )
    }
    InfLoc <-
      length(unlist(lapply(lapply(timeWin, function(x)
        which(x == Inf)), function(y)
          length(y) > 0)))
    if (length(InfLoc) > 0) {
      if (class(trackDat) == "list" & length(trackDat) >= 1) {
        timeWin[[InfLoc]][which(timeWin[[InfLoc]] == Inf)] <-
          max(unlist(lapply(trackDat, function(x)
            max(MoveR::listGet(x, timeCol)))))
      } else{
        # in case the input trackDat contains value for only one tracklet (a dataframe)
        timeWin[[InfLoc]][which(timeWin[[InfLoc]] == Inf)] <-
          max(MoveR::listGet(trackDat, timeCol), na.rm = T)
      }
    } else {
      timeWin = timeWin
    }
    # select the part of the tracklets that are included in timeWin
    WhoWhen <- lapply(seq(length(timeWin)),
                      function(p)
                        MoveR::cutTracklets(
                          trackDat,
                          customFunc = function(x)
                            x[[timeCol]] >= timeWin[[p]][[1]] &
                            x[[timeCol]] <= timeWin[[p]][[2]]
                        ))
    WhoWhen <- unlist(WhoWhen, recursive = FALSE)
    
    # Then, convert the list of tracklet as an unique list (only in case there is several tracklet in trackDat)
    trackDatList <- MoveR::convert2List(WhoWhen)
    
    # Compute the surface of a regular hexagon as follow: 3*side*apothem
    # where:
    # the apothem corresponds to binRad/2
    # side corresponds to the length of one side and is computed as follow: apothem * 2 tan(pi/6)
    # hence surface is 3 * (binrad/2 * 2 * tan(pi/6)) * binRad/2 which can be simplified
    surface <- 1.5 * tan(pi / 6) * binRad ^ 2
    
    ## Divide the plane in a hexagonal grid, each cell representing a neighborhood that a particule could 'explore' around its position
    ## and count the number of unique cells that the particle has entered in at least once using hexagonal heatmaps, as implemented in hexbin package
    ## compute the number of bins needed given the perception distance of the particle
    nbins <- imgRes[1] / binRad
    ## apply hexbin to count the number of visited cells
    nbcells <-
      hexbin::hexbin(
        trackDatList[["x.pos"]],
        trackDatList[["y.pos"]],
        xbnds = c(0, imgRes[1]),
        ybnds = c(0, imgRes[2]),
        xbins = nbins,
        shape = 1
      )@ncells
    ### compute the total surface explored, by multiplying the number of visited cells by the surface of one cell and the user-specified scale
    Explored <- nbcells * surface * scale
    ### plot the result but display it only if graph = T
    Exploredplot <- hexbin::hexbinplot(
      trackDatList[["y.pos"]] ~
        trackDatList[["x.pos"]],
      xbins = nbins,
      data = trackDatList,
      aspect = '1',
      main = "Heatmap of the explored areas",
      xlab = "Video width (pixels)",
      ylab = "Video height (pixels)",
      colramp = function(n) {
        hexbin::heat.ob(n, beg = 240, end = 1)
      },
      colorcut = seq(0, 1, length = 10),
      cex.labels = 0.9,
      xlim = c(0, imgRes[1]),
      ylim = c(0, imgRes[2]),
      #scales = list(tck=0.25)
      scales = list(
        y = list(at = pretty(
          seq(0, imgRes[2]), n = 6, min.n = 3
        )),
        x = list(at = pretty(
          seq(0, imgRes[1]), n = 6, min.n = 3
        )),
        tck = 0.25
      )
    )
    if (isTRUE(graph)) {
      print(Exploredplot)
    }
    if (isTRUE(saveGraph)) {
      for (i in seq(1e10)) {
        plotname <-
          paste(paste("Exploredplot", i, sep = "_"), "tiff", sep = ".")
        if (isTRUE(file.exists(paste(getwd(), plotname, sep = "/")))) {
          next
        } else{
          break
        }
      }
      grDevices::tiff(plotname)
      print(Exploredplot)
      dev.off()
    } else if (!isTRUE(saveGraph) & !isFALSE(saveGraph)) {
      saveGraph
      print(Exploredplot)
      dev.off()
    }
    return(Explored)
  }
