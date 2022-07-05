#' @title Explored Area
#'
#' @description Given a list of tracking fragments containing cartesian coordinates, and a value of
#'  reaction distance this function displays an heatmap (i.e., hexbin plot) of the explored areas and returns the
#'  total surface explored in a geometrically consistent and scalable manner
#'
#' @param trackDat A list of tracking fragments to plot along x and y coordinates
#'
#' @param binRad A numeric value corresponding to the diameter of the typical surface a particle can "explore" around its position
#' For instance, the reaction distance of a Trichograms (i.e., a parasitoid micro-wasp) is about 4 mm, in this case, a reasonable value is of order 16 mm^2,
#' the diameter of such a cell is hence about 8 mm
#'
#' @param imgRes A vector of 2 numeric values, resolution of the video used as x and y limit of the plot
#'  (i.e., the number of pixels in image width and height, e.g., 1920 x 1080).
#'
#' @param scale A ratio corresponding to the scaling factor to be applied to the trajectory coordinates (optional)
#'
#' @param timeCol A character string corresponding to the name of the column containing Time information (e.g., "frame") (optional)
#'
#' @param timeWin  A list of one or several vector containing 2 numeric values separated by a comma
#' corresponding to the time interval between which the fragments have to be drawn according to timeCol (optional)
#'
#'
#' @return displays an heatmap (i.e., hexbin plot) of the explored areas and returns the
#'  total surface explored in a geometrically consistent and scalable manner
#'
#'
#' @authors Quentin PETITJEAN, Vincent CALCAGNO
#'
#'
#' @examples
#'
#'
#'
#' @export

exploredArea <-
  function(trackDat,
           binRad = NULL,
           imgRes = c(1920, 1080),
           scale = 1,
           timeCol = "frame",
           timeWin = list(c(0, Inf))) {
    if(is.null(binRad)){ 
      stop(
        "binRad argument is missing, \na numeric value specifying the diameter of the cell a particle can explore is needed to compute the surface explored"
      )
      }
    # Select only the part of the fragments parts included in the selected time Window
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
            max(
              listGet(x, timeCol)
            ))))
      } else{
        # in case the input trackDat contains value for only one fragment (a dataframe)
        timeWin[[InfLoc]][which(timeWin[[InfLoc]] == Inf)] <-
          max(listGet(trackDat, timeCol), na.rm = T)
      }
    } else {
      timeWin = timeWin
    }
    selVal <- unlist(lapply(timeWin, function(x)
      seq(x[1],
          x[2])))
    # select the fragment that are detected in the selected timeline part
    if (class(trackDat) == "list" & length(trackDat) >= 1) {
      WhoWhen <-
        cutFrags(trackDat, function(x)
          x[[timeCol]] >= min(selVal, na.rm = T) &
            x[[timeCol]] <= max(selVal, na.rm = T))
    } else{
      # in case the input trackDat contains value for only one fragment (a dataframe)
      WhoWhen <-
        trackDat[which(trackDat[[TimeCol]] >= min(selVal, na.rm = T) &
                         trackDat[[TimeCol]] <= max(selVal, na.rm = T)),]
    }
    
    # Then, convert the list of fragment as an unique list (only in case there is several fragment in trackDat)
    if (class(WhoWhen) == "list" & length(WhoWhen) > 1) {
      trackDatList <- convert2list(WhoWhen)
    } else{
      # in case the input trackDat contains value for only one fragment (a dataframe)
      trackDatList <- WhoWhen
    }
    
    # Compute the surface of a regular hexagon as follow: 3*side*apothem
    # where:
    # the apothem corresponds to binRad/2
    # side corresponds to the length of one side and is computed as follow: apothem * 2 tan(pi/6)
    # hence surface is 3 * (binrad/2 * 2 * tan(pi/6)) * binRad/2 which can be simplified
    surface <- 1.5 * tan(pi / 6) * binRad ^ 2
    
    ## Divide the plane in a hexagonal grid, each cell representing a neighborhood that a particule could 'explore' around its position
    ## and count the number of unique cells that the particle has entered in at least once using hexagonal heatmaps, as implemented in hexbin package
    ## compute the number of bins needed given the perception distance of the insect
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
    ### plot the result
    Exploredplot <- hexbin::hexbinplot(
      trackDatList[["y.pos"]] ~
        trackDatList[["x.pos"]],
      xbins = binRad,
      data = trackDatList,
      aspect = '1',
      main = "Heatmap of the explored areas",
      xlab = "Video width",
      ylab = "Video height",
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
    
    print(Exploredplot)
    return(Explored)
  }