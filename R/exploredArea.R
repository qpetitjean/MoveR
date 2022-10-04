#' @title Compute the total surface explored and display the corresponding heatmap
#'
#' @description Given a list of tracking fragments containing cartesian coordinates, and a value of
#'  reaction distance this function displays an heatmap (i.e., hexbin plot) of the explored areas and returns the
#'  total surface explored in a geometrically consistent and scalable manner.
#'
#' @param trackDat A list of data frame containing tracking informations for each fragment (including a timeline)
#' or a data frame containing tracking information for one fragment.
#'
#' @param binRad A numeric value corresponding to the diameter of the typical surface a particle can "explore" around its position
#' For instance, the reaction distance of a Trichograms (i.e., a parasitoid micro-wasp) is about 4 mm, in this case, a reasonable value is of order 16 mm^2,
#' the diameter of such a cell is hence about 8 mm.
#'
#' @param imgRes A vector of 2 numeric values, the resolution of the video used as x and y limit of the plot
#'  (i.e., the number of pixels in image width and height, e.g., 1920 x 1080).
#'
#' @param scale A ratio corresponding to the scaling factor to be applied to the trajectory coordinates (optional).
#'
#' @param timeCol A character string corresponding to the name of the column containing Time information (e.g., "frame").
#'
#' @param timeWin  A list of one or several vector containing 2 numeric values separated by a comma
#' corresponding to the time interval between which the fragments have to be drawn according to timeCol (optional).
#'
#' @param saveGraph A logical value indicating whether the heatmap should be saved as a .tiff file in the hardrive - within the working directory (default = FALSE).
#' Alternatively, the user can specify the saving function and hence the extension and the
#' directory where the heatmap should be saved (see for instance grDevices::png(), grDevices::jpeg() or grDevices::tiff()).
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
#'# generate some dummy fragments
#'## start to specify some parameters to generate fragments
#'Fragn <- 20 # the number of fragment to simulate
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
#'
#'MoveR::drawFrags(fragsList,
#'          imgRes = c(max(MoveR::convert2list(fragsList)[["x.pos"]]),
#'                     max(MoveR::convert2list(fragsList)[["y.pos"]])),
#'          timeCol = "frame")
#'
#'# extract image resolution
#'imgRes <- c(max(MoveR::convert2list(fragsList)[["x.pos"]]), 
#'            max(MoveR::convert2list(fragsList)[["y.pos"]]))
#'# compute the total surface explored and displays the heatmap for all fragments
#'# and save the plot in the working directory using user specified parameters (saveGraph argument)
#'# if saveGraph is "TRUE", the plot is saved in the working directory as .tiff image
#'# if saveGraph is "FALSE", the plot is only displayed.
#'MoveR::exploredArea(fragsList,
#'             binRad = 8,
#'             imgRes = imgRes,
#'             scale = 1,
#'             timeCol = "frame",
#'             timeWin = list(c(0, Inf)),
#'             saveGraph = grDevices::tiff("exploredPlotTest.tiff",
#'                                         width = 600,
#'                                         height = 600
#'             ))
#'
#'# compute the surface explored for each fragment and display (but do not save) the heatmap for each fragment
#'# here by combining exploredArea and analyseFrags, the surface explored for each fragment will be appended to the data
#'# of the corresponding fragment.
#'MoveR::analyseFrags(
#'  fragsList,
#'  customFunc = list(
#'    exploredArea = function(x)
#'      exploredArea(
#'        x,
#'        binRad = 8,
#'        imgRes = imgRes,
#'        scale = 1,
#'        timeCol = "frame",
#'        timeWin = list(c(0, Inf)),
#'        saveGraph = FALSE
#'      ))
#')
#'
#' @export

exploredArea <-
  function(trackDat,
           binRad = NULL,
           imgRes = c(1920, 1080),
           scale = 1,
           timeCol = "frame",
           timeWin = list(c(0, Inf)),
           saveGraph = FALSE) {
    if (is.null(binRad)) {
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
    
    # select the part of the fragments that are included in timeWin
    WhoWhen <- lapply(seq(length(timeWin)),
                      function(p)
                        cutFrags(
                          trackDat,
                          customFunc = function(x)
                            x[[timeCol]] >= timeWin[[p]][[1]] &
                            x[[timeCol]] <= timeWin[[p]][[2]]
                        ))
    WhoWhen <- unlist(WhoWhen, recursive = FALSE)
    
    # Then, convert the list of fragment as an unique list (only in case there is several fragment in trackDat)
    trackDatList <- convert2list(WhoWhen)
    
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
