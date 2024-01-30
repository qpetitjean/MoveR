#' @title Display tracklets on a plot.
#'
#' @description Given an object of class "tracklets" containing a list of tracklets,
#' this function returns a plot with tracklets represented as a sequence of segments connecting
#' cartesian coordinates between successive frames. 
#'
#' @param trackDat An object of class "tracklets" containing a list of tracklets and their characteristics classically used for further computations (at least x.pos, y.pos, frame).
#'
#' @param selTrack A vector containing the id of the tracklets to plot (optional).
#'
#' @param selId A vector containing the id of the particles to plot. Specifying selId result in displaying all tracklets belonging to the selected particles. Note that particles' identity is retrieved from the "identity" column (optional).
#'
#' @param imgRes A vector of 2 numeric values, the resolution of the video used as x and y limit of the plot (i.e., the number of pixels in image width and height).
#' If imgRes is unspecified, the function try to retrieve it from tracklets object attribute or approximate it using x and y maximum values + 5%.
#'
#' @param timeWin  A list of one or several vector containing 2 numeric values separated by a comma
#' corresponding to the time interval between which tracklets have to be drawn in frame (optional).
#'
#' @param timeCol A character string corresponding to the name of the column containing time information (default = "frame").
#'
#' @param colGrad A vector of a ready to use color ramp or several colors specified
#' either with their name or hexadecimal values (optional).
#'
#' @param colId A character string corresponding to a column name present within each tracklet's data frame from the trackDat list 
#' and which is used as grouping factor to color the tracklets (default = timeCol).
#' It is possible to specify "tracklets", "selTrack" or "selId" to color tracklets according to tracklets' id, or selected tracklets and particles' id respectively.
#' Note that if colId is made of continuous values, the legend is plotted as a gradient (except if colId = "selTrack" or "selId").
#'
#' @param add2It A function specifying element(s) to add to the plot.
#'
#' @param srt A value or a vector of two values specifying the orientation of the axes values
#' for the x and y axis respectively (default = 0).
#'
#' @param lwd A numeric value, the line width of the tracklets (default = 1).
#'
#' @param main Primary title of the plot (default = "").
#'
#' @param xlab X-axis label (default = "Video width (pixels)").
#'
#' @param ylab Y-axis label (default = "Video height (pixels)").
#' 
#' @param axLabPos A vector of two numeric values indicating on which margin line axis title (x, y respectively) should be, starting at 0 counting outwards (see \code{\link[graphics]{mtext}}). 
#'
#' @param legend A logical value (i.e., TRUE or FALSE) indicating whether the legend should be displayed or not (default = TRUE).
#'
#' @param legend.title The legend title.
#'
#' @param cex.axis A numeric value, the character size and expansion for axis and legend values (default = 1).
#'
#' @param cex.main A numeric value, the character size and expansion for primary title (default = 1.25).
#'
#' @param cex.lab A numeric value, the character size and expansion for axes label (default = 1).
#'
#' @param cex.leg A numeric value, the character size and expansion for the legend label (default = 1).
#'
#' @param ncol.leg A numeric value, the number of columns the legend should be displayed, only when colId is not continuous (default = 1)
#'
#' @param cex.start A numeric value, the size of the dot representing the start of the tracklets (default = 0.5).
#'
#' @param asp A numeric value, the y/x aspect ratio of the plot window (see \code{\link[graphics]{plot.window}}).
#'
#' @param progress A logical value (i.e., TRUE or FALSE) indicating whether a progress bar should be displayed to inform process progression (default = TRUE).
#'
#' @return A plot displaying selected tracklets over a specified time window.
#'
#'
#' @author Quentin PETITJEAN
#'
#'
#' @examples
#'
#' set.seed(2023)
#' # generate some dummy tracklets
#' ## start to specify some parameters to generate tracklets
#' TrackN <- 40 # the number of tracklet to simulate
#' TrackL <-
#'   1:1000 # the length of the tracklets or a sequence to randomly sample tracklet length
#' 
#' TrackList <-
#'   MoveR::trackletsClass(stats::setNames(lapply(lapply(seq(TrackN), function(i)
#'     trajr::TrajGenerate(sample(TrackL, 1), random = TRUE, fps = 1)), function(j) {
#'       data.frame(
#'         x.pos = j$x - min(j$x),
#'         y.pos = j$y - min(j$y),
#'         frame = j$time,
#'        identity = paste("Id", sample(1:10,1, replace = T), sep = "_")
#'       )
#'     }), seq(TrackN)))
#' 
#'  # example 1: draw all tracklets, for the whole video duration
#'  MoveR::drawTracklets(TrackList)
#'  
#'  # example 1bis: draw all tracklets, for the whole video duration and change color gradient
#'  MoveR::drawTracklets(TrackList,
#'                       colGrad = viridis::viridis(10))
#' 
#'  # example 2: draw only selected tracklets (i.e., 1, 5 and 10), for the whole video duration
#'  MoveR::drawTracklets(TrackList,
#'                   selTrack = c(1, 5, 10))
#' 
#'  # example 3: draw tracklets according to time intervals (first interval 1 to 100, second interval 800 to 900 frames)
#'  MoveR::drawTracklets(TrackList,
#'                   timeWin = list(c(1, 100), c(800, 900)),)
#' 
#'  # example 4: draw tracklets according to time intervals (first interval 1 to 100, second interval 800 to 900 frames)
#'  # and color the tracklets according to their Id instead of time elapsed
#'  MoveR::drawTracklets(TrackList,
#'                   timeWin = list(c(1, 100), c(800, 900)),
#'                   colId = "tracklets",
#'                   cex.leg = 0.75,
#'                   ncol.leg = 2)
#' 
#'  # example 5: draw tracklets according to time interval (interval 1 to 100) and add dummy points on the plot
#'  # (here we draw the starting and ending points of each tracklets in blue and green respectively)
#'  MoveR::drawTracklets(TrackList,
#'                   timeWin = list(c(1, 100)),
#'                   add2It = list(for (j in seq(TrackList)) {
#'                     points(
#'                       TrackList[[j]]$x.pos[TrackList[[j]]$frame == 1],
#'                       TrackList[[j]]$y.pos[TrackList[[j]]$frame == 1],
#'                       col = "blue",
#'                       pch = 19,
#'                       cex = 0.8
#'                     )
#'                   }, for (l in seq(TrackList)) {
#'                     points(
#'                       TrackList[[l]]$x.pos[TrackList[[l]]$frame == 100],
#'                       TrackList[[l]]$y.pos[TrackList[[l]]$frame == 100],
#'                       col = "green",
#'                       pch = 19,
#'                       cex = 0.8
#'                     )
#'                   }))
#' 
#'  # example 6: select some tracklets, time windows and color tracklet according to their id
#'  MoveR::drawTracklets(TrackList,
#'                       selTrack = names(TrackList)[c(1, 5, 10, 12)],
#'                       timeWin = list(c(1, 100), c(800, 1000)),
#'                       colId = "selTrack",
#'                       cex.leg = 0.75)
#'  
#'   # example 7: select some particles' identity, time windows and color tracklet (all tracklets belonging to the selected particles' identity) according to particles identity
#'   ## identify particles' identity
#'   ids <- unique(unlist(lapply(TrackList, function(x)
#'     MoveR::listGet(x, "identity"))))
#'   
#'   MoveR::drawTracklets(TrackList,
#'                 selId = ids[c(1, 5, 10)],
#'                 timeWin = list(c(1, 100), c(800, 1000)),
#'                 colId = "selId",
#'                 cex.leg = 0.75)
#'   
#'   # example 8: make a panel plot with 
#'   # 1- all tracklets belonging to a given particle's identity
#'   # 2- all tracklets belonging to a given particle's identity on a specific time window
#'   # 3- Specific tracklets belonging to a given particle's identity on a specific time window
#'   par(mfrow=c(1,3))
#'   #1
#'   MoveR::drawTracklets(TrackList,
#'                 selId = ids[1],
#'                 colId = "tracklets",
#'                 axLabPos = c(-13, 2),
#'                 cex.leg = 0.75,
#'                 ncol.leg = 1,
#'                 main = "all id_7",
#'                 cex.main = 0.8)
#'   #2
#'   MoveR::drawTracklets(TrackList,
#'                 selId = ids[1],
#'                 timeWin = list(c(1, 100), c(800, 1000)),
#'                 colId = "tracklets",
#'                 axLabPos = c(-13, 2),
#'                 cex.leg = 0.75,
#'                 ncol.leg = 1,
#'                 main = "id_7, \nspecific time window",
#'                 cex.main = 0.8)
#'   #3
#'   MoveR::drawTracklets(TrackList,
#'                 selTrack = names(TrackList)[c(1, 12)],
#'                 selId = ids[1],
#'                 timeWin = list(c(1, 100), c(800, 1000)),
#'                 colId = "selTrack",
#'                 axLabPos = c(-13, 2),
#'                 cex.leg = 0.75,
#'                 ncol.leg = 1,
#'                 main = "id_7, \nspecific time window, specific tracklets",
#'                 cex.main = 0.8)
#'
#' @export

drawTracklets <- function(trackDat,
                          selTrack = NULL,
                          selId = NULL,
                          imgRes = c(NA, NA),
                          timeWin = list(c(0, Inf)),
                          timeCol = "frame",
                          colGrad = c(
                            "#000000",
                            "#999999",
                            "#6600CC",
                            "#9999FF",
                            "#33CCFF",
                            "#0066CC",
                            "#66CC00",
                            "#FFFF00",
                            "#FF3300",
                            "#CC0000"
                          ),
                          colId = NULL,
                          add2It = NULL,
                          srt = 0,
                          lwd = 1,
                          main = "",
                          xlab = "Video width (pixels)",
                          ylab = "Video height (pixels)",
                          axLabPos = c(0,2),
                          legend = TRUE,
                          legend.title = NULL,
                          cex.axis = 1,
                          cex.main = 1.25,
                          cex.lab = 1,
                          cex.leg = 1,
                          ncol.leg = 1,
                          cex.start = 0.5,
                          asp = 1,
                          progress = TRUE) {

  error <- .errorCheck(trackDat = trackDat)
  if(!is.null(error)){
    stop(error)
  }
  
  if (is.null(names(trackDat))) {
    names(trackDat) <- seq_along(trackDat)
  }
  # compute the duration of the video according to timeCol argument
  viDur <-
    max(unlist(lapply(trackDat, function(x)
      max(MoveR::listGet(x, timeCol),
          na.rm = T))))
  # if imgRes is unspecified; it try to retrieve it from tracklets object attribute else, 
  # retrieve it approximately using the maximum value in x and y coordinates
  if (TRUE %in% is.na(imgRes)) {
    if (!is.null(MoveR::getInfo(trackDat, "imgRes"))) {
      imgRes <- MoveR::getInfo(trackDat, "imgRes")
    } else{
      xCoords <- unlist(lapply(trackDat, function(x)
        MoveR::listGet(x, "x.pos")))
      if (length(which(is.infinite(xCoords)) > 0)) {
        xCoords <- xCoords[!is.infinite(xCoords)]
      }
      width <-
        round(max(xCoords, na.rm = T) + 5 * max(xCoords, na.rm = T) / 100, 0)
      
      yCoords <- unlist(lapply(trackDat, function(x)
        MoveR::listGet(x, "y.pos")))
      if (length(which(is.infinite(yCoords)) > 0)) {
        yCoords <- yCoords[!is.infinite(yCoords)]
      }
      height <-
        round(max(yCoords, na.rm = T) + 5 * max(yCoords, na.rm = T) / 100, 0)
      
      imgRes <- c(width, height)
    }
  }
  # set colId
  if (is.null(colId)) {
    colId <- timeCol
    colVal <- NULL
  }else if(colId == "selTrack" && !is.null(selTrack)){
    colVal <- selTrack
    colId <- "Selected Tracklets"
  }else if(colId == "tracklets" && !is.null(selTrack)){
    colVal <- selTrack
    colId <- "selTrack"
  }else if(colId == "selId" && !is.null(selId)){
    colVal <- selId
    colId <- "Selected IDs"
  }else if(colId == "tracklets" && is.null(selTrack) && is.null(selId)){
    colVal <- names(trackDat)
    colId <- "Tracklets IDs"
  }else if(colId == "tracklets" && is.null(selTrack) && !is.null(selId)){
    colVal <- names(Filter(Negate(is.null),
                           lapply(trackDat, function(df) {
                             if (unique(df[["identity"]]) %in% selId) {
                               return(df)
                             }
                           })))
    colId <- "Tracklets IDs"
  }else{ 
    colId <- colId
    colVal <- NULL
  }

  # define color ramp according to colId
  Pal <- grDevices::colorRampPalette(c(colGrad))
  if(is.null(colVal)){
  colVal <- unique(unlist(lapply(trackDat, function(x)
    MoveR::listGet(x, colId))))
  }
  colVal <- colVal[order(colVal)]
  coloration <- Pal(length(colVal[!is.na(colVal)]))
  
  # FORCE the number of column in the legend if colVal is numeric
  if (is.numeric(colVal[!is.na(colVal)])) {
    ncol.leg = 1
  }
  
  # initialize the plot window
  ScaleY <- pretty(c(0, imgRes[[2]]), n = 5)
  ScaleX <- pretty(c(0, imgRes[[1]]), n = 5)
  
  plot(
    NA,
    xlim = ifelse(rep(isTRUE(legend), 2),
                  c(
                    0,
                    max(ScaleX) + (ScaleX[2] - ScaleX[1]) * ncol.leg + (cex.leg * 1.5 / 100 *
                                                                          (ScaleX[2] - ScaleX[1]))
                  ),
                  c(0, max(ScaleX))),
    ylim = c(0, max(ScaleY)),
    ylab = "",
    xlab = "",
    axes = FALSE,
    asp = asp
  )
  
  ## add plot title
  graphics::mtext(
    main,
    cex = cex.main,
    side = 3,
    line = 1.5,
    adj = c(0.5, 0.5)
  )
  
  ## add axis labels
  axisLab <- c(xlab, ylab)
  for (l in 1:2) {
    graphics::mtext(
      axisLab[l],
      cex = cex.lab,
      side = l,
      line = axLabPos[l],
      adj = c(0.5, 0.5),
      padj = c(0.5, 0.5),
      outer = F
    )
  }
  # draw axes
  Scales <- list(ScaleX = ScaleX, ScaleY = ScaleY)
  ScalesLab <- Scales
  if (ScaleX[1] == ScaleY[1]) {
    ScalesLab[["ScaleX"]] <-
      ScalesLab[["ScaleX"]][-which(ScalesLab[["ScaleX"]] == 0)]
  }
  for (j in seq_along(Scales)) {
    graphics::segments(
      x0 = ifelse(names(Scales)[j] == "ScaleY", min(ScaleX), get(names(Scales)[j])[1]),
      y0 = ifelse(names(Scales)[j] == "ScaleY", get(names(Scales)[j])[1], 0),
      x1 = ifelse(names(Scales)[j] == "ScaleY", min(ScaleX), max(get(names(
        Scales
      )[j]))),
      y1 = ifelse(names(Scales)[j] == "ScaleY", max(get(names(
        Scales
      )[j])), 0)
    )
    
    graphics::text(
      ifelse(
        names(ScalesLab)[j] == rep("ScaleY", length(ScalesLab[[j]])),
        rep(min(ScalesLab[[j]]), length(ScalesLab[[j]])),
        ScalesLab[[j]]
      ),
      ifelse(
        names(ScalesLab)[j] == rep("ScaleY", length(ScalesLab[[j]])),
        ScalesLab[[j]],
        rep(0, length(ScalesLab[[j]]))
      ),
      ScalesLab[[j]],
      xpd = TRUE,
      srt = ifelse(length(srt) > 1, srt[2], srt),
      cex = cex.axis,
      pos = ifelse(names(ScalesLab)[j] == "ScaleY", 2, 1)
    )
    graphics::text(
      ifelse(
        names(ScalesLab)[j] == rep("ScaleY", length(ScalesLab[[j]])),
        rep(min(ScaleX), length(get(
          names(ScalesLab)[j]
        ))),
        ScalesLab[[j]]
      ),
      ifelse(
        names(ScalesLab)[j] == rep("ScaleY", length(get(
          names(ScalesLab)[j]
        ))),
        ScalesLab[[j]],
        rep(0, length(ScalesLab[[j]]))
      ),
      "-",
      xpd = TRUE,
      srt = ifelse(names(ScalesLab)[j] == "ScaleY", 0, 90),
      adj = c(0.8, 0.25)
    )
  }
  
  if (!is.list(timeWin)) {
    stop(
      "[timeWin] argument should be a list of vector(s) containing starting and ending value of each time window interval"
    )
  }
  if (max(unlist(lapply(timeWin, length))) > 2) {
    stop(
      "[timeWin] argument contains a vector of length > 2,
         [timeWin] should be a list of vector(s) containing 2 values (start and end of the time window interval)"
    )
  }
  
  # in case some tracklet has been selected
  if (!is.null(selTrack)) {
    NewTrackList <- trackDat[which(names(trackDat) %in% selTrack)]
  } else{
    NewTrackList <- trackDat
  }
  if (!is.null(selId)) {
    NewTrackList <- Filter(Negate(is.null),
                           lapply(NewTrackList, function(df) {
                             if (unique(df[["identity"]]) %in% selId) {
                               return(df)
                             }
                           }))
  } else{
    NewTrackList <- NewTrackList
  }
  # if timeWin is set to default (0 to Inf)
  if (length(timeWin) == 1 & timeWin[[1]][2] == Inf) {
    timeWin[[1]][2] <- viDur
  } else {
    # in case there is Inf in timeWin replace it by vidur
    InfLoc <-
      length(unlist(lapply(lapply(timeWin, function(x)
        which(x == Inf)), function (y)
          length(y) > 0)))
    if (length(InfLoc) > 0) {
      timeWin[[InfLoc]][which(timeWin[[InfLoc]] == Inf)] <- viDur
    } else {
      timeWin <- timeWin
    }
    ## cut the tracklets to draw only the specified part
    NewTrackList <- lapply(seq_along(timeWin),
                           function(p)
                             .cutTracklets(
                               NewTrackList,
                               customFunc = function(x)
                                 x[[timeCol]] >= timeWin[[p]][[1]] &
                                 x[[timeCol]] <= timeWin[[p]][[2]]
                             ))
    
    NewTrackList <- unlist(NewTrackList, recursive = FALSE)
  }
  
  ## in case legend argument is TRUE, create a legend
  if (isTRUE(legend)) {
    ### draw the legend as a gradient if colId is a continuous vector (numerical) or as category in case
    ### colId is a factor or character vector
    if (is.numeric(colVal[!is.na(colVal)])) {
      ScaleVal <-
        pretty(c(min(colVal, na.rm = T), max(colVal, na.rm = T)), n = 5)
      ScaleLegtemp <- which(colVal %in% ScaleVal)
      for (val in which(!ScaleVal %in% colVal)) {
        if (val == length(ScaleVal)) {
          ScaleLegtemp <-
            c(ScaleLegtemp, max(ScaleLegtemp + mean(diff(
              ScaleLegtemp
            ))))
        } else if (val == 1) {
          ScaleLegtemp <-
            c(min(ScaleLegtemp - mean(diff(
              ScaleLegtemp
            ))), ScaleLegtemp)
        }
      }
      if (max(nchar(ScaleVal)) >= 5) {
        ScaleVal <- format(ScaleVal, scientific = TRUE)
      } else{
        ScaleVal <- format(ScaleVal, scientific = FALSE)
      }
      ScaleLeg <-
        as.integer(ScaleLegtemp * max(ScaleY) / max(ScaleLegtemp))
      ScaleLeg[1] <- ScaleLeg[1] + (2.5 * max(ScaleY) / 100)
      ScaleLeg[length(ScaleLeg)] <-
        ScaleLeg[length(ScaleLeg)] - (5 * max(ScaleY) / 100)
      
      legend_image <-
        grDevices::as.raster(matrix(coloration, ncol = 1))
      graphics::rasterImage(
        legend_image,
        max(ScaleX) + (1 * max(ScaleX) / 100),
        max(ScaleY) - (5 * max(ScaleY) / 100),
        max(ScaleX) + (5 * max(ScaleX) / 100),
        min(ScaleY) + (1 * max(ScaleY) / 100)
      )
      rasterW <-
        (max(ScaleX) + (5 * max(ScaleX) / 100)) - (max(ScaleX) + (1 * max(ScaleX) / 100))
      graphics::text(
        x = max(ScaleX) + rasterW + ceiling(max(strwidth(ScaleVal))),
        y = ScaleLeg,
        labels =  ScaleVal,
        cex = cex.leg
      )
    } else{
      if (TRUE %in%  grepl("[0-9]", colVal)) {
        colValNum <- as.numeric(gsub("[^[:digit:]]", "", colVal))
        names(colValNum) <- seq_along(colValNum)
        colVal <- colVal[as.numeric(names(sort(colValNum)))]
      } else{
        colVal <- colVal
      }
      
      legend_image <- graphics::legend(
        x = max(ScaleX) + (1 * max(ScaleX) / 100),
        y = max(ScaleY) - (1 * max(ScaleY) / 100),
        legend = colVal[!is.na(colVal)],
        pch = 22,
        col = "black",
        pt.bg = coloration,
        bty = "n",
        pt.cex = 1.5,
        text.font = 1,
        cex = cex.leg,
        xpd = TRUE,
        ncol = ncol.leg
      )
      rasterW <- legend_image$rect$w
    }
    ### legend title (retrieve from colId)
    graphics::text(
      max(ScaleX) + ifelse(is.numeric(colVal[!is.na(colVal)]),
                           rasterW + ceiling(max(
                             strwidth(ScaleVal)
                           )),
                           rasterW) / 2,
      max(ScaleY) + (5 * max(ScaleY) / 100),
      ifelse(
        colId == timeCol,
        ifelse(
          is.null(legend.title),
          paste("Time ", "(", timeCol, ")", sep = ""),
          legend.title
        ),
        ifelse(is.null(legend.title), colId, legend.title)
      ),
      cex = cex.leg,
      font = 1,
      xpd = TRUE
    )
  }
  
  # if an additional function (add2it argument) is added to drawTracklets draw it
  if (!is.null(add2It) == TRUE) {
    add2It
  }

  if (length(colVal) < length(coloration)) {
    coloration <- coloration[1:length(colVal)]
  }
  coloration <-
    data.frame(colors = coloration, colVal = colVal[!is.na(colVal)])
  
  # plot all tracklets according to timeWin
  if (isTRUE(progress)) {
    # initialize progress bar
    total = length(NewTrackList)
    pb <-
      progress::progress_bar$new(format = "Drawing Tracklets [:bar] :current/:total (:percent)", total = total)
    pb$tick(0)
  }
  if (colId == "Selected Tracklets" | colId == "Tracklets IDs"){
  iter <- 0 
  }
  # loop over the tracklets to draw them
  invisible(lapply(NewTrackList, function(x) {
    if(!colId %in% c("Selected Tracklets", "Selected IDs", "Tracklets IDs")){
    x[["colInd"]] <- coloration[match(as.character(MoveR::listGet(x, colId)),
                                      as.character(coloration[["colVal"]][!is.na(colVal)])), "colors"] 
    }else if (colId == "Selected Tracklets" | colId == "Tracklets IDs"){
      iter <<- iter + 1
      x[["colInd"]] <- rep(coloration[match(names(NewTrackList)[iter], 
                                            coloration[["colVal"]]), "colors"],
                           nrow(x))
    }else if (colId == "Selected IDs"){
      x[["colInd"]] <- coloration[match(as.character(MoveR::listGet(x, "identity")),
                         as.character(coloration[["colVal"]][!is.na(colVal)])), "colors"]
    }
    # Draw the starting point and segments
    with(x,
         graphics::points(
           x.pos[1],
           y.pos[1],
           col = colInd[1],
           pch = 19,
           cex = cex.start
         ))
    with(x,
         graphics::segments(
           head(x.pos, -1),
           head(y.pos, -1),
           x.pos[-1],
           y.pos[-1],
           col = colInd,
           lwd = lwd
         ))
    if (isTRUE(progress)) {
      # progress bar
      pb$tick(1)
    }
    return(NULL) 
  }
  ))
}