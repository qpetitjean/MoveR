#' @title Draw tracking fragments
#'
#' @description Given a list of tracking fragments containing cartesian coordinates,
#' this fonction returns a plot with fragments represented as a sequence of segments connecting
#' cartesian coordinates between successive frames. Fragments are colored according to the time.
#'
#'
#' @param fragsList A list of tracking fragments to plot along x and y coordinates
#'
#' @param selFrags A vector of either numeric values or character strings,
#' the number or the id of the selFrags to plot (optional)
#'
#' @param imgRes A vector of 2 numeric values, resolution of the video used as x and y limit of the plot
#'  (i.e., the number of pixels in image width and height, e.g., 1920 x 1080).
#'
#' @param timeWin  A list of one or several vector containing 2 numeric values separated by a comma
#' corresponding to the time interval between which selFrags have to be drawn in frame (optional)
#'
#' @param colGrad A vector of a ready to use color ramp or several colors specified
#' either with their name or hexadecimal values choose color
#' gradient representing time (see base R documentation) (optional)
#'
#' @param add2It A function specifying an element to add to draw_frags
#'
#'
#' @return A plot with all or only selected fragment paths colored according to time
#'
#'
#' @authors Quentin Petitjean, Vincent Calcagno
#'
#'
#'
#' @examples
#'
#'
#'# generate some dummy fragments
#'fragsList = list()
#'for (i in seq(10)) {
#'  rPath <- TrajGenerate(1000, random = TRUE, fps = 1)
#'  rPath$y = rPath$y - min(rPath$y)
#'  rPath$x = rPath$x - min(rPath$x)
#'  sillydf = data.frame(rPath[1], rPath[2], rPath[3])
#'  names(sillydf) = c("x.pos", "y.pos", "frame")
#'  fragsList[[i]] = sillydf
#'}
#'# convert frag list to a simple list to extract image resolution for generated fragments (x, y scales of the plot)
#' trackDat <- convert2list(fragsList)
#'
#'# Exemple 1: draw all fragments, for the whole video duration
#'drawFrags(
#'  fragsList,
#'  imgRes = c(max(trackDat$x.pos), max(trackDat$y.pos)),
#'  timeWin = list(c(0, 1001))
#')
#'
#'# Exemple 2: draw only selected fragments (i.e., 1, 5 and 10), for the whole video duration
#'drawFrags(
#'  fragsList,
#'  imgRes = c(max(trackDat$x.pos), max(trackDat$y.pos)),
#'  timeWin = list(c(0, 1001)),
#'  selFrags = c(1, 5, 10)
#')
#'
#'# Exemple 3: draw fragments according to time intervals (first interval 1 to 100, second interval 800 to 900 frames)
#'drawFrags(
#'  fragsList,
#'  imgRes = c(max(trackDat$x.pos), max(trackDat$y.pos)),
#'  timeWin = list(c(1, 100), c(800, 900))
#')
#'
#'# Exemple 4: draw fragments according to time intervals (first interval 1 to 100, second interval 800 to 900 frames)
#'# and add dummy points on the plot (here we draw the starting and ending points of each fragments in blue and green respectively)
#'drawFrags(
#' fragsList,
#' imgRes = c(max(trackDat$x.pos), max(trackDat$y.pos)),
#' timeWin = list(c(1, 100), c(800, 900)),
#' add2It = list(for (j in seq(fragsList)) {
#'  points(
#'    fragsList[[j]]$x.pos[fragsList[[j]]$frame == 1],
#'    fragsList[[j]]$y.pos[fragsList[[j]]$frame == 1],
#'    col = "blue",
#'    pch = 19,
#'    cex = 0.8
#'  )
#'}, for (k in seq(fragsList)) {
#'  points(
#'    fragsList[[k]]$x.pos[fragsList[[k]]$frame == 800],
#'    fragsList[[k]]$y.pos[fragsList[[k]]$frame == 800],
#'    col = "blue",
#'    pch = 19,
#'    cex = 0.8
#'  )
#'}, for (l in seq(fragsList)) {
#'  points(
#'    fragsList[[l]]$x.pos[fragsList[[l]]$frame == 100],
#'    fragsList[[l]]$y.pos[fragsList[[l]]$frame == 100],
#'    col = "green",
#'    pch = 19,
#'    cex = 0.8
#'  )
#'}, for (m in seq(fragsList)) {
#'  points(
#'    fragsList[[m]]$x.pos[fragsList[[m]]$frame == 900],
#'    fragsList[[m]]$y.pos[fragsList[[m]]$frame == 900],
#'    col = "green",
#'    pch = 19,
#'    cex = 0.8
#'  )
#'})
#')
#'
#'
#' @export

drawFrags <- function(fragsList,
                      selFrags = NULL,
                      imgRes = c(1920, 1080),
                      timeWin = list(c(0, Inf)),
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
                      add2It = NULL) {
  
  ## compute the duration of the video in frame
  viDur <-
    max(unlist(lapply(fragsList, function (x)
      max(
        list_get(x, "frame")
      ))))
  
  # define color ramp according to timeWin
  Pal <- colorRampPalette(c(colGrad))
  coloration <- Pal(viDur)
  color_legend <- Pal(viDur)
  
  # save initial graphical window parameters
  opar <- par(no.readonly = TRUE)
  
  plot.new()
  
  layout(matrix(1:2, ncol = 2),
         width = c(4, 1),
         height = c(1, 1))
  
  # create legend
  par(mfg = c(1, 2), mai = c(0.9, 0.2, 0.6, 0.08))
  legend_image <- as.raster(matrix(color_legend, ncol = 1))
  plot(
    c(0, 4),
    c(0, 1),
    type = "n",
    axes = F,
    xlab = "",
    ylab = "",
    font.main = 1,
    main = "Time (frames)"
  )
  text(
    x = 2.5,
    y = seq(0, 1, l = 5),
    labels = round(seq(0, viDur, l = 5), digits = 0)
  )
  rasterImage(legend_image, 1, 1, 0, 0)
  
  # create blank plot
  par(mfg = c(1, 1), mai = c(0.9, 0.8, 0.6, 0.01))
  plot(
    NA,
    xlim = c(0, imgRes[[1]]),
    ylim = c(0, imgRes[[2]]),
    main = "Fragments",
    xlab = "Video width (pixels)",
    ylab = "Video height (pixels)"
  )
  
  # if an additional function is added to drawFrags draw it
  if (!is.null(add2It) == TRUE) {
    add2It
  }
  
  # if timeWin is set to default (0 to Inf)
  if (length(timeWin) == 1 & timeWin[[1]][2] == Inf) {
   timeWin[[1]][2] = viDur
  }
  
  # if timeWin is specified color only the fraction of fragments within time window interval(s)
  dfColoration <- cbind(replicate(length(timeWin), coloration))
  
  for (i in seq(length(timeWin))) {
    if (timeWin[[i]][1] > 0) {
      lower <- timeWin[[i]][1] - 1
      dfColoration[1:lower, i] = rgb(1, 1, 1, 0)
    } else if (timeWin[[i]][1] == 0) {
      dfColoration[, i] <- Pal(viDur)
    }
    
    if (timeWin[[i]][2] < viDur) {
      higher <- timeWin[[i]][2] + 1
      dfColoration[higher:(viDur), i] = rgb(1, 1, 1, 0)
    } else if (timeWin[[i]][2] == viDur) {
      dfColoration[, i] = Pal(viDur)
    }
  }
  
  # In case several time window intervals are specified, this concatenate all coloration vector to plot all intervals
  dfColoration[dfColoration == rgb(1, 1, 1, 0)] <- NA
  dfColoration <- as.data.frame(dfColoration)
  coloration <- dfColoration[, 1]
  for (j in 1:length(dfColoration)) {
    coloration[!is.na(dfColoration[, j])] = dfColoration[, j][!is.na(dfColoration[, j])]
  }
  coloration[is.na(coloration)] <- rgb(1, 1, 1, 0)
  
  # plot all fragments according to timeWin color
  if (is.null(selFrags)) {
    # initialize progress bar
    total = length(fragsList)
    pb <-
      progress::progress_bar$new(format = "fragments drawing [:bar] :current/:total (:percent)", total = total)
    pb$tick(0)
    Sys.sleep(0.001)
    
    for (f in seq(length(fragsList))) {
      fragsList[[f]]$colorpal <-
        coloration[match(fragsList[[f]]$frame, rownames(as.data.frame(coloration)))]
      points(
        fragsList[[f]]$x.pos[1],
        fragsList[[f]]$y.pos[1],
        col = fragsList[[f]]$colorpal[1],
        pch = 19,
        cex = 0.5
      )
      with(fragsList[[f]],
           segments(
             head(x.pos, -1),
             head(y.pos, -1),
             x.pos[-1],
             y.pos[-1],
             fragsList[[f]]$colorpal
           ))
      
      # progress bar
      pb$tick(1)
      Sys.sleep(1 / 1000)
    }
  } else {
    # plot only the selected fragments according to timeWin color
    # initialize progress bar
    total = length(selFrags)
    pb <-
      progress::progress_bar$new(format = "fragments drawing [:bar] :current/:total (:percent)", total = total)
    pb$tick(0)
    Sys.sleep(0.001)
    
    for (f in selFrags) {
      fragsList[[f]]$colorpal <-
        coloration[match(fragsList[[f]]$frame, rownames(as.data.frame(coloration)))]
      points(
        fragsList[[f]]$x.pos[1],
        fragsList[[f]]$y.pos[1],
        col = fragsList[[f]]$colorpal,
        pch = 19,
        cex = 0.5
      )
      with(fragsList[[f]],
           segments(
             head(x.pos, -1),
             head(y.pos, -1),
             x.pos[-1],
             y.pos[-1],
             fragsList[[f]]$colorpal
           ))
      
      # progress bar
      pb$tick(1)
      Sys.sleep(1 / 1000)
      }
  }
  # reset graphical window parameters
  par(opar)
  
}
