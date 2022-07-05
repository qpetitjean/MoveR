
nbins = 100
var1 = "SlideVarAngle"
var2 = "SlidemeanSpeed"

actives2 <- function(trackDat, trackDatSmoothed, var1 = NULL, var2 = NULL, nbins = NULL, na.rm = T, graph = T){

  if(isTRUE(na.rm)){
  tempDf <-
    na.omit(trackDatSmoothed[c(var1, var2)])
  }
  tempDf$SlidemeanSpeed <- log10(tempDf$SlidemeanSpeed)
  if (length(which(is.infinite(tempDf$SlidemeanSpeed)) > 0)) {
    tempDf <- tempDf[-c(which(is.infinite(tempDf$SlidemeanSpeed))), ]
  }
  if(isTRUE(graph)){
    gplots::hist2d(
      x = cbind(tempDf$SlideVarAngle, tempDf$SlidemeanSpeed),
      nbins = nbins,
      xlab = "SlideVarAngle",
      ylab = "SlidemeanSpeed (log10)"
    )
  }
 
  nbins <- rep(nbins, 2)
  x = tempDf$SlideVarAngle
  y = tempDf$SlidemeanSpeed
  ## specify groups of values according to nbins
  x.cuts <-
    seq(from = min(x),
        to = max(x),
        length = nbins[1] + 1)
  y.cuts <-
    seq(from = min(y),
        to = max(y),
        length = nbins[2] + 1)
  index.x <- cut(x, x.cuts, include.lowest = TRUE)
  index.y <- cut(y, y.cuts, include.lowest = TRUE)
  ## compute the matrix of count according to each cuts
  m <- tapply(x, list(index.x, index.y), base::length)
  m[is.na(m)] <- 0
  ## check the occurrence of 0
  m0 <- m
  m0[m > 0] <- 1
  
  if(isTRUE(graph)){
  plot3D::persp3D(
    z = m,
    theta = 20,
    phi = 30,
    xlab = "SlideVarAngle",
    ylab = "log(SlidemeanSpeed)",
    zlab = "Counts"
  )
    graphics::image(
      x.cuts,
      y.cuts,
      m0,
      col = c("black", "#FF9933"),
      main = "zeros (black) vs higher (orange) counts"  ,
      xlab = "SlideVarAngle",
      ylab = "log(SlidemeanSpeed)"
    )
  }
  ## transform m as dataframe
  mdf <-
    data.frame(rows = rownames(m)[row(m)],
               vars = colnames(m)[col(m)],
               values = c(m))
  names(mdf) <- c("x", "y", "value")
  mdfL <-
    lapply(seq(ncol(mdf) - 1), function(z)
      gsub('^.|.$', "", mdf[, z]))
  mdf2 <-
    cbind(data.frame(do.call("cbind", mdfL)), mdf$value)
  xcomp <- strsplit(as.character(mdf2$X1), ",", fixed = TRUE)
  ycomp <- strsplit(as.character(mdf2$X2), ",", fixed = TRUE)
  xcomp <-
    unlist(lapply(xcomp, function(x)
      mean(as.numeric(x), na.rm = T)))
  ycomp <-
    unlist(lapply(ycomp, function(x)
      mean(as.numeric(x), na.rm = T)))
  mdf3 <-
    data.frame(x = xcomp,
               y = ycomp,
               value = mdf2[["mdf$value"]])
  mdf3$spots <- rep(NA)
  ## use the total number of count divided by the square of bins to specify a treshold for hot and cold spots
  ### in case it does not allow to split inactives and actives clusters, change the treshold incrementing nbins by 1% 
  IncTresh <-
    unlist(lapply(0:100, function(x)
      x * (unique(nbins) ^ 2) / 100))
  for (uptresh in IncTresh) {
    hotLim <- sum(m) / (unique(nbins) ^ 2 - uptresh)
    Hot = which(mdf3$value > hotLim,
                arr.ind = TRUE)
    mdf3$spots[Hot] <- "Hot"
    Cold = which(mdf3$value <= hotLim,
                 arr.ind = TRUE)
    mdf3$spots[Cold] <- "Cold"
    ## identify the peaks drawed by hotspots using density based clustering
    db <-
      fpc::dbscan(scale(mdf3[which(mdf3$spots == "Hot"), c("x", "y")], center = T),
                  eps = 0.15,
                  MinPts = 5)
    ## extract the border of the clusters (rectangular)
    if (length(unique(db$cluster)[grepl("0", unique(db$cluster))]) > 0) {
      dbclust <- unique(db$cluster)[-which(unique(db$cluster) == 0)]
    } else {
      dbclust <- unique(db$cluster)
    }
    XRanges = sapply(dbclust,
                     function(i)
                       range(mdf3[which(mdf3$spots == "Hot"), c("x", "y")][db$cluster == i, 1]))
    YRanges = sapply(dbclust,
                     function(i)
                       range(mdf3[which(mdf3$spots == "Hot"), c("x", "y")][db$cluster == i, 2]))
    ## compute centroids of each cluster
    centroids <-
      as.data.frame(do.call("rbind",
                            lapply(dbclust,
                                   function(i)
                                     apply(mdf3[which(mdf3$spots == "Hot"), c("x", "y")][db$cluster == i, ], 2,
                                           mean, na.rm = T))))
    # reorder the centroids according to x axis to ensure that active is clust 1, uncertain is clust 2 (when detected) and inactive is clust 3
    centroids <- cbind(centroids, dbclust)
    centroids <- centroids[order(centroids[, "x"]), ]
    # determine whether cluster belong to inactive or active category according to its coordinate on x axis (angle variance, the lower the variance, the more individual are actives)
    if (nrow(centroids) < 2) {
      next
    }
    if (nrow(centroids) == 2) {
      inactivClust <-
        mdf3[which(mdf3$spots == "Hot"), c("x", "y", "value")][db$cluster == centroids[which(centroids$x == max(centroids$x)), "dbclust"], ]
      activClust <-
        mdf3[which(mdf3$spots == "Hot"), c("x", "y", "value")][db$cluster == centroids[which(centroids$x == min(centroids$x)), "dbclust"], ]
    } else if (nrow(centroids) == 3) {
      inactivClust <-
        mdf3[which(mdf3$spots == "Hot"), c("x", "y", "value")][db$cluster == centroids[which(centroids$x == max(centroids$x)), "dbclust"], ]
      activClust <-
        mdf3[which(mdf3$spots == "Hot"), c("x", "y", "value")][db$cluster == centroids[which(centroids$x == min(centroids$x)), "dbclust"], ]
      uncertClust <-
        mdf3[which(mdf3$spots == "Hot"), c("x", "y", "value")][db$cluster == centroids[which(centroids$x != min(centroids$x) &
                                                                                               centroids$x != max(centroids$x)), "dbclust"], ]
    }
    #activClust <-mdf3[which(mdf3$spots == "Hot"), c("x", "y", "value")][db$cluster == 3, ]
    #inactivClust <-mdf3[which(mdf3$spots == "Hot"), c("x", "y", "value")][db$cluster == 2, ]
    
    if (min(XRanges) == min(inactivClust[["x"]])) {
      next
    } else{
      break
    }
  }
  if(nrow(centroids) > 3){
    print(paste(folder, "failed" , "More than 3 clusters identified", sep = " "))
    rm(list = setdiff(ls(), envirList))
    gc()
    pb$tick(1)
    next
  }
  if(isTRUE(graph)){
    plot(
      mdf3[which(mdf3$spots == "Cold"), c("x", "y")],
      pch = 20,
      col = rgb(
        red = 0,
        green = 0,
        blue = 1,
        alpha = 0.2
      ),
      ylab = "SlideMeanSpeed (log10)",
      xlab = "SlideVarAngle",
      main = "2d hist and clustering of the two groups: \nactives vs inactives"
    )
    points(mdf3[which(mdf3$spots == "Hot"), c("x", "y")],
           pch = 20,
           col = rgb(
             red = 1,
             green = 0,
             blue = 0,
             alpha = 0.2
           ))
    ## add the rectangular cluster limits
    sapply(dbclust,
           function(i)
             polygon(c(XRanges[, i], rev(XRanges[, i])), rep(YRanges[, i], each = 2)))
    ## add it to the previous plot
    points(centroids, col = "black", pch = 19)
    
    plot(inactivClust)
  }
  names(inactivClust) <-
    c('X_values', 'Y_values', 'response')
  names(activClust) <-
    c('X_values', 'Y_values', 'response')
  # Set the covariance matrix
  sigma2 <-
    matrix(cov(inactivClust[, c("X_values", "Y_values")]), ncol = 2)
  # compute the means (centroid location)
  mu <-
    c(
      mean(inactivClust$X_values, na.rm = T),
      mean(inactivClust$Y_values, na.rm = T)
    )
  # get the eigenvalues and eigenvectors
  evals <- eigen(sigma2)$values
  evecs <- eigen(sigma2)$vectors
  # Set the angles of a circle
  theta <- seq(0, 2 * pi, len = 200)
  # Get critical value for inactive moments
  cInact <- qchisq(0.95, 2)
  cInact <- sqrt(cInact)
  # Get critical value for uncertain moments
  cUncert <- qchisq(0.99, 2)
  cUncert <- sqrt(cUncert)
  # Get the distances
  xInact <- cInact * sqrt(evals[1]) * cos(theta)
  yInact <- cInact * sqrt(evals[2]) * sin(theta)
  RawEllipseInact <- cbind(xInact, yInact)
  xUncert <- cUncert * sqrt(evals[1]) * cos(theta)
  yUncert <- cUncert * sqrt(evals[2]) * sin(theta)
  RawEllipseUncert <- cbind(xUncert, yUncert)
  # Covert the coordinates
  transRawEllipseInact <- evecs %*% t(RawEllipseInact)
  transRawEllipseInact <- t(transRawEllipseInact)
  ellipseContourInact <-
    data.frame(x = transRawEllipseInact[, 1] + mu[1], y = transRawEllipseInact[, 2] + mu[2])
  transRawEllipseUncert <- evecs %*% t(RawEllipseUncert)
  transRawEllipseUncert <- t(transRawEllipseUncert)
  ellipseContourUncert <-
    data.frame(x = transRawEllipseUncert[, 1] + mu[1], y = transRawEllipseUncert[, 2] + mu[2])
  ## determine the position of major and minor radius to specify two new layer for tresholding
  ### find the coordinates of the major and minor axis
  majRadInact <-
    ellipseContourInact[c(which(ellipseContourInact$x == min(ellipseContourInact$x)),
                          which(ellipseContourInact$x == max(ellipseContourInact$x))),]
  minRadInact <-
    ellipseContourInact[c(which(ellipseContourInact$y == min(ellipseContourInact$y)),
                          which(ellipseContourInact$y == max(ellipseContourInact$y))),]
  majRadUncert <-
    ellipseContourUncert[c(
      which(
        ellipseContourUncert$x == min(ellipseContourUncert$x)
      ),
      which(
        ellipseContourUncert$x == max(ellipseContourUncert$x)
      )
    ),]
  minRadUncert <-
    ellipseContourUncert[c(
      which(
        ellipseContourUncert$y == min(ellipseContourUncert$y)
      ),
      which(
        ellipseContourUncert$y == max(ellipseContourUncert$y)
      )
    ),]
  if(isTRUE(graph)){
    plot(
      inactivClust$X_values,
      inactivClust$Y_values,
      xlim = c(
        range(tempDf["SlideVarAngle"])[1] - (range(tempDf["SlideVarAngle"])[2] - range(tempDf["SlideVarAngle"])[1] / 2),
        range(tempDf["SlideVarAngle"])[2] + (range(tempDf["SlideVarAngle"])[2] - range(tempDf["SlideVarAngle"])[1] / 2)
      ),
      ylim = c(
        range(tempDf["SlidemeanSpeed"])[1] + (range(tempDf["SlidemeanSpeed"])[1] - range(tempDf["SlidemeanSpeed"])[2] / 2),
        range(tempDf["SlidemeanSpeed"])[2] + (range(tempDf["SlidemeanSpeed"])[2] - range(tempDf["SlidemeanSpeed"])[1] / 2)
      )
    )
    points(activClust$X_values, activClust$Y_values, col = "#99CC66")
    if(exists("uncertClust")){
      names(uncertClust) <-
        c('X_values', 'Y_values', 'response')
      points(uncertClust$X_values, uncertClust$Y_values, col = "#FF9933")
    }
    # plot the ellipses
    lines(ellipseContourInact, col = "#993333")
    lines(ellipseContourUncert, col = "#FF9933")
    ### plot major and minor radius for both uncertain and inactive ellipses
    lines(majRadUncert$x, majRadUncert$y,  col = "#FF9933")
    lines(minRadUncert$x, minRadUncert$y, col = "#FF9933")
    lines(majRadInact$x, majRadInact$y,  col = "#993333")
    lines(minRadInact$x, minRadInact$y, col = "#993333")
    # plot the centroid
    points(mu[1], mu[2], col = "#993333", pch = 19)
    ### delimit (expend) inactive cluster to upper limit of the ellipse in y and lower in x
    ### create the tresholds
    #### for y, the threshold corresponds to the value of y for which x is minimum of the minor radius
    minRadUncert <- minRadUncert[!duplicated(minRadUncert$x) & !duplicated(minRadUncert$y),]
    minRadInact <- minRadInact[!duplicated(minRadInact$x) & !duplicated(minRadInact$y),]
    majRadInact <- majRadInact[!duplicated(majRadInact$x) & !duplicated(majRadInact$y),]
    majRadUncert <- majRadUncert[!duplicated(majRadUncert$x) & !duplicated(majRadUncert$y),]
    YtreshUncert <- max(minRadUncert$y, na.rm = T)
    YtreshInact <-  max(minRadInact$y, na.rm = T)
    ###### draw the threshold
    lines(
      x = c(minRadUncert$x[which(minRadUncert$y == max(minRadUncert$y, na.rm = T))], ifelse(
        length(which(
          is.infinite(tempDf$SlideVarAngle)
        )) > 0,
        max(tempDf$SlideVarAngle[-which(is.infinite(tempDf$SlideVarAngle))], na.rm = T),
        max(tempDf$SlideVarAngle, na.rm = T)
      ) * 2),
      y = rep(YtreshUncert, 2)
      ,
      col = "#FF9933"
    )
    lines(
      x = c(minRadInact$x[which(minRadInact$y == max(minRadInact$y, na.rm = T))], ifelse(
        length(which(
          is.infinite(tempDf$SlideVarAngle)
        )) > 0,
        max(tempDf$SlideVarAngle[-which(is.infinite(tempDf$SlideVarAngle))], na.rm = T),
        max(tempDf$SlideVarAngle, na.rm = T)
      ) * 2),
      y = rep(YtreshInact, 2)
      ,
      col = "#993333"
      
    )
    #### for x, the threshold corresponds to the left limit of the ellipse (minimum x value of the major radius)
    XtreshUncert <- min(majRadUncert$x, na.rm = T)
    XtreshInact <- min(majRadInact$x, na.rm = T)
    ###### draw the threshold
    lines(
      x = rep(XtreshUncert, 2),
      y = c(majRadUncert$y[which(majRadUncert$x == min(majRadUncert$x, na.rm =
                                                         T))], ifelse(
                                                           length(which(
                                                             is.infinite(tempDf$SlidemeanSpeed)
                                                           )) > 0,
                                                           min(tempDf$SlidemeanSpeed[-which(is.infinite(tempDf$SlidemeanSpeed))], na.rm = T),
                                                           min(tempDf$SlidemeanSpeed, na.rm = T)
                                                         ) * 2),
      col = "#FF9933"
    )
    lines(
      x = rep(XtreshInact, 2),
      y = c(majRadInact$y[which(majRadInact$x == min(majRadInact$x, na.rm =
                                                       T))], ifelse(
                                                         length(which(
                                                           is.infinite(tempDf$SlidemeanSpeed)
                                                         )) > 0,
                                                         min(tempDf$SlidemeanSpeed[-which(is.infinite(tempDf$SlidemeanSpeed))], na.rm = T),
                                                         min(tempDf$SlidemeanSpeed, na.rm = T)
                                                       ) * 2),
      col = "#993333"
    )
  }
  
  hexplt <- hexbin::hexbinplot(
    tempDf[, "SlidemeanSpeed"] ~ tempDf[, "SlideVarAngle"],
    data = tempDf,
    aspect = '1',
    xbins = 40,
    xlab = "SlideVarAngle",
    ylab = "log(SlidemeanSpeed)",
    panel = function(x, y, ...) {
      hexbin::panel.hexbinplot(x, y, ...)
      lattice::panel.lines(ellipseContourInact, col = "#993333")
      lattice::panel.lines(ellipseContourUncert, col = "#FF9933")
    }
  )
  print(hexplt)
  
  trackdatL <- convert2list(trackDat)
  toClust <-
    data.frame(SlideVarAngle = trackdatL[["SlideVarAngle"]],
               LogSlidemeanSpeed = log10(trackdatL[["SlidemeanSpeed"]]))
  SigSqrt <- evecs %*% diag(sqrt(evals)) %*% t(evecs)
  Z <-
    t(apply(toClust, 1, function(x)
      solve(SigSqrt, x - mu)))
  toClust$insideInact <-
    rowSums(Z ^ 2) < stats::qchisq(0.95, df = ncol(Z))
  toClust$insideUncert <-
    rowSums(Z ^ 2) < stats::qchisq(0.99, df = ncol(Z))
  
  ### apply the thresholds previously computed
  toClust$insideInact[which(toClust["SlideVarAngle"] > XtreshInact &
                              toClust["SlidemeanSpeed"] < majRadInact$y[which(XtreshInact == majRadInact$x)])] <-
    TRUE
  toClust$insideInact[which(toClust["SlideVarAngle"] > minRadInact$x[which(YtreshInact == minRadInact$y)] &
                              toClust["SlidemeanSpeed"] < YtreshInact)] <-
    TRUE
  toClust$insideUncert[which(toClust["SlideVarAngle"] > XtreshUncert &
                               toClust["SlidemeanSpeed"] < majRadUncert$y[which(XtreshUncert == majRadUncert$x)])] <-
    TRUE
  toClust$insideUncert[which(toClust["SlideVarAngle"] > minRadUncert$x[which(YtreshUncert == minRadUncert$y)] &
                               toClust["SlidemeanSpeed"] < YtreshUncert)] <-
    TRUE
  toClust$insideUncert[which(toClust["SlideVarAngle"] < XtreshInact)] <-
    FALSE
  toClust$insideUncert[which(toClust$insideInact == TRUE)] <-
    FALSE
  toClust$mvt <- NA
  toClust$mvt[which(toClust$insideInact == TRUE)] <-
    "inactive"
  toClust$mvt[which(toClust$insideUncert == TRUE)] <-
    "uncertain"
  toClust$mvt[which(toClust$insideUncert == FALSE &
                      toClust$insideInact == FALSE)] <-
    "active"
  Clustresample <-
    toClust[seq(1, nrow(toClust), 20),]
  if (length(which(is.infinite(Clustresample[, "SlidemeanSpeed"])) > 0)) {
    Clustresample <- Clustresample[-c(which(is.infinite(Clustresample[, "SlidemeanSpeed"]))),]
  }
  if (length(which(is.infinite(Clustresample[, "SlideVarAngle"])) > 0)) {
    Clustresample <- Clustresample[-c(which(is.infinite(Clustresample[, "SlideVarAngle"]))),]
  }

  trackdatL$mvt <- NA
  trackdatL$mvt[which(toClust$mvt == "inactive")] <-
    "inactive"
  trackdatL$mvt[which(toClust$mvt == "uncertain")] <-
    "uncertain"
  trackdatL$mvt[which(toClust$mvt == "active")] <-
    "active"
  if(isTRUE(graph)){
    xrange <- pretty(
      range(Clustresample["SlideVarAngle"], na.rm = T),
      min.n = 4,
      n = 6,
      bounds = TRUE
    )
    yrange <- pretty(
      range(Clustresample["SlidemeanSpeed"], na.rm = T),
      min.n = 4,
      n = 6,
      bounds = TRUE
    )
    plot(
    NULL,
    ylim = c(min(yrange), max(yrange)),
    xlim = c(min(xrange), max(xrange)),
    ylab = "SlideMeanSpeed (log10)",
    xlab = "SlideVarAngle",
    main = "2d clusters of activity classification"
  )
    points(Clustresample[which(Clustresample$mvt == "inactive"), "SlidemeanSpeed"] ~ Clustresample[which(Clustresample$mvt == "inactive"), "SlideVarAngle"], col = "#993333")
    points(Clustresample[which(Clustresample$mvt == "uncertain"), "SlidemeanSpeed"] ~ Clustresample[which(Clustresample$mvt == "uncertain"), "SlideVarAngle"], col = "#FF9933")
    points(Clustresample[which(Clustresample$mvt == "active"), "SlidemeanSpeed"] ~ Clustresample[which(Clustresample$mvt == "active"), "SlideVarAngle"], col = "#99CC66")
    legend(
      xrange[length(xrange) - 1],
      max(pretty(
        range(Clustresample["SlidemeanSpeed"], na.rm = T),
        min.n = 4,
        n = 6,
        bounds = TRUE
      )),
      c("active", "uncertain", "inactive"),
      cex = 0.8,
      fill = c("#99CC66", "#FF9933", "#993333")
    )
    # similar representation but with hexbin
    toClustNoNA <- na.omit(toClust)
    if (length(which(is.infinite(toClustNoNA[, "SlidemeanSpeed"])) > 0)) {
      toClustNoNA <-
        toClustNoNA[-c(which(is.infinite(toClustNoNA[, "SlidemeanSpeed"]))),]
    }
    with(
      toClustNoNA,
      hextri::hextri(
        SlideVarAngle,
        SlidemeanSpeed,
        class = toClustNoNA$mvt,
        colours = c("#99CC66", "#993333", "#FF9933"),
        style = "size",
        nbins = 15,
        xlab = "SlideVarAngle",
        ylab = "log(SlidemeanSpeed)",
        main = "2d clusters of activity classification",
        diffuse = FALSE,
        border = TRUE
      )
    )
    # pie chart of behavioral state proportion
    act <-
      length(which(trackdatL$mvt == "active")) / length(trackdatL$mvt[-c(which(is.na(trackdatL$mvt)))]) * 100
    inact <-
      length(which(trackdatL$mvt == "inactive")) / length(trackdatL$mvt[-c(which(is.na(trackdatL$mvt)))]) * 100
    uncert <-
      length(which(trackdatL$mvt == "uncertain")) / length(trackdatL$mvt[-c(which(is.na(trackdatL$mvt)))]) * 100
    pie(
      c(act, uncert, inact),
      labels = paste(round(
        data.frame(
          active = act,
          uncertain = uncert,
          inactive = inact
        ),
        digits = 2
      ), "%", sep = ""),
      col = c("#99CC66", "#FF9933", "#993333")
    )
    legend(
      .8,
      1.0,
      c("active", "uncertain", "inactive"),
      cex = 0.8,
      fill = c("#99CC66", "#FF9933", "#993333")
    )
  }
  Res <- convert2frags(trackdatL, by = "frags_id")
  return(Res)
}
  
  
  
  
  