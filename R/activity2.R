#' @title Determine active or inactive states according to density based clustering method.
#'
#' @description Given a list of data frames containing tracking information and two variable of interest the function use
#' density based clustering as introduced in Ester et al. (1996). 
#' As a result, the function use the DBSCAN method from Hennig (2020) to discriminate active and inactive states in a 2d space by returning numeric values indicating whether the particle is "active" (1) or "inactive" (0).
#'
#' @param trackDat A list of data frame containing tracking information for each tracklet.
#'
#' @param var1 A character string indicating the name of the variable to use as the first dimension.
#'
#' @param var2 A character string indicating the name of the variable to use as the second dimension.
#'
#' @param var1T A function used to transform var1 (e.g., log, sqrt - optional).
#'
#' @param var2T A function used to transform var2 (e.g., log, sqrt - optional).
#'
#' @param nbins A numeric value indicating the number of bins in both vertical and horizontal directions (default = 100).
#'
#' @param eps A numeric value specifying the reachability distance (Ester et al., 1996), which correspond to the maximum distance around cluster's members (see \code{\link[fpc]{dbscan}}).
#'
#' @param minPts A numeric value specifying the reachability minimum no. of points (Ester et al., 1996), which correspond to the minimum number of point per cluster (see \code{\link[fpc]{dbscan}}).
#'
#' @param scale A logical value (i.e., TRUE or FALSE) indicating whether the data should be centered (i.e., values minus the mean) and scaled (divided by the standard deviation) (default = TRUE).
#'
#' @param na.rm A logical value (i.e., TRUE or FALSE) indicating whether NA values should be stripped before the computation proceeds (default = TRUE).
#'
#' @param graph A logical value (i.e., TRUE or FALSE) indicating whether the distribution (3d density map) of the active and inactive states according to the classification (density based clustering) plot should be displayed or not (default = TRUE). 
#'
#'
#' @return This function returns the results of the classification (actives vs. inactives) as numeric value (1 or 0, respectively) appended to
#' the original list of data frame containing tracking information for each tracklet. 
#' Also, if graph argument is TRUE, the function returns the distribution (3d density map) of the active and inactive states according to the classification (density based clustering) in the viewer.
#'
#' @author Quentin PETITJEAN
#'
#' @seealso \code{\link[fpc]{dbscan}}
#'
#' @references
#' \itemize{
#'          \item{Christian, H., (2020). fpc: Flexible Procedures for Clustering. R package version 2.2-9. \href{https://CRAN.R-project.org/package=fpc}{https://CRAN.R-project.org/package=fpc}}
#'          \item{Ester, M., Kriegel H.P., Sander, J., Xu X., (1996). A Density-Based Algorithm for Discovering Clusters in Large Spatial Databases with Noise. Institute for Computer Science, University of Munich. Proceedings of 2nd International Conference on Knowledge Discovery and Data Mining (KDD-96).}
#'          }
#'
#' @examples
#' \dontrun{
#'
#' # Download the first dataset from the sample data repository
#' Path2Data <- MoveR::DLsampleData(dataSet = 1, tracker = "TRex")
#' Path2Data
#'
#' # Import the list containing the 9 vectors classically used for further computation
#' Data <- MoveR::readTrex(Path2Data[[1]])
#'
#' # convert it to a list of tracklets
#' trackDat <- MoveR::convert2Tracklets(Data[1:7], by = "identity")
#'
#' # infinite values that are present in the tracking output should be removed
#' ## define the filter
#' filter.Inf <-
#'   MoveR::filterFunc(
#'     trackDat,
#'     toFilter = "x.pos",
#'     customFunc = function(x)
#'      is.infinite(x)
#'   )
#'
#' ### filter Infinite values
#' trackDat.Infilt <-
#'   MoveR::filterTracklets(trackDat,
#'                      filter = filter.Inf,
#'                      splitCond = TRUE,
#'                      minDur = 100)
#'
#' ### remove some tracklet to speed up the computation
#' trackdat2 <- trackDat.Infilt[[2]][1:75]
#'
#' # check the tracklet
#' MoveR::drawTracklets(trackdat2,
#'                  timeCol = "frame")
#'
#' # add some metric to the dataset to perform 2d clustering (speed and turning angle)
#' # and smooth them by computing the mean value over a 10 frames' sliding window
#' Tstep = 10
#' trackdat3 <-
#'   MoveR::analyseTracklets(
#'     trackdat2,
#'     customFunc = list(
#'       # specify a first function to compute speed over each tracklet (a modulus present within the MoveR package)
#'       speed = function(x)
#'         MoveR::speed(x,
#'                      timeCol = "frame",
#'                      scale = 1),
#'       # compute turning angle in radians over each tracklet (a modulus present within the MoveR package)
#'       TurnAngle = function(x)
#'         MoveR::turnAngle(
#'           x,
#'           timeCol = "frame",
#'           unit = "radians",
#'           scale = 1
#'         ),
#'       # smooth variance of turning angles
#'       SlideVarAngle = function (y)
#'         MoveR::slidWindow(y$TurnAngle,
#'                        Tstep = Tstep, function (x)
#'                          circular::var(
#'                            circular::circular(
#'                              x,
#'                              type = "angle",
#'                              units = "radians",
#'                              zero = 0
#'                            ),
#'                            na.rm = T
#'                          )),
#'       # smooth speed
#'       SlidemeanSpeed = function (y)
#'         MoveR::slidWindow(y$speed,
#'                        Tstep = Tstep, function (x)
#'                          mean(x, na.rm = T))
#'     )
#'   )
#'
#' # use density based clustering to classify actives and inactives states in a 2 dimension array (here the speed and the angle variance)
#' # when graph = TRUE, the function also display the distribution (3d density map) of the active and inactive states according to the classification (density based clustering)
#'
#' trackdat3 <- MoveR::activity2(
#'   trackdat3,
#'   var1 = "SlidemeanSpeed",
#'   var2 = "SlideVarAngle",
#'   var1T = log10,
#'   nbins = 100,
#'   eps = 0.15,
#'   minPts = 5,
#'   scale = TRUE,
#'   na.rm = TRUE,
#'   graph = TRUE
#' )
#'
#' # draw the particle' trajectory and spot the inactive moments using red dots
#' MoveR::drawTracklets(trackdat3,
#'           cex.start = 0.1,
#'           add2It = list(for (i in seq_along(trackdat3)) {
#'             points(
#'               trackdat3[[i]][["x.pos"]][which(trackdat3[[i]][["activity2"]] == 0)],
#'               trackdat3[[i]][["y.pos"]][which(trackdat3[[i]][["activity2"]] == 0)],
#'               col = "red",
#'               pch = 19,
#'               cex = 1.5
#'             )
#'           }))
#'
#' }
#' @export
#'
activity2 <-
  function(trackDat,
           var1 = NULL,
           var2 = NULL,
           var1T = NULL,
           var2T = NULL,
           nbins = NULL,
           eps = NULL,
           minPts = NULL,
           scale = TRUE,
           na.rm = TRUE,
           graph = TRUE) {
    # retrieve var1 and var2 from the dataset and transform them if needed
    trackdatL <- MoveR::convert2List(trackDat)
    var1n <- var1
    var2n <- var2
    if (!is.null(var1T)) {
      var1 <- var1T(trackdatL[[var1]])
      var1n <-
        paste0(var1n, " (", gsub('"', '', regmatches(
          deparse(var1T), gregexpr('"([^"]*)"', deparse(var1T))
        )[[1]]), ")")
    } else{
      var1 <- trackdatL[[var1]]
    }
    if (!is.null(var2T)) {
      var2 <- var2T(trackdatL[[var2]])
      var2n <-
        paste0(var2n, " (", gsub('"', '', regmatches(
          deparse(var2T), gregexpr('"([^"]*)"', deparse(var2T))
        )[[1]]), ")")
    } else{
      var2 <- trackdatL[[var2]]
    }
    # in case var1 and var2 have different length, return an error message.
    if (length(var1) == length(var2)) {
      trackDatSmoothed <- data.frame(var1 = var1, var2 = var2)
    } else{
      stop(
        "var1 and var2 have different length: ",
        length(var1),
        ", ",
        length(var2),
        ".\n",
        "var1 and var2 must contain the same amount of data."
      )
    }
    # if minPts or eps argument are null return an error
    if (is.null(eps)) {
      stop(
        "eps argument is missing, specifying a maximum distance around cluster's members is needed."
      )
    }
    if (is.null(minPts)) {
      stop(
        "minPts argument is missing, specifying a minimum number of point per cluster is needed."
      )
    }
    
    # if na.rm argument is TRUE, remove the lines containing NA in the dataframe containing var1 and var2
    if (isTRUE(na.rm)) {
      tempDf <-
        na.omit(trackDatSmoothed)
    }
    # in case there is infinite values remove them
    if (length(which(is.infinite(tempDf[["var1"]]))) > 0) {
      tempDf <- tempDf[-c(which(is.infinite(tempDf[["var1"]]))),]
      warning("var1 contains infinite values, these values has been removed")
    }
    if (length(which(is.infinite(tempDf[["var2"]]))) > 0) {
      tempDf <- tempDf[-c(which(is.infinite(tempDf[["var2"]]))),]
      warning("var1 contains infinite values, these values has been removed")
    }
    if (nrow(tempDf) == 0) {
      stop("In var1 and var2 NA and Inf values has been removed but no data remain")
    }
    if (is.null(nbins)) {
      nbins = 100
      warning("nbins argument is unspecified, default value is 100")
    }
    nbins <- rep(nbins, 2)
    x = tempDf[["var2"]]
    y = tempDf[["var1"]]
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
      cbind(data.frame(do.call("cbind", mdfL)), value = mdf[["value"]])
    xcomp <- strsplit(as.character(mdf2[["X1"]]), ",", fixed = TRUE)
    ycomp <- strsplit(as.character(mdf2[["X2"]]), ",", fixed = TRUE)
    xcomp <-
      unlist(lapply(xcomp, function(x)
        mean(as.numeric(x), na.rm = T)))
    ycomp <-
      unlist(lapply(ycomp, function(x)
        mean(as.numeric(x), na.rm = T)))
    mdf3 <-
      data.frame(x = xcomp,
                 y = ycomp,
                 value = mdf2[["value"]])
    mdf3[["spots"]] <- rep(NA)
    
    ## use the total number of count divided by the square of bins to specify a treshold for hot and cold spots
    ### in case it does not allow to split inactives and actives clusters, change the treshold incrementing nbins by 1%
    IncTresh <-
      unlist(lapply(0:100, function(x)
        x * (unique(nbins) ^ 2) / 100))
    ### loop trough treshold increments
    for (uptresh in IncTresh) {
      hotLim <- sum(m) / (unique(nbins) ^ 2 - uptresh)
      Hot = which(mdf3[["value"]] > hotLim,
                  arr.ind = TRUE)
      mdf3[["spots"]][Hot] <- "Hot"
      Cold = which(mdf3[["value"]] <= hotLim,
                   arr.ind = TRUE)
      mdf3[["spots"]][Cold] <- "Cold"
      if (length(which(mdf3[["spots"]] == "Hot")) == 0) {
        next
      }
      ## identify the peaks drawed by hotspots using density based clustering
      db <-
        fpc::dbscan(
          mdf3[which(mdf3[["spots"]] == "Hot"), c("x", "y")],
          eps = eps,
          MinPts = minPts,
          scale = scale,
          method = "hybrid"
        )
      ## extract the border of the clusters (rectangular)
      if (length(unique(db[["cluster"]])[grepl("0", unique(db[["cluster"]]))]) > 0) {
        dbclust <-
          unique(db[["cluster"]])[-which(unique(db[["cluster"]]) == 0)]
      } else {
        dbclust <- unique(db[["cluster"]])
      }
      XRanges = sapply(dbclust,
                       function(i)
                         range(mdf3[which(mdf3[["spots"]] == "Hot"), c("x", "y")][db[["cluster"]] == i, 1]))
      YRanges = sapply(dbclust,
                       function(i)
                         range(mdf3[which(mdf3[["spots"]] == "Hot"), c("x", "y")][db[["cluster"]] == i, 2]))
      ## compute centroids of each cluster
      centroids <-
        as.data.frame(do.call("rbind",
                              lapply(dbclust,
                                     function(i)
                                       apply(mdf3[which(mdf3[["spots"]] == "Hot"), c("x", "y")][db[["cluster"]] == i,], 2,
                                             mean, na.rm = T))))
      # reorder the centroids according to x axis to ensure that active is clust 1, and inactive is clust 2
      centroids <- cbind(centroids, dbclust)
      if (nrow(centroids) < 2) {
        next
      }
      centroids <- centroids[order(centroids[, "x"]),]
      # determine whether cluster belong to inactive or active category according to its coordinate on x axis (angle variance, the lower the variance, the more individual are actives)
      if (nrow(centroids) == 2) {
        inactivClust <-
          mdf3[which(mdf3[["spots"]] == "Hot"), c("x", "y", "value")][db[["cluster"]] == centroids[which(centroids[["x"]] == max(centroids[["x"]])), "dbclust"],]
        activClust <-
          mdf3[which(mdf3[["spots"]] == "Hot"), c("x", "y", "value")][db[["cluster"]] == centroids[which(centroids[["x"]] == min(centroids[["x"]])), "dbclust"],]
      } else if (nrow(centroids) > 2) {
        next
      }
      if (min(XRanges) == min(inactivClust[["x"]])) {
        next
      } else{
        print("Clusters identified !")
        break
      }
    }
    if (!exists("inactivClust", inherits = FALSE)) {
      stop(
        "failed to identify active and inactive cluster: no cluster identified, perhaps eps and minPts arguments should be modified."
      )
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
      c(mean(inactivClust[["X_values"]], na.rm = T),
        mean(inactivClust[["Y_values"]], na.rm = T))
    # get the eigenvalues and eigenvectors
    evals <- eigen(sigma2)[["values"]]
    evecs <- eigen(sigma2)[["vectors"]]
    # Set the angles of a circle
    theta <- seq(0, 2 * pi, len = 200)
    # Get critical value for inactive states
    cInact <- qchisq(0.95, 2)
    cInact <- sqrt(cInact)
    # Get the distances
    xInact <- cInact * sqrt(evals[1]) * cos(theta)
    yInact <- cInact * sqrt(evals[2]) * sin(theta)
    RawEllipseInact <- cbind(xInact, yInact)
    # Convert the coordinates
    transRawEllipseInact <- evecs %*% t(RawEllipseInact)
    transRawEllipseInact <- t(transRawEllipseInact)
    ellipseContourInact <-
      data.frame(x = transRawEllipseInact[, 1] + mu[1], y = transRawEllipseInact[, 2] + mu[2])
    ## determine the position of major and minor radius to specify two new layer for tresholding
    ### find the coordinates of the major and minor axis
    majRadInact <-
      ellipseContourInact[c(which(ellipseContourInact[["x"]] == min(ellipseContourInact[["x"]])),
                            which(ellipseContourInact[["x"]] == max(ellipseContourInact[["x"]]))), ]
    minRadInact <-
      ellipseContourInact[c(which(ellipseContourInact[["y"]] == min(ellipseContourInact[["y"]])),
                            which(ellipseContourInact[["y"]] == max(ellipseContourInact[["y"]]))), ]
    ### delimit (expend) inactive cluster to upper limit of the ellipse in y and lower in x
    ### create the tresholds
    #### for y, the threshold corresponds to the value of y for which x is minimum of the minor radius
    minRadInact <-
      minRadInact[!duplicated(minRadInact[["x"]]) &
                    !duplicated(minRadInact[["y"]]), ]
    majRadInact <-
      majRadInact[!duplicated(majRadInact[["x"]]) &
                    !duplicated(majRadInact[["y"]]), ]
    YtreshInact <-  max(minRadInact[["y"]], na.rm = T)
    #### for x, the threshold corresponds to the left limit of the ellipse (minimum x value of the major radius)
    XtreshInact <- min(majRadInact[["x"]], na.rm = T)
    # clustering the data
    toClust <-
      data.frame(var2 = var2,
                 var1 = var1)
    SigSqrt <- evecs %*% diag(sqrt(evals)) %*% t(evecs)
    Z <-
      t(apply(toClust, 1, function(i)
        solve(SigSqrt, i - mu)))
    toClust[["insideInact"]] <-
      rowSums(Z ^ 2) < stats::qchisq(0.95, df = ncol(Z))
    ### apply the thresholds previously computed
    toClust[["insideInact"]][which(toClust["var2"] > XtreshInact &
                                     toClust["var1"] < majRadInact[["y"]][which(XtreshInact == majRadInact[["x"]])])] <-
      TRUE
    toClust[["insideInact"]][which(toClust["var2"] > minRadInact[["x"]][which(YtreshInact == minRadInact[["y"]])] &
                                     toClust["var1"] < YtreshInact)] <-
      TRUE
    toClust[["activity"]] <- rep(NA, nrow(toClust))
    toClust[["activity"]][which(toClust[["insideInact"]] == TRUE)] <-
      0
    toClust[["activity"]][which(toClust[["insideInact"]] == FALSE)] <-
      1
    trackdatL[["activity2"]] <- toClust[["activity"]]
    
    if (isTRUE(graph)) {
      # display the distribution (3d density map) of the active and inactive states according to the classification (density based clustering)
      ## retrieve the result of the classification in the matrix of counts
      ZPlot <-
        t(apply(mdf3[, c(1, 2)], 1, function(i)
          solve(SigSqrt, i - mu)))
      mdf3[["insideInact"]] <-
        rowSums(ZPlot ^ 2) < stats::qchisq(0.95, df = ncol(ZPlot))
      ## apply the thresholds previously computed
      mdf3[["insideInact"]][which(mdf3["x"] > XtreshInact &
                                    mdf3["y"] < majRadInact[["y"]][which(XtreshInact == majRadInact[["x"]])])] <-
        TRUE
      mdf3[["insideInact"]][which(mdf3["x"] > minRadInact[["x"]][which(YtreshInact == minRadInact[["y"]])] &
                                    mdf3["y"] < YtreshInact)] <-
        TRUE
      mdf3[["activity"]] <- rep(NA, nrow(mdf3))
      mdf3[["activity"]][which(mdf3[["insideInact"]] == TRUE)] <-
        0
      mdf3[["activity"]][which(mdf3[["insideInact"]] == FALSE)] <-
        1
      # convert the result to density matrix
      l1 <- mdf3[which(mdf3["activity"] == 0), ]
      l2 <- mdf3[which(mdf3["activity"] == 1), ]
      dens <- stats::xtabs(value ~ y + x, mdf3[-c(4, 5, 6)])
      dens1 <- stats::xtabs(value ~ y + x, l1[-c(4, 5, 6)])
      dens2 <- stats::xtabs(value ~ y + x, l2[-c(4, 5, 6)])
      
      # draw the plot
      fig <- plotly::plot_ly(x=~colnames(dens), y=~rownames(dens), contours = list(
        z = list(
          show = TRUE,
          start = round(min(sqrt(dens)),-2),
          project = list(z = TRUE),
          end = round(max(sqrt(dens)),-2),
          size = max(sqrt(dens)) / 10,
          color = "white"
        )
      ))
      ## add inactive layer
      fig <- plotly::add_surface(
        p = fig,
        z = sqrt(dens),
        opacity = 0.8,
        colorscale = "Hot",
        cmin = min(sqrt(dens1)),
        cmax = max(sqrt(dens1)),
        colorbar = list(title = "inactive\ncounts (sqrt)")
      )
      ## add active layer
      fig <- plotly::add_surface(
        p = fig,
        z = sqrt(dens2),
        opacity = 1,
        colorscale = list(
          c(0, 0.25, 1),
          c("rgb(20,20,20)", "rgb(58,139,44)", "rgb(234,239,226)")
        ),
        #colorscale = "Greens",
        colorbar = list(title = "active\ncounts (sqrt)")
      )
      fig <- plotly::layout(
        fig,
        title = '3D density plot and 95% ellipsis of the inactive state',
        scene1 = list(
          xaxis = list(title = var2n),
          yaxis = list(title = var1n),
          zaxis = list(title = "counts (sqrt)")
        )
      )
      print(fig)
    }
    Res <- MoveR::convert2Tracklets(trackdatL, by = "trackletId")
    return(Res)
  }
