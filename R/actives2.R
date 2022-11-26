#' @title Determine active or inactive states according to density based clustering method.
#'
#' @description Given a list of data frames containing tracking informations and two variable of interest the function use 
#' density based clustering as introduced in Ester et al. (1996) and according to the DBSCAN method from Hennig (2020) 
#' to discriminate actives and inactives states in a 2d space. 
#'
#'
#' @param trackDat A list of data frame containing tracking informations for each fragment, including a vector
#' containing behavioral patterns (e.g., behavioral states, location in areas).
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
#' @param scale A logical value indicating whether the data should be centered (substracting the mean) and scaled (dividing the standard deviation to the centered data).
#' 
#' @param na.rm A logical value indicating whether NA values should be stripped before the computation proceeds (default = TRUE).
#'
#' @param graph A logical value indicating whether the various diagnostics plots should be displayed or not (default = TRUE).
#'
#'
#' @return This function returns the results of the classification (actives vs. inactives) appended to 
#' the original list of data frame containing tracking informations for each fragment.
#'
#' @author Quentin PETITJEAN
#' 
#' @seealso \code{\link[fpc]{dbscan}}
#'
#' @references
#' \itemize{
#'          \item{Christian Hennig (2020). fpc: Flexible Procedures for Clustering. R package version 2.2-9. \href{https://CRAN.R-project.org/package=fpc}{https://CRAN.R-project.org/package=fpc}}
#'          \item{Martin Ester, Hans-Peter Kriegel, Joerg Sander, Xiaowei Xu (1996). A Density-Based Algorithm for Discovering Clusters in Large Spatial Databases with Noise. Institute for Computer Science, University of Munich. Proceedings of 2nd International Conference on Knowledge Discovery and Data Mining (KDD-96).}
#'          }
#'
#' @examples
#'
#'# Load the sample dataset
#'Data <-
#'  MoveR::readTrex(
#'    system.file("sampleData/sample_1/TREXOutput", package = "MoveR"),
#'    mirrorY = T,
#'    imgHeight = 2160,
#'    rawDat = F
#'  )
#'
#'# convert it to a list of fragments
#'trackDat <- MoveR::convert2frags(Data[1:7], by = "identity")
#'
#'# infinite values that are present in the tracking output should be removed
#'## define the filter
#'filter.Inf <-
#'  MoveR::filterFunc(
#'    trackDat,
#'    toFilter = "x.pos",
#'    customFunc = function(x)
#'      is.infinite(x)
#'  )
#'
#'### filter Infinite values
#'trackDat.Infilt <-
#'  MoveR::filterFrags(
#'    trackDat,
#'    filter = filter.Inf,
#'    splitCond = TRUE,
#'    minDur = 100
#'  )
#'
#'### remove some fragment to speed up the computation
#'trackdat2 <- trackDat.Infilt[[2]][1:75]
#'
#'# check the fragments
#'MoveR::drawFrags(trackdat2,
#'                imgRes = c(max(MoveR::convert2list(trackDat.Infilt[[2]])[["x.pos"]]),
#'                           max(MoveR::convert2list(trackDat.Infilt[[2]])[["y.pos"]])),
#'                timeCol = "frame")
#'
#'# add some metric to the dataset (speed and turning angle) and time unit conversion
#'fragsListV1 <-
#'  MoveR::analyseFrags(
#'    trackdat2,
#'    customFunc = list(
#'      # specify a first function to compute speed over each fragment (a modulus present within the MoveR package)
#'      speed = function(x)
#'        MoveR::speed(
#'          x,
#'          TimeCol = "frame",
#'          scale = 1,
#'          unit = "pixels"
#'        ),
#'      # compute turning angle in radians over each fragment (a modulus present within the MoveR package)
#'      TurnAngle = function(x)
#'        MoveR::turnAngle(x, unit = "radians")
#'    )
#'  )
#'
#'# smooth the computed metric by computing the mean value over a 10 frames' sliding window 
#'fragsListV1Smoothed <-
#'  MoveR::analyseFrags(
#'    fragsListV1,
#'    customFunc = list(
#'      # smooth variance of turning angles
#'      SlideVarAngle = function (y)
#'        MoveR::slidWin(y$TurnAngle,
#'                      Tstep = 10, function (x)
#'                        circular::var(
#'                          circular::circular(
#'                            x,
#'                            type = "angle",
#'                            units = "radians",
#'                            zero = 0
#'                          ),
#'                          na.rm = T
#'                        )),
#'      # smooth speed
#'      SlidemeanSpeed = function (y)
#'        MoveR::slidWin(y$speed,
#'                      Tstep = 10, function (x)
#'                        mean(x, na.rm = T))
#'    )
#')
#'
#'# use density based clustering to classify actives and inactives states in a 2 dimension array (here the speed and the angle variance)
#'# when graph = TRUE, several graphical output are displayed: 
#'# - the distribution of inactives states
#'# - the resuls of the density based clustering with the two groups displayed
#'# - a similar representation but as hexbinplot, with the count 
#'# - the final representation of the 2d clustering with the increasing size of the dot representing the increasing number of count
#'# - a pie chart representing the proportion of actives vs inactives states
#'
#'fragsListV1Smoothed <- MoveR::actives2(
#'  fragsListV1Smoothed,
#'  var1 = "SlidemeanSpeed",
#'  var2 = "SlideVarAngle",
#'  var1T = log10,
#'  var2T = NULL,
#'  nbins = 100,
#'  eps = 0.15,
#'  minPts = 5,
#'  scale = TRUE, 
#'  na.rm = TRUE,
#'  graph = TRUE
#') 
#'
#' @export
#'
#'
actives2 <-
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
    trackdatL <- MoveR::convert2list(trackDat)
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
    if(is.null(eps)){
      stop("eps argument is missing, specifying a maximum distance around cluster's members is needed.")
    }
    if(is.null(minPts)){
      stop("minPts argument is missing, specifying a minimum number of point per cluster is needed.")
    }

    # if na.rm argument is TRUE, remove the lines containing NA in the dataframe containing var1 and var2
    if (isTRUE(na.rm)) {
      tempDf <-
        na.omit(trackDatSmoothed)
    }
    # in case there is infinite values remove them
    if (length(which(is.infinite(tempDf[["var1"]]))) > 0) {
      tempDf <- tempDf[-c(which(is.infinite(tempDf[["var1"]]))), ]
      warning("var1 contains infinite values, these values has been removed")
    }
    if (length(which(is.infinite(tempDf[["var2"]]))) > 0) {
      tempDf <- tempDf[-c(which(is.infinite(tempDf[["var2"]]))), ]
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
    for (uptresh in IncTresh) {
      hotLim <- sum(m) / (unique(nbins) ^ 2 - uptresh)
      Hot = which(mdf3[["value"]] > hotLim,
                  arr.ind = TRUE)
      mdf3[["spots"]][Hot] <- "Hot"
      Cold = which(mdf3[["value"]] <= hotLim,
                   arr.ind = TRUE)
      mdf3[["spots"]][Cold] <- "Cold"
      if(length(which(mdf3[["spots"]] == "Hot")) == 0){
        print(
          "failed to identify active and inactive cluster: no cluster identified")
        next
      }
      ## identify the peaks drawed by hotspots using density based clustering
      db <-
        fpc::dbscan(mdf3[which(mdf3[["spots"]] == "Hot"), c("x", "y")],
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
                                       apply(mdf3[which(mdf3[["spots"]] == "Hot"), c("x", "y")][db[["cluster"]] == i, ], 2,
                                             mean, na.rm = T))))
      # reorder the centroids according to x axis to ensure that active is clust 1, and inactive is clust 2
      centroids <- cbind(centroids, dbclust)
      if (nrow(centroids) < 2) {
        next
      }
      centroids <- centroids[order(centroids[, "x"]), ]
      # determine whether cluster belong to inactive or active category according to its coordinate on x axis (angle variance, the lower the variance, the more individual are actives)
      if (nrow(centroids) == 2) {
        inactivClust <-
          mdf3[which(mdf3[["spots"]] == "Hot"), c("x", "y", "value")][db[["cluster"]] == centroids[which(centroids[["x"]] == max(centroids[["x"]])), "dbclust"], ]
        activClust <-
          mdf3[which(mdf3[["spots"]] == "Hot"), c("x", "y", "value")][db[["cluster"]] == centroids[which(centroids[["x"]] == min(centroids[["x"]])), "dbclust"], ]
      } else if (nrow(centroids) > 2) {
        print(
          "failed to identify active and inactive cluster: more than 2 clusters identified")
        next
      }
      if (min(XRanges) == min(inactivClust[["x"]])) {
        next
      } else{
        break
      }
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
                            which(ellipseContourInact[["x"]] == max(ellipseContourInact[["x"]]))),]
    minRadInact <-
      ellipseContourInact[c(which(ellipseContourInact[["y"]] == min(ellipseContourInact[["y"]])),
                            which(ellipseContourInact[["y"]] == max(ellipseContourInact[["y"]]))),]
    ### delimit (expend) inactive cluster to upper limit of the ellipse in y and lower in x
    ### create the tresholds
    #### for y, the threshold corresponds to the value of y for which x is minimum of the minor radius
    minRadInact <-
      minRadInact[!duplicated(minRadInact[["x"]]) &
                    !duplicated(minRadInact[["y"]]),]
    majRadInact <-
      majRadInact[!duplicated(majRadInact[["x"]]) &
                    !duplicated(majRadInact[["y"]]),]
    YtreshInact <-  max(minRadInact[["y"]], na.rm = T)
    #### for x, the threshold corresponds to the left limit of the ellipse (minimum x value of the major radius)
    XtreshInact <- min(majRadInact[["x"]], na.rm = T)
    # clustering the data
    toClust <-
      data.frame(var2 = var2,
                 var1 = var1)
    SigSqrt <- evecs %*% diag(sqrt(evals)) %*% t(evecs)
    Z <-
      t(apply(toClust, 1, function(x)
        solve(SigSqrt, x - mu)))
    toClust[["insideInact"]] <-
      rowSums(Z ^ 2) < stats::qchisq(0.95, df = ncol(Z))
    ### apply the thresholds previously computed
    toClust[["insideInact"]][which(toClust["var2"] > XtreshInact &
                                     toClust["var1"] < majRadInact[["y"]][which(XtreshInact == majRadInact[["x"]])])] <-
      TRUE
    toClust[["insideInact"]][which(toClust["var2"] > minRadInact[["x"]][which(YtreshInact == minRadInact[["y"]])] &
                                     toClust["var1"] < YtreshInact)] <-
      TRUE
    toClust[["activ"]] <- rep(NA, nrow(toClust))
    toClust[["activ"]][which(toClust[["insideInact"]] == TRUE)] <-
      "inactive"
    toClust[["activ"]][which(toClust[["insideInact"]] == FALSE)] <-
      "active"
    trackdatL[["actives2"]] <- toClust[["activ"]]
    
    if (isTRUE(graph)) {
      # distribution of inactive states
      plot(inactivClust,
           main = "Distribution of inactive states")
      
      # density based-clustering
      plot(
        mdf3[which(mdf3[["spots"]] == "Cold"), c("x", "y")],
        pch = 20,
        col = rgb(
          red = 0,
          green = 0,
          blue = 1,
          alpha = 0.2
        ),
        xlab = var2n,
        ylab = var1n,
        main = "2d hist and clustering of the two groups: \nactives vs inactives"
      )
      ## add point belonging to active cluster
      points(activClust[["X_values"]], activClust[["Y_values"]], col = "#99CC66", pch =
               19)
      points(inactivClust[["X_values"]],
             inactivClust[["Y_values"]],
             col = "#993333",
             pch = 19)
      # plot the inactive cluster ellipse
      lines(ellipseContourInact, col = "black")
      ### plot major and minor radius for both Otherain and inactive ellipses
      lines(majRadInact[["x"]], majRadInact[["y"]],  col = "black")
      lines(minRadInact[["x"]], minRadInact[["y"]], col = "black")
      ## add centroid of each cluster
      points(centroids, col = "black", pch = 19)

      ###### draw the Y threshold
      lines(
        x = c(minRadInact[["x"]][which(minRadInact[["y"]] == max(minRadInact[["y"]], na.rm = T))], ifelse(
          length(which(is.infinite(tempDf[["var2"]]))) > 0,
          max(tempDf[["var2"]][-which(is.infinite(tempDf[["var2"]]))], na.rm = T),
          max(tempDf[["var2"]], na.rm = T)
        ) * 2),
        y = rep(YtreshInact, 2)
        ,
        col = "black"
        
      )
    
      ###### draw the X threshold
      lines(
        x = rep(XtreshInact, 2),
        y = c(majRadInact[["y"]][which(majRadInact[["x"]] == min(majRadInact[["x"]], na.rm =
                                                                   T))], ifelse(
                                                                     length(which(is.infinite(tempDf[["var1"]]))) > 0,
                                                                     min(tempDf[["var1"]][-which(is.infinite(tempDf[["var1"]]))], na.rm = T),
                                                                     min(tempDf[["var1"]], na.rm = T)
                                                                   ) * 2),
        col = "black"
      )
      # represent the result as 2d hexbinplot
      hexplt <- hexbin::hexbinplot(
        tempDf[, "var1"] ~ tempDf[, "var2"],
        data = tempDf,
        aspect = '1',
        xbins = 40,
        xlab = var2n,
        ylab = var1n,
        main = "2d hexbinplot and 95% ellipsis of the inactive states",
        panel = function(x, y, ...) {
          hexbin::panel.hexbinplot(x, y, ...)
          lattice::panel.lines(ellipseContourInact, col = "#993333")
          lattice::panel.lines(ellipseContourInact, col = "#993333")
        }
      )
      print(hexplt)
      # represent the result as 2d hexbinplot with size of the bin corresponding to the density
      toClustNoNA <- na.omit(toClust)
      if (length(which(is.infinite(toClustNoNA[, "var1"])) > 0)) {
        toClustNoNA <-
          toClustNoNA[-c(which(is.infinite(toClustNoNA[, "var1"]))),]
      }
      with(
        toClustNoNA,
        hextri::hextri(
          tempDf[, "var2"],
          tempDf[, "var1"],
          class = toClustNoNA[["activ"]],
          colours = c("#99CC66", "#993333"),
          style = "size",
          nbins = 15,
          xlab = var2n,
          ylab = var1n,
          main = "2d clusters of activity classification",
          diffuse = FALSE,
          border = TRUE
        )
      )
      legend("topright", fill=c("#99CC66", "#993333"),
             legend=c("actives","inactives"),bty="n")
      # pie chart of behavioral state proportion
      act <-
        length(which(trackdatL[["actives2"]] == "active")) / length(trackdatL[["actives2"]][-c(which(is.na(trackdatL[["actives2"]])))]) * 100
      inact <-
        length(which(trackdatL[["actives2"]] == "inactive")) / length(trackdatL[["actives2"]][-c(which(is.na(trackdatL[["actives2"]])))]) * 100
      pie(
        main = "Proportion of active vs. inactive states",
        c(act, inact),
        labels = paste(round(
          data.frame(active = act,
                     inactive = inact),
          digits = 2
        ), "%", sep = ""),
        col = c("#99CC66", "#993333")
      )
      legend(
        .8,
        1.0,
        c("active", "inactive"),
        cex = 0.8,
        fill = c("#99CC66", "#993333")
      )
    }
    Res <- MoveR::convert2frags(trackdatL, by = "fragsId")
    return(Res)
  }
