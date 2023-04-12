#' @title Compute sensitivity index for particles detection.
#'
#' @description Given a list of tracklets containing cartesian coordinates of particles over time
#' and a dataframe containing the "true" (i.e., manually detected) coordinates of the particles, this function
#' compare the location of the particles performed manually and by the tracking software to return a list of
#' informations related to sensitivity analysis:
#'
#' \itemize{
#'    \item{"SensitivityStats": }{a dataframe containing 4 columns summarizing the sensitivity analysis results:
#'       \itemize{
#'          \item{"mean": }{the mean sensitivity index (if particles positions are compared to manual detection over several time units).}
#'          \item{"n": }{the number of sensitivity index performed.}
#'          \item{"sd": }{the standard deviation of the mean sensitivity index.}
#'          \item{"se": }{the standard error of the mean sensitivity index.}
#'       }}
#'    \item{"SensitivityDetails": }{a dataframe containing 2 columns which gives the detailed results of the sensitivity analyses:
#'       \itemize{
#'          \item{"sensitivity": }{the sensitivity indices computed over each time unit).}
#'          \item{"timeCol": }{the time unit at which each sensitivty index have been computed.}
#'      }}
#'    \item{"FalseNegative": }{a dataframe containing 3 columns which gives the position and time of manually detected particle's that have not been detected by the tracking method:
#'       \itemize{
#'          \item{"x.pos": }{the x position of the manually detected particle's that have not been detected by the tracking method (from refDat).}
#'          \item{"y.pos": }{the y position of the manually detected particle's that have not been detected by the tracking method (from refDat).}
#'          \item{"timeCol": }{the time unit at which manual detection have been performed (from refDat).}
#'      } }
#'    \item{"FalsePositive": }{a dataframe containing the informations belonging to each particles that have been detected by the tracking method but is not truly present (i.e., not detected via manual observation)
#'    }
#' }
#'
#' @param trackDat A list of data frame containing tracking information for each tracklet (i.e., x.pos, y.pos, frame).
#'
#' @param refDat A dataframe containing "true" x and y coordinates of the particles (e.g., manually detected using imageJ)
#' as well as a column specifying the time (e.g., frame). In case the the particles are located over several time unit
#'  (i.e., location of particles for several frames), sensitivity index is averaged and Sd, Se and n are returned.
#'
#' @param radius A numeric value expressed in the same unit than x and y and corresponding to the radius of the circles
#' used to determine whether values are considered similar to those within the refDat or not (default = 20).
#'
#' @param imgRes A vector of 2 numeric values, the resolution of the video used as x and y limit of the plot (i.e., the number of pixels in image width and height).
#' If imgRes is unspecified, the function retrieve it using x and y maximum values + 5%.
#'
#' @param timeCol A character string corresponding to the name of the column containing time information (default = "frame")
#'
#' @param progress A logical value (i.e., TRUE or FALSE) indicating whether a progress bar should be displayed to inform process progression (default = TRUE).
#'
#' @return A list of dataframes summarizing the results of the sensitivity analysis:
#' \itemize{
#'          \item{"SensitivityStats": }{sensitivity index, n, standard deviation, standard error (sd, se are only computed if refDat contains particle's position over several time units).}
#'          \item{"SensitivityDetails": }{a data frame containing detailed sensitivity index and time units on which test have been performed.}
#'          \item{"FalseNegative": }{the list of the False negative (i.e., manually detected particle's that have not been detected by the tracking method.}
#'          \item{"FalsePositive": }{the list of the false positive (i.e., the informations of the particles that have been detected by the tracking method but is not truly present (i.e., not detected via manual observation).}
#'      }
#'
#' @author Quentin PETITJEAN
#'
#' @examples
#' ## Not run:
#' 
#' # Download the first dataset from the sample data repository
#' Path2Data <- MoveR::DLsampleData(dataSet = 1, tracker = "TRex")
#' Path2Data
#' 
#' # Import the list containing the 9 vectors classically used for further computation
#' Data <- MoveR::readTrex(Path2Data[[1]],
#'                         flipY = T,
#'                         imgHeight = 2160)
#' 
#' # convert it to a list of tracklets
#' trackDat <- MoveR::convert2Tracklets(Data[1:7], by = "identity")
#' 
#' # load the reference dataset (a dataframe containing manually detected position of the particle's over time unit)
#' refDat <-
#'   read.csv(Path2Data[[3]],
#'            dec = ".",
#'            sep = ";")
#' 
#' # perform the sensitivity analysis 
#' ## NB: here NAs are introduced because some tracklets in the raw data have Inf values in x and y.pos, which usually produce a warning message 
#' ## here the warning has been silenced but in this case sensitivity analysis should be preceded by a filtering step to remove Inf values (see \code{\link{filterTracklets}})
#' w <- getOption("warn")
#' options(warn = -1)
#' 
#' sensitivity <- MoveR::evalSens(
#'   refDat = refDat,
#'   trackDat = trackDat,
#'   radius = 50,
#'   imgRes = c(3840, 2160),
#'   timeCol = "frame"
#' )
#' options(warn = w)
#' 
#' # check the results
#' str(sensitivity)
#' 
#' # visualize it
#' # Draw the particle detected by the tracking method
#' # and add the position of the good detections (darkgreen),
#' # false negative and positive (red and blue, respectively)
#' # at a given time unit
#' 
#' ## retrieve the frame at which the analysis has been performed
#' TimeF <- unique(refDat[["frame"]])
#' 
#' # display the results
#' for (i in seq_along(TimeF)) {
#'   MoveR::drawTracklets(
#'     trackDat,
#'     timeWin = list(c(TimeF[[i]], TimeF[[i]])),
#'     main = paste("frame", TimeF[[i]], sep = " "),
#'     colGrad = "darkgrey",
#'     legend = FALSE,
#'     add2It = list(
#'       MoveR::circles(
#'         refDat[which(refDat[["frame"]] == TimeF[[i]]), "x.pos"],
#'         refDat[which(refDat[["frame"]] == TimeF[[i]]), "y.pos"],
#'         border = "darkgreen",
#'         radius = 50,
#'         Res = 1000,
#'         lwd = 1.5,
#'         lty = 1
#'       ),
#'       if (length(sensitivity$FalseNegative$x[sensitivity$FalseNegative$frame == TimeF[[i]]]) > 0) {
#'         MoveR::circles(
#'           x = sensitivity$FalseNegative$x[sensitivity$FalseNegative$frame == TimeF[[i]]],
#'           y = sensitivity$FalseNegative$y[sensitivity$FalseNegative$frame == TimeF[[i]]],
#'           border = "red",
#'           radius = 50,
#'           Res = 1000,
#'           lwd = 1.5,
#'           lty = 1
#'         )
#'       },
#'       if (length(sensitivity$FalsePositive$x.pos[sensitivity$FalsePositive$frame == TimeF[[i]]]) > 0) {
#'         MoveR::circles(
#'           x = sensitivity$FalsePositive$x.pos[sensitivity$FalsePositive$frame == TimeF[[i]]],
#'           y = sensitivity$FalsePositive$y.pos[sensitivity$FalsePositive$frame == TimeF[[i]]],
#'           border = "blue",
#'           radius = 50,
#'           Res = 1000,
#'           lwd = 1.5,
#'           lty = 1
#'         )
#'       }
#'     )
#'   )
#' }
#'
#' ## End(Not run)
#' @export

evalSens <-
  function(trackDat,
           refDat,
           radius = 20,
           imgRes = c(NA, NA),
           timeCol = "frame",
           progress = TRUE) {
    # transform refDat to a list of dataframe according to timeCol
    if (is.data.frame(refDat)) {
      refDat <- split(refDat, refDat[[timeCol]])
    }
    
    # if imgRes is unspecified retrieve it approximately using the maximum value in x and y coordinates
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
    
    # initialize the dataframe and vector to retrieve the results
    sensitiv <-
      data.frame(matrix(
        ncol = 2,
        nrow = length(refDat),
        dimnames = list(NULL, c("sensitivity", timeCol))
      ))
    FalsePId <- c()
    FalseNId <- c()
    if (isTRUE(progress)) {
      # initialize progress bar
      total = length(refDat)
      pb <-
        progress::progress_bar$new(format = "reference data processing [:bar] :current/:total (:percent)", total = total)
      pb$tick(0)
    }
    
    for (h in seq(length(refDat))) {
      # if x.pos, y,pos, and timeCol are not found
      if (is.null(MoveR::listGet(refDat[[h]], "x.pos"))) {
        stop(
          "for the element ",
          h,
          " in refDat :",
          "\n x values are not found, verify that x coordinates are stored in a column named x.pos
         or append a column containing x coordinates in the reference dataframe"
        )
      }
      
      if (is.null(MoveR::listGet(refDat[[h]], "y.pos"))) {
        stop(
          "for the element ",
          h,
          " in refDat :",
          "\n y values are not found, verify that y coordinates are stored in a column named y.pos
         or append a column containing y coordinates in the reference dataframe"
        )
      }
      
      if (is.null(MoveR::listGet(refDat[[h]], timeCol))) {
        stop(
          "for the element ",
          h,
          " in refDat :",
          "\n",
          timeCol,
          "column is not found, verify that timeCol argument correspond to the name of the time column
         or append a column containing the time in the reference dataframe"
        )
      } else if (length(unique(refDat[[h]][[timeCol]])) > 1) {
        stop(
          "for the element ",
          h,
          " in refDat:",
          "\nframe column contains multiple frame numbers, refDat must be a list of df where each df is
       the location of particles in a single frame"
        )
      } else if (length(unique(refDat[[h]][[timeCol]])) == 1) {
        # 1- create a caneva based on pixels values and extract values included in radius
        locTab <- matrix(
          seq(imgRes[1] * imgRes[2]),
          nrow = imgRes[2],
          ncol = imgRes[1],
          byrow = TRUE
        )
        
        ## transform the caneva in a dataframe to compute distances to the reference point
        locTabDf <-
          setNames(expand.grid(1:nrow(locTab), 1:ncol(locTab)), c("y.pos", "x.pos"))
        
        ## compute distances to the reference point and extract values included in the circle which have
        ## the reference point coordinates as center and size given by radius
        refVal <- apply(MoveR::dist2Pt(locTabDf, refDat[[h]]), 2,
                        function(k)
                          locTab[as.matrix(locTabDf[k < radius, ])])
        
        # 2- check whether the tracklets pass through reference point or not
        
        ## subsetting trackDat according to the time (timeCol) were true position were recorded
        trackT <- as.data.frame(MoveR::convert2List(trackDat))
        trackTsub <-
          trackT[trackT[[timeCol]] == unique(refDat[[h]][[timeCol]]), ]
        
        area <- data.frame(matrix(ncol = length(refVal),
                                  nrow = nrow(trackTsub)))
        for (j in seq(nrow(trackTsub))) {
          indLoc <-
            locTab[round(trackTsub[["y.pos"]][j]), round(trackTsub[["x.pos"]][j])]
          for (k in seq(length(refVal))) {
            if (indLoc %in% refVal[[k]]) {
              area[j, k] = TRUE
              refVal[[k]] <- NA
              break
            } else {
              area[j, k] = FALSE
            }
          }
        }
        
        # 3- compute index of sensitivity
        ## Which reference has been not found in tracklets (False negative = 1)
        FalseN <- c(rep(NA, ncol(area)))
        for (l in seq(ncol(area))) {
          if (!TRUE %in% area[, l]) {
            FalseN[[l]] <- 1
          } else {
            FalseN[[l]] <- 0
          }
        }
        FalseNId_temp <- refDat[[h]][which(FalseN == 1),]
        FalseNId <- rbind(FalseNId, FalseNId_temp)
        
        ## which tracklets has been not found in reference (False positive = 1)
        FalseP <- c(rep(NA, nrow(area)))
        for (m in seq(nrow(area))) {
          if (!TRUE %in% area[m,]) {
            FalseP[[m]] <- 1
          } else {
            FalseP[[m]] <- 0
          }
        }
        FalsePId_temp <- trackTsub[which(FalseP == 1),]
        FalsePId <- rbind(FalsePId, FalsePId_temp)
        
        ## count the number of false positive and false negative
        FalsePcount <- data.frame(table(FalseP))
        FalseNcount <- data.frame(table(FalseN))
        
        ## compute sensitivity index
        if (length(FalsePcount[FalsePcount[["FalseP"]] == 0, 2]) == 0) {
          sensitiv[h, timeCol] <- unique(refDat[[h]][[timeCol]])
          sensitiv[h, "sensitivity"] <- 0
        } else if (length(FalsePcount[FalsePcount[["FalseP"]] == 0, 2]) > 0) {
          sensitiv[h, timeCol] <- unique(refDat[[h]][[timeCol]])
          sensitiv[h, "sensitivity"] <-
            FalsePcount[FalsePcount[["FalseP"]] == 0, 2] / (FalsePcount[FalsePcount[["FalseP"]] == 0, 2] +
                                                              FalseNcount[FalseNcount[["FalseN"]] == 1, 2])
        }
      }
      if (isTRUE(progress)) {
        # progress bar
        pb$tick(1)
      }
    }
    
    # 4- return the summary of the analysis
    SensitivRes <- list(
      SensitivityStats =
        data.frame(
          mean = mean(sensitiv$sensitivity),
          n = nrow(sensitiv),
          sd = sd(sensitiv$sensitivity),
          se = sd(sensitiv$sensitivity) / sqrt(nrow(sensitiv))
        ),
      SensitivityDetails = sensitiv,
      FalseNegative = FalseNId,
      FalsePositive = FalsePId
    )
    return(SensitivRes)
  }
