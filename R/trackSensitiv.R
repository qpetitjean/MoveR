#' @title Compute the sensitivity index of particles detection
#'
#' @description Given a list of tracking fragments containing cartesian coordinates of particles over time
#' and a dataframe containing the "true" (i.e., manually detected) coordinates of the particles, this function
#' compare the location of the particles performed manually and by the tracking software to return a list of
#' informations related to sensitivity analysis:
#'
#' \itemize{
#'    \item{"Sensitivity_Stats": }{a dataframe containing 4 columns summarizing the sensitivity analysis results:
#'       \itemize{
#'          \item{"mean": }{the mean sensitivity index (if particles positions are compared to manual detection over several time units).}
#'          \item{"n": }{the number of sensitivity index performed.}
#'          \item{"sd": }{the standard deviation of the mean sensitivity index.}
#'          \item{"se": }{the standard error of the mean sensitivity index.}
#'       }}
#'    \item{"sensitivity_Details": }{a dataframe containing 2 columns which gives the detailed results of the sensitivity analyses:
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
#'    \item{"FalsePositive": }{a dataframe containing 10 columns which retrieves the informations of the particles that have been detected by the tracking method but is not truly present (i.e., not detected via manual observation):
#'       \itemize{
#'          \item{"see \code{\link{readCtrax}},
#'          \code{\link{readTrackR}},
#'          \code{\link{readTrex}},
#'          \code{\link{readIdtracker}}"}
#'      }
#'    }
#' }
#'
#' @param trackDat A list of data frame containing tracking information for each fragment (i.e., x.pos, y.pos, frame).
#'
#' @param refDat A dataframe containing "true" x and y coordinates of the particles (e.g., manually detected using imageJ)
#' as well as a column specifying the time (e.g., frame). In case the the particles are located over several time unit
#'  (i.e., location of individuals for several frames), sensitivity index is averaged and Sd, Se and n are returned.
#'
#' @param radius A numeric value expressed in the same unit than x and y and corresponding to the radius of the circles
#' used to determine whether values are considered similar to those within the refDat or not (default = 20).
#'
#' @param imgRes A vector of 2 numeric values, resolution of the video used as x and y limit of the plot
#'  (e.g., the number of pixels in image width and height, default = 1920 x 1080).
#'
#' @param timeCol A character string corresponding to the name of the column containing time information (default = "frame")
#'
#' @return A list of dataframes summarizing the results of the sensitivity analysis:
#' \itemize{
#'          \item{"Sensitivity_Stats": }{sensitivity index, n, standard deviation, standard error (sd, se are only computed if refDat contains particle's position over several time units).}
#'          \item{"sensitivity_Details": }{a data frame containing detailed sensitivity index and time units on which test have been performed.}
#'          \item{"FalseNegative": }{the list of the False negative (i.e., manually detected particle's that have not been detected by the tracking method.}
#'          \item{"FalsePositive": }{the list of the false positive (i.e., the informations of the particles that have been detected by the tracking method but is not truly present (i.e., not detected via manual observation).}
#'      }
#'
#' @authors Quentin PETITJEAN
#'
#' @examples
#'
#'# load the sample data
#'Data <-
#'  readTrex(
#'    system.file("sampleData/sample_1/TREXOutput", package = "MovR"),
#'    mirrorY = T,
#'    imgHeight = 2160,
#'    rawDat = F
#'  )
#'# convert it to a list of fragments
#'trackDat <- convert2frags(Data[1:7], by = "identity")
#'
#'# load the reference dataset (a dataframe containing manually detected position of the particle's over time unit)
#'refDat <-
#'  read.csv(
#'    system.file("sampleData/sample_1/ReferenceData", package = "MovR"),
#'    dec = ".",
#'    sep = ";"
#'  )
#'
#'# perform the sensitivity analysis
#'sensitivity <- trackSensitiv(
#'  refDat = refDat,
#'  trackDat = trackDat,
#'  radius = 50,
#'  imgRes = c(3840, 2160),
#'  timeCol = "frame"
#')
#'
#'# Draw the particle detected by the tracking method
#'# and add the position of the good detections (darkgreen),
#'# false negative and positive (red and blue, respectively)
#'# at a given time unit (here the frame 2700)
#'TimeU <- 2700
#'
#'drawFrags(
#'  trackDat,
#'  imgRes = c(3840, 2160),
#'  timeWin = list(c(TimeU, TimeU)),
#'  add2It = list(
#'    circles(
#'     refDat[which(refDat[["frame"]] == TimeU), "x.pos"],
#'      refDat[which(refDat[["frame"]] == TimeU), "y.pos"],
#'      border = "darkgreen",
#'      radius = 50,
#'      Res = 1000,
#'      lwd = 1.5,
#'      lty = 1
#'    ),
#'    if (length(sensitivity$FalseNegative$x[sensitivity$FalseNegative$frame == TimeU]) > 0) {
#'      circles(
#'        x = sensitivity$FalseNegative$x[sensitivity$FalseNegative$frame == TimeU],
#'        y = sensitivity$FalseNegative$y[sensitivity$FalseNegative$frame == TimeU],
#'        border = "red",
#'        radius = 50,
#'        Res = 1000,
#'        lwd = 1.5,
#'        lty = 1
#'      )
#'    },
#'    if (length(sensitivity$FalsePositive$x.pos[sensitivity$FalsePositive$frame == TimeU]) > 0) {
#'      circles(
#'        x = sensitivity$FalsePositive$x.pos[sensitivity$FalsePositive$frame == TimeU],
#'        y = sensitivity$FalsePositive$y.pos[sensitivity$FalsePositive$frame == TimeU],
#'        border = "blue",
#'        radius = 50,
#'        Res = 1000,
#'        lwd = 1.5,
#'        lty = 1
#'      )
#'    }
#'  )
#')
#'
#' @export

trackSensitiv <-
  function(trackDat,
           refDat,
           radius = 20,
           imgRes = c(1920, 1080),
           timeCol = "frame") {
    # transform refDat to a list of dataframe according to timeCol
    if (is.data.frame(refDat)) {
      refDat <- split(refDat, refDat[[timeCol]])
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
    
    # initialize progress bar
    total = length(refDat)
    pb <-
      progress::progress_bar$new(format = "reference data processing [:bar] :current/:total (:percent)", total = total)
    pb$tick(0)
    Sys.sleep(0.001)
    
    for (h in seq(length(refDat))) {
      # if x.pos, y,pos, and timeCol are not found
      if (is.null(listGet(refDat[[h]], "x.pos"))) {
        stop(
          "for the element ",
          h,
          " in refDat :",
          "\n x values are not found, verify that x coordinates are stored in a column named x.pos
         or append a column containing x coordinates in the reference dataframe"
        )
      }
      
      if (is.null(listGet(refDat[[h]], "y.pos"))) {
        stop(
          "for the element ",
          h,
          " in refDat :",
          "\n y values are not found, verify that y coordinates are stored in a column named y.pos
         or append a column containing y coordinates in the reference dataframe"
        )
      }
      
      if (is.null(listGet(refDat[[h]], timeCol))) {
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
       the location of individuals in a single frame"
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
        refVal <- apply(dist2Pt(locTabDf, refDat[[h]]), 2,
                        function(k)
                          locTab[as.matrix(locTabDf[k < radius,])])
        
        # 2- check whether the fragments pass through reference point or not
        
        ## subsetting trackDat according to the time (timeCol) were true position were recorded
        FragsT <- as.data.frame(convert2list(trackDat))
        FragsTsub <-
          FragsT[FragsT[[timeCol]] == unique(refDat[[h]][[timeCol]]),]
        
        area <- data.frame(matrix(ncol = length(refVal),
                                  nrow = nrow(FragsTsub)))
        for (j in seq(nrow(FragsTsub))) {
          indLoc <-
            locTab[round(FragsTsub[["y.pos"]][j]), round(FragsTsub[["x.pos"]][j])]
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
        
        # 3 - compute index of sensitivity
        ## Which reference has been not found in fragments (False negative = 1)
        FalseN <- c(rep(NA, ncol(area)))
        for (l in seq(ncol(area))) {
          if (!TRUE %in% area[, l]) {
            FalseN[[l]] <- 1
          } else {
            FalseN[[l]] <- 0
          }
        }
        FalseNId_temp <- refDat[[h]][which(FalseN == 1), ]
        FalseNId <- rbind(FalseNId, FalseNId_temp)
        
        ## which fragments has been not found in reference (False positive = 1)
        FalseP <- c(rep(NA, nrow(area)))
        for (m in seq(nrow(area))) {
          if (!TRUE %in% area[m, ]) {
            FalseP[[m]] <- 1
          } else {
            FalseP[[m]] <- 0
          }
        }
        FalsePId_temp <- FragsTsub[which(FalseP == 1), ]
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
      
      # progress bar
      pb$tick(1)
      Sys.sleep(1 / 1000)
    }
    
    # 4 return the summary of the analysis
    SensitivRes <- list(
      Sensitivity_Stats =
        data.frame(
          mean = mean(sensitiv$sensitivity),
          n = nrow(sensitiv),
          sd = sd(sensitiv$sensitivity),
          se = sd(sensitiv$sensitivity) / sqrt(nrow(sensitiv))
        ),
      sensitivity_Details = sensitiv,
      FalseNegative = FalseNId,
      FalsePositive = FalsePId
    )
    str(SensitivRes)
    return(SensitivRes)
  }