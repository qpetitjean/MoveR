#' @title trackSensitiv
#'
#' @description compare the location of individual performed manually and by tracking software to return false positive, false negative and
#' sensitivity index
#'
#' @seealso
#'
#' @param referenceDf a list of data frame containing "true" x and y coordinates of individuals (manually detected using imageJ for instance)
#' as well as the id of the frame. In case the list contains several data frame (i.e., location of individuals for several frames),
#' sensitivity index is averged and Sd, Se and n are returned
#'
#' @param trackingData A list of tracking fragments
#'
#' @param imgRes A vector of 2 numeric values, resolution of the video used as x and y limit to create a caneva
#'  (i.e., the number of pixels in image width and height, e.g., 1920 x 1080). (default = c(1920, 1080))
#'
#' @param radius a numeric value expressed in pixels and corresponding to the radius of the circle used to determine whether values
#' are considered similar to those within the referenceDF or not (default = 20)
#'
#'
#' @return a set of nested lists containing the summary of the analysis:
#' list 1: sensitivity index, Sd, Se, n (Sd, Se are only computed if the list of referenceDf is > 1)
#' list 2: a data frame containing detailed sensitivity index and frame on which test have been performed
#' list 3: the list of the False negative
#' list 4: the list of the false positive
#'
#' @authors Quentin Petitjean
#'
#'
#' @examples
#'
#' #TODO
#'
#' @export

trackSensitiv <-
  function(referenceDf,
           trackingData,
           radius = 20,
           imgRes = c(1920, 1080)) {
    sensitiv <-
      data.frame(matrix(
        ncol = 2,
        nrow = 0,
        dimnames = list(NULL, c("frame", "sensitivty"))
      ))
    FalsePId <- c()
    FalseNId <- c()
    
    # initialize progress bar
    total = length(referenceDf)
    pb <-
      progress::progress_bar$new(format = "referenceDf processing [:bar] :current/:total (:percent)", total = total)
    pb$tick(0)
    Sys.sleep(0.001)
    
    for (h in seq(length(referenceDf))) {
      #if x, y, and frame are not found
      if (is.null(list_get(referenceDf[[h]], "x"))) {
        stop(
          "for the element ",
          h,
          " in referenceDf :",
          "\n x values are not found, verify that x coordinates are stored in a column named x
         or append a column containing x coordinates in the data frame"
        )
      }
      
      if (is.null(list_get(referenceDf[[h]], "y"))) {
        stop(
          "for the element ",
          h,
          " in referenceDf :",
          "\n y values are not found, verify that y coordinates are stored in a column named y
         or append a column containing y coordinates in the data frame"
        )
      }
      
      if (is.null(list_get(referenceDf[[h]], "frame"))) {
        stop(
          "for the element ",
          h,
          " in referenceDf :",
          "\n frame values are not found, verify that frame number are stored in a column named frame
         or append a column containing frame number in the data frame"
        )
      } else if (length(unique(referenceDf[[h]]$frame)) > 1) {
        stop(
          "for the element ",
          h,
          " in referenceDf:",
          "\nframe column contains multiple frame numbers, referenceDf must be a list of df where each df is
       the location of individuals in a single frame"
        )
      } else if (length(unique(referenceDf[[h]]$frame)) == 1) {
        xref <- round(referenceDf[[h]]$x)
        yref <- round(referenceDf[[h]]$y)
        
        # 1- create a caneva based on pixels values and extract values included in radius
        location_tab = matrix(
          seq(imgRes[1] * imgRes[2]),
          nrow = imgRes[2],
          ncol = imgRes[1],
          byrow = TRUE
        )
        
        # transform the caneva in df to compute distance to the reference point
        location_tab_df = expand.grid(1:nrow(location_tab), 1:ncol(location_tab))
        
        refvalues <- list()
        for (i in seq(nrow(referenceDf[[h]]))) {
          # compute distance to the reference point
          location_tab_df$dist = sqrt((location_tab_df[2] - xref[i]) ^ 2 + (location_tab_df[1] - yref[i]) ^
                                        2)
          
          # check whether pixels value are in or out the circle that have location_tab[yind, xind] as center
          location_tab_df$inside = location_tab_df$dist < radius
          
          # extract values included in radius (reference point)
          refvalues_temp <-
            location_tab[as.matrix(location_tab_df[location_tab_df$inside, c(1:2)])]
          
          refvalues <- c(refvalues, list(refvalues_temp))
        }
        
        # 2- check whether fragments pass through reference point or not
        
        # subset the tracking data file according to the frame were true position were recorded
        FragsT <-
          map_df(trackingData, ~ as.data.frame(.x), .id = "frags_id")
        FragsTsub <- FragsT[FragsT$frame == unique(referenceDf[[h]]$frame), ]
        
        area = data.frame()
        for (j in seq(nrow(FragsTsub))) {
          indLoc <-
            location_tab[round(FragsTsub$y.pos[j]), round(FragsTsub$x.pos[j])]
          
          for (k in seq(refvalues)) {
            if (indLoc %in% refvalues[[k]]) {
              area[j, k] = TRUE
              refvalues[[k]] <- NA
              break
            } else {
              area[j, k] = FALSE
            }
          }
        }
        
        # 3 - compute index of sensitivity
        
        # Which reference has been not found in fragments (False negative = 1)
        FalseN <- vector()
        for (l in seq(ncol(area))) {
          if (!TRUE %in% area[, l]) {
            FalseN_temp = 1
          } else {
            FalseN_temp = 0
          }
          FalseN <- c(FalseN,  FalseN_temp)
        }
        
        FalseNId_temp <- referenceDf[[h]][which(FalseN == 1), ]
        FalseNId <- rbind(FalseNId, FalseNId_temp)
        
        # which fragments has been not found in reference (False positive = 1)
        FalseP <- vector()
        for (m in seq(nrow(area))) {
          if (!TRUE %in% area[m, ]) {
            FalseP_temp = 1
          } else {
            FalseP_temp = 0
          }
          FalseP <- c(FalseP,  FalseP_temp)
        }
        FalsePId_temp <- FragsTsub[which(FalseP == 1), ]
        FalsePId <- rbind(FalsePId, FalsePId_temp)
        
        # compute sensitivity index
        if (length(count(FalseP)[count(FalseP)$x == 0, 2]) == 0){
          sensitiv_temp <- c(0)
          sensitiv[h,] <-
            c(data.frame(unique(referenceDf[[h]]$frame), sensitiv_temp))
        } else if (count(FalseP)[count(FalseP)$x == 0, 2] > 0 ) {
            sensitiv_temp <-
              count(FalseP)[count(FalseP)$x == 0, 2] / (count(FalseP)[count(FalseP)$x == 0, 2] + count(FalseN)[count(FalseN)$x == 1, 2])
            sensitiv[h,] <-
              c(data.frame(unique(referenceDf[[h]]$frame), sensitiv_temp))
          }
      }
      
      # progress bar
      pb$tick(1)
      Sys.sleep(1 / 1000)
    }
    
    # 4 return the summary statistic of the analysis: list 1: sensitivity, Sd, Se, n, 
    # list 2: a data frame containing detailed sensitivity index and frame on which test have been performed,
    # list 3: list of the False negative, 
    # list 4: list of the false positive
    
    return(
      list(
        SensitivStats =
          data.frame(
            mean = mean(sensitiv$sensitivty),
            n = nrow(sensitiv),
            sd = sd(sensitiv$sensitivty),
            se = sd(sensitiv$sensitivty) / sqrt(nrow(sensitiv))
          ),
        sensitivity_details = data.frame(sensitivty = sensitiv$sensitivty,
                                         Frame = sensitiv$frame),
        FalseNegative = FalseNId,
        FalsePositive = FalsePId
      )
    )
  }
