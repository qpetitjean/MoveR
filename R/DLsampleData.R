#' @title Download sample data to depict the use of the MoveR package.
#'
#' @description Given the id of a sample dataset stored in \href{https://github.com/qpetitjean/MoveR_SampleData}{MoveR_SampleData Github repository}, 
#' this function download the selected sample dataset used as example to run the functions from the MoveR package and returns the local path to access the data.
#'
#' @param dataSet A numeric value, either 1 or 2 corresponding to the desired sampled dataset.
#'
#' @param dir The path to the directory where the sample dataset is saved, by default, the function will save it in a temporary directory (optional).
#'
#' @param tracker A character string corresponding to the name of the tracking software from which data have to be downloaded: Ctrax, IdTracker, TrackR and/or TRex. By default the function returns the path
#' to sample data from each tracking software.
#'
#' @return This function returns a vector containing the path to: 
#'  \itemize{
#'    \item{the selected sample dataset (raw tracking data).}
#'    \item{the distance matrix from the border of the arena (color tresholding - greyscale - performed using ImageJ).}
#'    \item{the reference data (a .csv file containing particles location -manually detected- over several frames to perform sensitivity analyses).}
#'    \item{the filtered/cleaned version of the tracking data (filtered using the approach described in the MoveR-Clean-FilterData vignette).}
#' }
#' Also, in case the sample dataset has already been downloaded in a temporary (default) or specified directory, the function retrieve the data instead of downloading it again.
#'
#' @author Quentin PETITJEAN
#'
#' @examples
#' ## Not run:
#' 
#' # Here we are selecting the first sample dataset, that have been tracked using \href{https://trex.run}{TRex}
#'
#' DLsampleData(dataSet = 1, tracker = "TRex")
#'
#' # If the "dir" argument is not specified, the data are saved in a temporary directory (deleted after the R session is properly terminated).
#' # In case the sample data has already been downloaded, the function will retrieve it instead of downloading it again.
#' # This behavior is similar if the "dir" argument is specified.
#'
#' ## End(Not run)
#' @export

DLsampleData <-
  function(dataSet = c(1, 2),
           tracker = c("Ctrax", "IdTracker", "TrackR", "TRex"),
           dir = NULL) {
    # select the sample dataset
    if (dataSet != 1 &
        dataSet != 2) {
      stop(
        "dataSet argument is not specified, please indicate either 1 or 2 to select the first or second sample dataset, respectively"
      )
    } else{
      sampleData <- paste("sample", dataSet, sep = "_")
    }
    if (!tracker %in% c("Ctrax", "IdTracker", "TrackR", "TRex")) {
      stop(
        "tracker argument is misspelled, please indicate from which tracking software data have to be downloaded: Ctrax, IdTracker, TrackR and/or TRex"
      )
    }
    # locate the directory where sample data will be located
    if (is.null(dir)) {
      # create a temporary directory
      td <- tempdir()
    } else{
      td <- dir
    }
    # retrieve the path to the temp dir
    dirList <- list.dirs(dirname(td), recursive = T)
    # look for previous dl of the sample data in temp dir
    TargetDirList <-
      dirList[grepl("MoveR_SampleData-main", dirList)]
    # if data has already been dl, retrieve the path, else dl it in temp dir
    if (length(TargetDirList) > 0) {
      td <-
        gsub("/", "\\\\", dirname(TargetDirList[which.min(lengths(regmatches(
          TargetDirList, gregexpr("/", TargetDirList)
        )))]))
    } else{
      # download sample data from the "https://github.com/qpetitjean/MoveR_SampleData" github repository
      ## get the file url
      url = "https://github.com/qpetitjean/MoveR_SampleData/archive/refs/heads/main.zip"
      ## create the placeholder file
      tf <- tempfile(tmpdir = td, fileext = ".zip")
      ## download the repository's files into the placeholder file
      download.file(url, tf)
      ## unzip the file to the temporary directory
      unzip(tf, exdir = td, overwrite = TRUE)
      ## remove the .zip file from the temp directory
      unlink(tf)
    }
    # Retrieve the path to the data
    # here we will use the sample of data given by dataSet argument
    ## path to the sample data from the previously downloaded and unziped file
    path2F <- c()
    for (i in seq_along(tracker)) {
      if (tracker[i] != "TRex") {
        TempPath <-
          paste(td,
                "MoveR_SampleData-main",
                sampleData,
                paste0(tracker[i], "Output"),
                sep = "\\")
        path2FTemp <-
          paste(TempPath, list.files(TempPath), sep = "\\")
      } else{
        path2FTemp <- paste(td,
                            "MoveR_SampleData-main",
                            sampleData,
                            paste0(tracker[i], "Output"),
                            sep = "\\")
      }
      path2F <- c(path2F, path2FTemp)
    }
    ## path to the distance matrix from the arena edge (generated using imageJ) from the previously downloaded and unziped file
    MatrixDat <-
      list.files(
        file.path(td,
                  "MoveR_SampleData-main",
                  sampleData,
                  "ReferenceData"),
        "DistMatrix"
      )
    ArenaFile <-
      paste(td,
            "MoveR_SampleData-main",
            sampleData,
            "ReferenceData",
            MatrixDat,
            sep = "\\")
    ## path to the reference data, a .csv file containing particles location (manually detected) over several frames to perform sensitivity analyses
    ### find whether RefDat file exist
    RefDat <-
      list.files(file.path(td,
                           "MoveR_SampleData-main",
                           sampleData,
                           "ReferenceData"),
                 "RefDat")
    if (length(RefDat) > 0) {
      refDatPath <-
        paste(td,
              "MoveR_SampleData-main",
              sampleData,
              "ReferenceData",
              RefDat,
              sep = "\\")
    } else{
      refDatPath <- NULL
    }
    ## path to the filtered/cleaned dataset (filtered using the procedure detailed) in the MoveR-Clean-FilterData vignette
    ### find whether CleanDat file exist
    CleanDat <-
      list.files(file.path(td,
                           "MoveR_SampleData-main",
                           sampleData), 
                 "cleaned")
    if (length(CleanDat) > 0) {
      CleanDatPath <-
        paste(td,
              "MoveR_SampleData-main",
              sampleData,
              CleanDat,
              sep = "\\")
    } else{
      CleanDatPath <- NULL
    }
    
    return(c(path2F, ArenaFile, refDatPath, CleanDatPath))
  }