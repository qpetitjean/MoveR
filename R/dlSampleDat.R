#' @title Download sample data for MoveR package
#'
#' @description Given the id of the dataset, this function download or retrieve the sample dataset used as example
#' to run the functions from the MoveR package.
#'
#' @param dataSet A numeric value, either 1 or 2 corresponding to the desired sampled dataset.
#'
#' @param dir The path to the directory where the sample dataset is saved, by default, the function will save it in a temporary directory (optional).
#'
#' @return This function returns a vector containing the path to the sample dataset (raw tracking data), the distance matrix from the border of the arena (performed using ImageJ) and the reference data, a .csv file containing particles location (manually detected) over several frames to perform sensitivity analyses.
#'
#' @author Quentin PETITJEAN
#'
#' @examples
#' 
#' # Here we are selecting the first sample dataset.
#' 
#' dlSampleDat(dataSet = 1)
#' 
#' # If the "dir" argument is not specified, the data are saved in a temporary directory (deleted after the R session is properly terminated).
#' # In case the sample data has already been downloaded, the function will retrieve it instead of downlading it again.
#' # This behavior is similar if the "dir" argument is specified.
#' 
#' @export

dlSampleDat <- function(dataSet = c(1, 2), dir = NA) {
  # select the sample dataset
  if (dataSet != 1 &
      dataSet != 2) {
    stop(
      "dataSet argument is not specified, please indicate either 1 or 2 to select the first or second sample dataset, respectively"
    )
  } else{
    sampleData <- paste("sample", dataSet, sep = "_")
  }
  # locate the directory where sample data will be located
  if (is.na(dir)) {
    # create a temporary directory
    td <- tempdir()
  } else{
    td <- dir
  }
  # retrieve the path to the temp dir
  dirList <- list.dirs(dirname(td), recursive = T)
  # look for previous dl of the sample data in temp dir
  TargetDirList <- dirList[grepl("MoveR_SampleData-main", dirList)]
  # if data has already been dl, retrieve the path, else dl it in temp dir
  if (length(TargetDirList) > 0) {
    dataPath <-
      dirname(TargetDirList[which.min(lengths(regmatches(
        TargetDirList, gregexpr("/", TargetDirList)
      )))])
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
  # Retrieve the path of the data
  # here we will use the sample of data given by sampleData argument
  ## path to the sample data from the previously downloaded and unziped file
  path2F <-
    paste(td, "MoveR_SampleData-main", sampleData, "TREXOutput", sep = "\\")
  
  ## path to the distance matrix from the arena edge (generated using imageJ) from the previously downloaded and unziped file
  ArenaFile <-
    paste(
      td,
      "MoveR_SampleData-main",
      sampleData,
      "ReferenceData",
      "DistMatrixFromArenaEdge.txt",
      sep = "\\"
    )
  ## path to the reference data, a .csv file containing particles location (manually detected) over several frames to perform sensitivity analyses
  ### find whether RefDat file exist
  RefDat <- list.files("D:/Postdoc_INRAE_SAM/MoveR_SampleData/sample_1/ReferenceData", "RefDat")
  if(length(RefDat) > 0){
    refDatPath <-
      paste(
        td,
        "MoveR_SampleData-main",
        sampleData,
        "ReferenceData",
        RefDat,
        sep = "\\"
      )
  }else{
    refDatPath <- NULL
  }
  return(c(path2F, ArenaFile, refDatPath))
}