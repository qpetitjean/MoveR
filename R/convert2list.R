#' @title Convert fragments list to simple list
#'
#' @description Given a nested list containing fragments sublists 
#' to a simple list as original raw data for use in track_stat
#'
#'
#' @param tracking_data A list of data frame containing tracking informations for each fragment
#' (e.g., maj.ax, angle, min.ax, x.pos, y.pos, ...)
#'
#'
#' @return A list of tracking data as in raw file
#'
#' @authors Quentin Petitjean
#'
#'
#'
#' @examples
#'
#' # TODO
#'
#' @export

convert2list <- function(tracking_data) {
  tracking_data_list <- list()
  # convert the fragments list nested in tracking_data to a dataframe and add the fragment id
  tracking_data_df <- do.call("rbind", tracking_data)
  tracking_data_df$frags_id <-
    gsub("[.][0-9]*", "", rownames(tracking_data_df))
  # then transfrom each df column in one element of the list
  for (i in names(tracking_data_df)) {
    tracking_data_list[[i]] <- unname(as.matrix(tracking_data_df[, i]))
    
  }
  return(tracking_data_list)
} 