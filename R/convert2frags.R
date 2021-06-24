#' @title Convert raw tracking data to fragments list
#'
#' @description Given a list containing raw tracking data,
#' this function convert it to a list of fragments
#'
#'
#' @param tracking_data A list containing raw tracking datas 
#'
#'
#' @return A list of fragments
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

convert2frags <- function(tracking_data) {
  # convert the tracking_data list to a dataframe
  tracking_data_df <-
    as.data.frame(do.call(bind_cols, tracking_data[1:7]))
  # convert the dataframe tracking_data_df to a list containing fragments data as sublists
  tracking_data_list <-
    split(tracking_data_df, tracking_data_df$identity)
  # add fragments name fragments to each sublist
  names(tracking_data_list) <-
    paste("frags", (1:length(tracking_data_list)), sep = "_")
  
  return(tracking_data_list)
  
}
