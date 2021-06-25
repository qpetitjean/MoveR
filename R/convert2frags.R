#' @title Convert raw tracking data to fragments list
#'
#' @description Given a list containing raw tracking data,
#' this function convert it to a list of fragments
#'
#'
#' @param tracking_data A list containing raw tracking datas 
#'
#' @param by A character vector identifying fragments to join by 
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

convert2frags <- function(tracking_data, by = NULL) {
  # convert the tracking_data list to a dataframe
  if(is.null(by)){
      stop("by argument is missing: \nimpossible to join fragments without an identifier")
  }
  tracking_data_df <-
    as.data.frame(do.call(dplyr::bind_cols, tracking_data))
  # convert the dataframe tracking_data_df to a list containing fragments data as sublists
  tracking_data_list <-
    split(tracking_data_df, list_get(tracking_data_df, by))

  return(tracking_data_list)
}
