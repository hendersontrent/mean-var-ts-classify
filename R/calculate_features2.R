#' Compute catch24 features on an input time series dataset using a modified version of theft::calculate_features
#' 
#' @import dplyr
#' @importFrom tibble as_tibble
#' @import Rcatch22
#' @param data \code{data.frame} with at least 4 columns: id variable, group variable, time variable, value variable
#' @param id_var \code{string} specifying the ID variable to identify each time series. Defaults to \code{"id"}
#' @param time_var \code{string} specifying the time index variable. Defaults to \code{"timepoint"}
#' @param values_var \code{string} specifying the values variable. Defaults to \code{"values"}
#' @param group_var \code{string} specifying the grouping variable that each unique series sits under (if one exists). Defaults to \code{NULL}
#' @param catch24 \code{Boolean} specifying whether to compute \code{catch24} in addition to \code{catch22} if \code{catch22} is one of the feature sets selected. Defaults to \code{FALSE}
#' @param seed \code{integer} denoting a fixed number for R's random number generator to ensure reproducibility
#' @param the_id \code{string} denoting the ID in the dataset to compute features for. Defaults to \code{NULL}
#' @return object of class \code{feature_calculations} that contains the summary statistics for each feature
#' @author Trent Henderson
#' @export
#' @examples
#' 

calculate_features2 <- function(data, id_var = "id", time_var = "timepoint", 
                                values_var = "values", group_var = NULL, 
                                catch24 = FALSE, seed = 123, the_id = NULL){
  
  #----------- Reshaping -----------
  
  data_re <- data %>%
    dplyr::rename(id = dplyr::all_of(id_var),
                  timepoint = dplyr::all_of(time_var),
                  values = dplyr::all_of(values_var))
  
  if(!is.null(group_var)){
    data_re <- data_re %>%
      dplyr::rename(group = dplyr::all_of(group_var)) %>%
      dplyr::select(c(.data$id, .data$timepoint, .data$values, .data$group))
  } else{
    data_re <- data_re %>%
      dplyr::select(c(.data$id, .data$timepoint, .data$values))
  }
  
  data_re <- data_re %>%
    dplyr::filter(id == the_id)
  
  #----------- Feature calculations -----------
  
  if("group" %in% colnames(data_re)){
    outData <- data_re %>%
      tibble::as_tibble() %>%
      dplyr::group_by(.data$id, .data$group) %>%
      dplyr::arrange(.data$timepoint) %>%
      tidyr::drop_na() %>%
      dplyr::summarise(Rcatch22::catch22_all(.data$values, catch24 = catch24)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(method = "catch22")
  } else{
    outData <- data_re %>%
      tibble::as_tibble() %>%
      dplyr::group_by(.data$id) %>%
      dplyr::arrange(.data$timepoint) %>%
      tidyr::drop_na() %>%
      dplyr::summarise(Rcatch22::catch22_all(.data$values, catch24 = catch24)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(method = "catch22")
  }
  
  return(outData)
}
