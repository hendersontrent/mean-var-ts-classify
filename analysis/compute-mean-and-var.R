#------------------------------------------
# This script sets out to compute features
# for mean and SD to understand which
# problems are appropriately z-scored to
# retain
#
# NOTE: This script requires setup.R and
# analysis/prepare-time-series-data.R 
# to have been run first
#-----------------------------------------

#--------------------------------------
# Author: Trent Henderson, 2 March 2023
#--------------------------------------

# Load data

load("data/TimeSeriesData.Rda")

#------------- Feature extraction --------------

#' Function to map over datasets to avoid massive dataframe processing times / crashes
#' 
#' @param data \code{data.frame} containing raw time series
#' @param theproblem \code{string} specifying the problem to calculate features for
#' @returns an object of class \code{data.frame}
#' @author Trent Henderson
#' 

extract_mean_and_sd <- function(data, theproblem){
  
  message(paste0("Doing problem ", match(theproblem, unique(data$problem)), "/", length(unique(data$problem))))
  
  # Filter to problem of interest and calculate features
  
  outs <- data %>%
    filter(problem == theproblem) %>%
    dplyr::rename(group = target) %>%
    dplyr::select(c(.data$id, .data$timepoint, .data$values, .data$group, .data$set_split, .data$problem)) %>%
    dplyr::group_by(.data$id, .data$group, .data$set_split, .data$problem) %>%
    dplyr::arrange(.data$timepoint) %>%
    dplyr::summarise(mu = mean(.data$values, na.rm = TRUE),
                     sigma = stats::sd(.data$values, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(method = "Mean and variance") %>%
    tidyr::pivot_longer(cols = mu:sigma, names_to = "names", values_to = "values")
  
  return(outs)
}

# Run the function

extract_mean_and_sd_safe <- purrr::possibly(extract_mean_and_sd, otherwise = NULL)

mean_sd_test <- unique(TimeSeriesData$problem) %>%
  purrr::map_df(~ extract_mean_and_sd_safe(data = TimeSeriesData, theproblem = .x))

save(mean_sd_test, file = "data/mean_sd_test.Rda")
rm(TimeSeriesData)
