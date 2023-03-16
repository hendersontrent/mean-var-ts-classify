#------------------------------------------
# This script sets defines a function to 
# calculate p-values between accuracy
# resamples for catch2 and catch24
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 10 February 2023
#------------------------------------------

#' Function to calculate p-values between two feature sets using resamples
#' @param data \code{data.frame} of raw classification accuracy results
#' @param theproblem \code{string} specifying the name of the problem to calculate analyse
#' @param problem_data \code{data.frame} contain problem summary information
#' @returns object of class \code{data.frame}
#' @author Trent Henderson
#' 

calculate_p_values <- function(data, theproblem, problem_data){
  
  message(paste0("Doing: ", theproblem))
  
  tmp_data <- data %>%
    filter(problem == theproblem)
  
  # Check for only 1 feature set present
  
  if(length(unique(tmp_data$method)) <= 1){
    outs <- data.frame(problem = theproblem, statistic = NA, p.value = NA)
    return(outs)
  }
  
  # Check for 0 variance
  
  sd_check <- tmp_data %>%
    group_by(method) %>%
    summarise(stddev = sd(accuracy, na.rm = TRUE)) %>%
    ungroup()
  
  # Set up vectors
  
  x <- tmp_data %>%
    filter(method == "FTM") %>%
    pull(accuracy)
  
  y <- tmp_data %>%
    filter(method == "FTM + catch22") %>%
    pull(accuracy)
  
  # Filter to get parameters for correlated t-test
  
  params <- problem_data %>%
    filter(problem == unique(tmp_data$problem))
  
  # Do calculation
  
  if(0 %in% sd_check$stddev){
    outs <- data.frame(problem = theproblem, statistic = NA, p.value = NA)
    return(outs)
  } else{
    t_test <- resampled_ttest(x = x, y = y, n = 30, n1 = as.integer(params$Train), n2 = as.integer(params$Test))
    outs <- data.frame(problem = theproblem)
    outs <- cbind(outs, t_test)
    return(outs)
  }
}
