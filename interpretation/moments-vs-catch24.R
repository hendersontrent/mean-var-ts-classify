#---------------------------------------
# This script calculates pairwise
# statistical comparisons between feature
# sets across the UEA/UCR Repository
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 18 May 2026
#---------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(correctR)

#--------------- Define functions ---------------

#' Get train-test sample sizes for each problem
#' 
#' @return \code{data.frame} of problem summaries
#' @author Trent Henderson
#' 

get_n <- function(){
  
  n_storage <- vector(mode = "list", length = length(list.files("feature-calculations/train-test-labels/")))
  
  for(i in 1:length(n_storage)){
    rm(label)
    load(paste0("feature-calculations/train-test-labels/", list.files("feature-calculations/train-test-labels/")[i]))
    
    n_tmp <- label |>
      distinct() |>
      reframe(counter = n(), .by = c("problem", "train_test")) |>
      pivot_wider(id_cols = "problem", names_from = "train_test", values_from = "counter")
    
    n_storage[[i]] <- n_tmp
  }
  
  n_storage <- do.call("rbind", n_storage)
  return(n_storage)
}

#' Calculate winner for a given problem
#' 
#' @param data \code{data.frame} to operate on
#' @param theproblem \code{character} denoting the dataset to work on
#' @param set1name \code{character} denoting the benchmark set to focus on
#' @param problem_data \code{data.frame} containing problem summary information
#' @return object of class \code{data.frame}
#' @author Trent Henderson
#' 

find_winner <- function(data, theproblem, set1name, problem_data){
  
  tmp2 <- data |>
    filter(problem == theproblem) |>
    dplyr::select(c(resample, feature_set, accuracy)) |>
    pivot_wider(id_cols = "resample", names_from = "feature_set", values_from = "accuracy")
  
  if(is.na(colSums(tmp2[, 2])) | is.na(colSums(tmp2[, 3]))){
  } else{
    
    if(colnames(tmp2)[2] != set1name){
      tmp2 <- tmp2 |>
        dplyr::select(c(1, 3, 2))
    }
    
    # Filter to get parameters for correlated t-test
    
    params <- problem_data |>
      filter(problem == theproblem)
    
    # Set up vectors
    
    x <- as.vector(unlist(tmp2[, "catch22"]))
    y <- as.vector(unlist(tmp2[, "User"]))
    
    # Do calcs
    
    set1_name <- "catch22"
    set2_name <- "User"
    
    t_test <- correctR::resampled_ttest(x = x, y = y, n = 100, 
                                        n1 = as.integer(params$Train), n2 = as.integer(params$Test),
                                        tailed = "one", greater = "x")
    
    tmp2 <- data |>
      filter(problem == theproblem) |>
      reframe(mean_acc = mean(accuracy, na.rm = TRUE),
              .by = c("problem", "feature_set")) |>
      pivot_wider(id_cols = "problem", names_from = "feature_set", values_from = "mean_acc")
    
    if(colnames(tmp2)[2] != set1name){
      tmp2 <- tmp2 |>
        dplyr::select(c(1, 3, 2))
    }
    
    tmp2 <- tmp2 |>
      mutate(p.value = as.numeric(t_test$p.value))
    
    return(tmp2)
  }
}

#---------------
# Core operation
#---------------

#' Compute pairwise comparison between resamples of accuracy between sets
#' 
#' @param data \code{data.frame} of classification results
#' @param combn_data \code{data.frame} of pairwise feature set name combinations
#' @param rownum \code{integer} denoting the row number of the combination data to use
#' @return object of class \code{data.frame}
#' @author Trent Henderson
#' 

calculate_wins <- function(data, combn_data, rownum){
  
  # Filter to correct pairwise combination
  
  thesets <- combn_data[rownum, ]
  message(paste0("Doing: ", thesets$set1, " vs ", thesets$set2))
  
  if(thesets$set1 == thesets$set2){
    outs <- data.frame(set1 = thesets$set1, 
                       set2 = thesets$set2,
                       counter = NA,
                       props = NA,
                       total_probs = NA,
                       ties = NA)
  } else{
    
    # Filter data
    
    tmp <- data |>
      filter(feature_set %in% c(thesets$set1, thesets$set2))
    
    # Calculate winner for each problem
    
    outs <- unique(tmp$problem) |>
      purrr::map_df(~ find_winner(data = tmp, theproblem = .x, set1name = thesets$set1, problem_data = problem_summaries)) |>
      rename(set1 = 2,
             set2 = 3) |>
      mutate(p.value.adj = p.adjust(p.value, method = "holm"),
             winner = case_when(
               p.value < .05 & set1 > set2 ~ thesets$set1,
               p.value < .05 & set2 > set1 ~ thesets$set2,
               TRUE                        ~ "tie")) |>
      reframe(counter = n(), .by = "winner") |>
      mutate(total_probs = sum(counter),
             props = counter / total_probs)
  }
  return(outs)
}

#' Calculate pairwise comparisons
#' 
#' @return \code{data.frame} containing the results
#' @author Trent Henderson
#' 

h2h <- function(){
  
  # Pull data
  
  files <- list.files("classification-models/results/")
  results <- vector(mode = "list", length = length(files))
  
  for(i in 1:length(results)){
    results[[i]] <- read.csv(paste0("classification-models/results/", files[i]))
  }
  
  results <- do.call("rbind", results)
  
  # Generate pairwise combinations and map over all of them
  
  combns <- crossing(unique(results$feature_set), unique(results$feature_set), .name_repair = "unique") |>
    rename(set1 = 1, set2 = 2)
  
  combns <- combns[!duplicated(data.frame(t(apply(combns, 1, sort)))), ] |> # Remove duplicates since we get both set's values in the function
    filter(set1 != set2)
  
  wins <- 1:nrow(combns) |>
    purrr::map_df(~ calculate_wins(data = results, combn_data = combns, rownum = .x))
  
  return(wins)
}

#--------------- Run the function for all models ---------------

# Get problem summaries

problem_summaries <- get_n()

# Run comparisons

results <- h2h()
