#' Fit classifiers over shuffles and extract performance metrics for null models
#' 
#' @param data \code{data.frame} containing time-series features
#' @param problem_name \code{string} denoting the problem to analyse
#' @param n \code{integer} denoting the number of shuffles to calculate. Defaults to \code{10000}
#' @return \code{data.frame} of classification performance
#' @author Trent Henderson
#' 

evaluate_performance_nulls <- function(data, problem_name, n = 10000){
  
  message(paste0("Doing: ", problem_name))
  
  tmp <- data %>%
    filter(problem == problem_name) %>%
    mutate(group = as.factor(as.character(group))) %>%
    dplyr::select(c(id, group, set_split, names, values)) %>%
    pivot_wider(id_cols = c(id, group, set_split), names_from = "names", values_from = "values") %>%
    dplyr::select_if(~sum(!is.na(.)) > 0) %>% # Delete features that are all NaNs
    dplyr::select(where(~dplyr::n_distinct(.) > 1)) %>% # Delete features with constant values
    pivot_longer(cols = -c(id, group, set_split), names_to = "names", values_to = "values")
  
  #------------------ Find good features to retain across resamples ---------------
  
  # Get number of cases in each set
  
  train_rows <- nrow(unique(tmp[tmp$set_split == "Train", 1]))
  test_rows <- nrow(unique(tmp[tmp$set_split == "Test", 1]))
  
  # Get proportion per class in train and test to use for resample procedure
  
  train_props <- tmp %>%
    dplyr::filter(set_split == "Train") %>%
    dplyr::select(c(id, group)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(counter = dplyr::n()) %>%
    dplyr::ungroup()
  
  test_props <- tmp %>%
    dplyr::filter(set_split == "Test") %>%
    dplyr::select(c(id, group)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(counter = dplyr::n()) %>%
    dplyr::ungroup()
  
  #-------------------------------------------------
  # Keep all features that have enough unique values
  # to not ruin models with resampling
  #-------------------------------------------------
  
  # Generate resamples
  
  res_data <- 1:1 %>%
    purrr::map(~ resample_data(tmp, train_rows = train_rows, test_rows = test_rows, train_props, test_props, .x))
  
  # Find only features across all resamples that have SD > 0
  
  good_features <- 1:1 %>%
    purrr::map(~ find_good_features(res_data, .x)) %>%
    unlist()
  
  good_features <- data.frame(names = good_features) %>%
    group_by(names) %>%
    summarise(counter = n()) %>%
    ungroup() %>%
    filter(counter == max(counter)) %>%
    pull(names)
  
  # Filter each resample by the new "good" feature vector
  
  res_data <- 1:1 %>%
    purrr::map(~ filter_good_features(res_data, .x, good_features = good_features))
  
  train <- res_data[[1]]$Train
  test <- res_data[[1]]$Test
  
  # Get numbers to rescale by from train set
  
  rescalers <- get_rescale_vals(train)
  
  # Apply rescaling
  
  train <- rescale_zscore(train, rescalers)
  test <- rescale_zscore(test, rescalers)
  
  # Compute shuffles
  
  outs <- 1:n %>%
    purrr::map_dfr(~ calculate_null(train, test, .x))
  
  outs$problem <- problem_name
  return(outs)
}

#' Helper function for null model evaluation
#' 
#' @param train_data \code{data.frame} of feature data for train set
#' @param test_data \code{data.frame} of feature data for train set
#' @param seed \code{integer} denoting fixed value for R's pseudorandom number generator
#' @author Trent Henderson
#' 

calculate_null <- function(train_data, test_data, seed){
  
  message(paste0("Shuffle ", seed))
  
  # Shuffle class labels
  
  train2 <- train_data
  train_shuffle <- sample(train2$group)
  train2$group <- train_shuffle
  
  # Fit classifier and generate predictions
  
  mod <- e1071::svm(group ~ ., data = train2, kernel = "linear", cost = 1, scale = FALSE, probability = TRUE)
  
  # Calculate balanced accuracy as the average of recalls
  
  cm <- t(as.matrix(caret::confusionMatrix(predict(mod, newdata = test_data), test_data$group)))
  bal_acc <- sum(diag(cm)) / (sum(diag(cm)) + (sum(cm) - sum(diag(cm))))
  
  results <- data.frame(model_type = "Null",
                        resample = seed,
                        balanced_accuracy = bal_acc)
  
  return(results)
}
