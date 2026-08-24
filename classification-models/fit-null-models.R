#---------------------------------------
# This script fits a linear SVM for each
# problem after randomly shuffling the
# class labels, to generate an empirical
# null (permutation) distribution of
# classification accuracy
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 4 August 2026
#---------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(e1071)

#--------------- Define functions ---------------

#' Calculate central tendency and spread values for all numeric columns in a dataset
#'
#' @param data \code{matrix} containing data to normalise
#' @return \code{list} of central tendency and spread values
#' @author Trent Henderson
#'

get_rescale_vals <- function(data){
  ct <- colMeans(data, na.rm = TRUE)
  spreads <- apply(data, 2, sd, na.rm = TRUE)
  outs <- list(ct, spreads)
  names(outs) <- c("CentralTendency", "Spread")
  return(outs)
}

#' Calculate z-score for all columns in a dataset using train set central tendency and spread
#'
#' @param data \code{matrix} containing data to normalise
#' @param rescalers \code{list} containing central tendency and spread values for the train set
#' @return \code{matrix} of rescaled data
#' @author Trent Henderson
#'

rescale_zscore <- function(data, rescalers){
  sweep(
    sweep(data, 2, rescalers$CentralTendency, "-"), 2, rescalers$Spread, "/"
  )
}

#' Helper function to generate IDs for resamples
#'
#' @param features \code{feature_calculations} object containing feature data
#' @param train_counts \code{data.frame} denoting number of train samples
#' @param test_counts \code{data.frame} denoting number of test samples
#' @return \code{list} of train-test IDs
#' @author Trent Henderson
#'

generate_resample_indx <- function(features, train_counts, test_counts){

  all_ids <- features |> dplyr::distinct(id, group)
  train_ids <- c()
  test_ids  <- c()

  for(g in unique(all_ids$group)){

    ids_g <- all_ids |>
      dplyr::filter(group == g) |>
      dplyr::pull(id)

    n_train_g <- train_counts |>
      dplyr::filter(group == g) |>
      dplyr::pull(n)

    n_test_g <- test_counts |>
      dplyr::filter(group == g) |>
      dplyr::pull(n)

    total_needed <- n_train_g + n_test_g
    sampled <- sample(ids_g, size = total_needed, replace = FALSE)
    train_ids <- c(train_ids, sampled[seq_len(n_train_g)])
    test_ids  <- c(test_ids,  sampled[(n_train_g + 1):total_needed])
  }

  return(list(train_ids = train_ids, test_ids = test_ids))
}

#' Randomly permute the class labels at the level of the time series (i.e., `id`), so
#' every feature and feature set for a given `id` carries the same shuffled label
#'
#' @param feature_data \code{data.frame} of feature data containing `id`, `group`, and `train_test` columns
#' @param stratify_by_split \code{Boolean} whether to permute labels separately within the train and
#' test sets, which retains the exact class balance of each set. Defaults to \code{TRUE}
#' @return \code{data.frame} of feature data with permuted labels
#' @author Trent Henderson
#'

shuffle_labels <- function(feature_data, stratify_by_split = TRUE){

  id_map <- feature_data |> dplyr::distinct(id, group, train_test)

  if(stratify_by_split){
    id_map <- id_map |>
      dplyr::group_by(train_test) |>
      dplyr::mutate(group = sample(group)) |> # Permutation within split retains class proportions of that split
      dplyr::ungroup()
  } else{
    id_map$group <- sample(id_map$group) # Permutation over the entire dataset
  }

  feature_data$group <- id_map$group[match(feature_data$id, id_map$id)]
  return(feature_data)
}

#' Function to partition features into MECE moments feature 'sets'
#'
#' @param feature_data \code{data.frame} of feature data
#' @return \code{data.frame} of results
#' @author Trent Henderson
#'

partition <- function(feature_data){

  # Mean

  moment1 <- feature_data |>
    filter(feature_set == "moments") |>
    filter(names == "mean") |>
    mutate(feature_set = "Moment 1")

  # Mean and variance

  moment12 <- feature_data |>
    filter(feature_set == "moments") |>
    filter(names %in% c("mean", "variance")) |>
    mutate(feature_set = "Moments 1,2")

  # Mean, variance, and skewness

  moment123 <- feature_data |>
    filter(feature_set == "moments") |>
    filter(names %in% c("mean", "variance", "skewness")) |>
    mutate(feature_set = "Moments 1,2,3")

  # Mean, variance, skewness, and kurtosis

  moment1234 <- feature_data |>
    filter(feature_set == "moments") |>
    filter(names %in% c("mean", "variance", "skewness", "kurtosis")) |>
    mutate(feature_set = "Moments 1,2,3,4")

  # catch22

  catch22 <- feature_data |>
    filter(feature_set == "catch22")

  #---------- Bind together ---------

  bound <- bind_rows(moment1, moment12, moment123, moment1234, catch22)
  return(bound)
}

#' Function that can iterate over problems and save outputs, where the class labels are
#' randomly permuted prior to each resample
#'
#' @param problem \code{character} denoting the dataset to work on
#' @param N \code{integer} denoting the number of permutations to compute. Defaults to \code{1}
#' @param seed \code{integer} denoting the fix for pseudorandom reproducibility
#' @param stratify_by_split \code{Boolean} whether to permute labels separately within the train and
#' test sets. Defaults to \code{TRUE}
#' @return \code{data.frame} containing null classification accuracy values for each feature set
#' @author Trent Henderson
#'

fit_null_models <- function(problem, N = 1, seed = 123, stratify_by_split = TRUE){

  cat(paste0("Evaluating: ", problem, "\n"))

  set.seed(seed)

  if(paste0(problem, ".csv") %in% list.files("classification-models/null-results/")){ # Prevent unnecessary re-runs
    return(NA)
  } else{

    #----------
    # Load data
    #----------

    suppressWarnings(rm(list = intersect(c("features", "label"), ls())))
    load(paste0("feature-calculations/features/", problem, ".Rda"))
    load(paste0("feature-calculations/train-test-labels/", problem, ".Rda"))

    label <- label |>
      dplyr::select(-c(problem)) |>
      dplyr::distinct() # Fixed upstream in code in feature-calculations/calculate-features.R but I had already run everything...

    features <- features |>
      dplyr::inner_join(label, by = c("id" = "id"))

    features <- partition(features)
    feature_sets <- unique(features$feature_set)

    #--------------------------
    # Compute class proportions
    #--------------------------

    # NOTE: these are computed from the *real* labels and canonical split, so every
    # permutation is evaluated on train-test sets of exactly the same size and class
    # balance as the observed analysis

    train_counts <- features |>
      dplyr::filter(train_test == "Train") |>
      dplyr::distinct(id, group) |>
      dplyr::count(group)

    test_counts <- features |>
      dplyr::filter(train_test == "Test") |>
      dplyr::distinct(id, group) |>
      dplyr::count(group)

    #----------------
    # Do permutations
    #----------------

    results <- list()
    counter <- 1

    for(r in seq_len(N)){

      message(paste0("Permutation: ", r))

      # Shuffle class labels prior to any model fitting. This is done once per permutation
      # (not once per feature set) so all feature sets within a permutation see the same
      # null labels, and it is done prior to the split so both train and test are permuted

      features_r <- shuffle_labels(features, stratify_by_split = stratify_by_split)

      if(r == 1){ # Canonical train-test split with permuted labels
        features_train_r <- features_r |> dplyr::filter(train_test == "Train")
        features_test_r <- features_r |> dplyr::filter(train_test == "Test")
      } else{
        idx <- generate_resample_indx(features_r, train_counts, test_counts)
        features_train_r <- features_r |> dplyr::filter(id %in% idx$train_ids)
        features_test_r <- features_r |> dplyr::filter(id %in% idx$test_ids)
      }

      # Execute main loop over feature sets

      for(i in feature_sets){

        message(paste0("Evaluating feature set: ", i))

        # Train

        x_train <- features_train_r |>
          dplyr::filter(feature_set == i) |>
          tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") |>
          dplyr::select(-c(id))

        y_train <- x_train$group
        x_train <- x_train |> dplyr::select(-group) |> as.matrix()
        x_train <- x_train[, colSums(is.na(x_train)) == 0, drop = FALSE] # Remove features that have NA values
        x_train <- x_train[, apply(x_train, 2, function(x) length(unique(x)) > 1), drop = FALSE] # Remove constant columns
        x_train <- x_train[, apply(x_train, 2, var, na.rm = TRUE) != 0, drop = FALSE] # Remove columns where SD = 0

        # Test

        x_test <- features_test_r |>
          dplyr::filter(feature_set == i) |>
          tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") |>
          dplyr::select(-c(id))

        y_test <- x_test$group
        x_test <- x_test |> dplyr::select(-group) |> as.matrix()
        x_test <- x_test[, colnames(x_test) %in% colnames(x_train), drop = FALSE]
        x_test <- x_test[, colSums(is.na(x_test)) == 0, drop = FALSE]
        x_test <- x_test[, apply(x_test, 2, function(x) length(unique(x)) > 1), drop = FALSE]
        x_test <- x_test[, apply(x_test, 2, var, na.rm = TRUE) != 0, drop = FALSE]
        x_train <- x_train[, colnames(x_train) %in% colnames(x_test), drop = FALSE] # Filter to those that met the criteria for valid columns back the other way
        x_test <- x_test[, colnames(x_train), drop = FALSE] # Filter to those that met the criteria for valid columns back the other way -- avoids some errors in column number differences...

        if(ncol(x_train) == 0 || ncol(x_test) == 0){
          tmp <- data.frame(feature_set = i, num_features = 0, accuracy = NA, resample = r)
          results[[counter]] <- tmp
          counter <- counter + 1
        } else{

          # Normalise data matrices using values from train data to keep test unseen

          rescalers <- get_rescale_vals(x_train)
          x_train <- rescale_zscore(x_train, rescalers)
          x_test <- rescale_zscore(x_test, rescalers)

          # Fit model and compute accuracy metrics

          train_df <- as.data.frame(x_train)
          train_df$group <- y_train
          mod <- try(e1071::svm(group ~ ., data = train_df, kernel = "linear", scale = FALSE))

          if(inherits(mod, "try-error")){
            acc <- NA
          } else{
            y_pred <- predict(mod, newdata = x_test)
            cm <- table(y_pred, y_test)
            acc <- sum(diag(cm)) / sum(cm)
          }

          tmp <- data.frame(
            feature_set = i,
            num_features = ncol(x_train),
            accuracy = acc,
            resample = r
          )

          results[[counter]] <- tmp
          counter <- counter + 1
        }
      }
    }

    # Bind results together

    results <- do.call(rbind, results)
    results$problem <- problem

    write.csv(results, paste0("classification-models/null-results/", problem, ".csv"), row.names = FALSE)
  }
}

# Run the null classifiers

if(!dir.exists("classification-models/null-results")){
  dir.create("classification-models/null-results")
}

gsub(".Rda", "\\1", list.files("feature-calculations/features")) |>
  purrr::map_dfr(~fit_null_models(problem = .x, N = 100, seed = 123))

#--------------- Compute empirical chance probabilities ---------------

null_files <- list.files("classification-models/null-results/")

null_results <- null_files |>
  purrr::map_dfr(~read.csv(paste0("classification-models/null-results/", .x)))

# Empirical chance probability for each problem x feature set

empirical_chance <- null_results |>
  filter(feature_set == "Moment 1") |>
  reframe(chance = mean(accuracy, na.rm = TRUE), .by = "problem")

write.csv(empirical_chance, "classification-models/empirical-chance.csv", row.names = FALSE)
