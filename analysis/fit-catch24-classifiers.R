#------------------------------------------
# This script sets out to compute 
# classification accuracy for catch24 for
# each problem
#
# NOTE: This script requires setup.R and
# analysis/compute-features.R to have been 
# run first
#-----------------------------------------

#------------------------------------
# Author: Trent Henderson, 5 May 2022
#------------------------------------

# Load in data and summarise to just problem, ID, and train-test set indicator as I didn't bind initially

load("data/TimeSeriesData.Rda")

train_test_ids <- TimeSeriesData %>%
  dplyr::select(c(problem, id, set_split)) %>%
  distinct()

rm(TimeSeriesData) # Clean up environment as dataframe is large

# Load and bind all the feature calculations and join in set split labels

features <- list()

for(i in list.files("data/feature-calcs", full.names = TRUE, pattern = "\\.Rda")){
  load(i)
  problem_name <- gsub("data/feature-calcs/", "\\1", i)
  problem_name <- gsub(".Rda", "\\1", problem_name)
  outs$problem <- problem_name
  features[[i]] <- outs
}

features <- do.call("rbind", features)

features <- features %>%
  left_join(train_test_ids, by = c("id" = "id", "problem" = "problem"))

rm(i, outs, train_test_ids, problem_name)

#---------------- Classification accuracy -----------------

catch24 <- unique(features$problem) %>%
  purrr::map_dfr(~ evaluate_performance(features, .x, n_resamples = 30))

save(catch24, file = "data/catch24.Rda")
