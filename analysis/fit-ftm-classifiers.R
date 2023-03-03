#------------------------------------------
# This script sets out to compute 
# classification accuracy for FTM for all
# problems
#
# NOTE: This script requires setup.R and
# analysis/compute-mean-and-var.R to have 
# been run first
#-----------------------------------------

#--------------------------------------
# Author: Trent Henderson, 2 March 2023
#--------------------------------------

# Load data

load("data/mean_sd_test.Rda")

# Fit classifiers

ftm <- unique(mean_sd_test$problem) %>%
  purrr::map_dfr(~ evaluate_performance(mean_sd_test, .x, n_resamples = 30))

save(ftm, file = "data/ftm.Rda")

ftm_null <- unique(mean_sd_test$problem) %>%
  purrr::map_dfr(~ evaluate_performance_nulls(mean_sd_test, .x, 10000))

save(ftm_null, file = "data/ftm_null.Rda")
