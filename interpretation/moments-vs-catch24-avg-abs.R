#---------------------------------------
# This script calculates average absolute
# performance gain of FTM + catch22 vs
# FTM
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 19 May 2026
#---------------------------------------

library(dplyr)
library(tidyr)

#-------------- Pull data ---------------

files <- list.files("classification-models/results/")
results <- vector(mode = "list", length = length(files))

for(i in 1:length(results)){
  results[[i]] <- read.csv(paste0("classification-models/results/", files[i]))
}

results <- do.call("rbind", results)

#-------------- Calculate differences ---------------

results |>
  dplyr::select(-c(num_features)) |>
  pivot_wider(id_cols = c("problem", "resample"), names_from = "feature_set", values_from = "accuracy") |>
  mutate(.diff = catch22 - User) |>
  reframe(.mean = mean(.diff))
