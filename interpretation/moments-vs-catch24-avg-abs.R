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
  filter(feature_set %in% c("catch22", "Moments 1,2")) %>%
  mutate(feature_set = ifelse(feature_set == "Moments 1,2", "moments_12", feature_set)) |>
  dplyr::select(-c(num_features)) |>
  pivot_wider(id_cols = c("problem", "resample"), names_from = "feature_set", values_from = "accuracy") |>
  mutate(.diff = catch22 - moments_12) |>
  reframe(.mean = mean(.diff) * 100)
