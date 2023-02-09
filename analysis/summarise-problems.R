#------------------------------------------
# This script produces a summary dataframe
# of descriptives for each dataset that
# can be easily called in correlated t-test
# analyses
#
# NOTE: This script requires setup.R to 
# have been run first
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 10 February 2023
#------------------------------------------

load("data/TimeSeriesData.Rda")

# Summarise dataset

problem_summaries <- TimeSeriesData %>%
  dplyr::select(c(problem, set_split, id)) %>%
  distinct() %>%
  group_by(problem, set_split) %>%
  summarise(counter = n()) %>%
  ungroup() %>%
  pivot_wider(id_cols = "problem", names_from = "set_split", values_from = "counter") %>%
  dplyr::select(c(problem, Train, Test))

save(problem_summaries, file = "data/problem_summaries.Rda")
rm(TimeSeriesData)
