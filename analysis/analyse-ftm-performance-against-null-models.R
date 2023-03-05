#------------------------------------------
# This script sets out to plot FTM results
#
# NOTE: This script requires setup.R and
# analysis/compute-mean-and-var.R to have 
# been run first
#-----------------------------------------

#--------------------------------------
# Author: Trent Henderson, 3 March 2023
#--------------------------------------

# Load data

load("data/ftm.Rda")
load("data/ftm_null.Rda")
load("data/TimeSeriesData.Rda")

# Get chance probabilities

num_classes <- TimeSeriesData %>%
  dplyr::select(c(target, problem)) %>%
  distinct() %>%
  group_by(problem) %>%
  summarise(classes = n()) %>%
  ungroup() %>%
  mutate(chance = 1 / classes)

rm(TimeSeriesData)

#------------- Final list of problems --------------

#----------------------------------
# Find out for which problems FTM 
# significantly outperformed chance
#----------------------------------

# Calculate means of actual models

avgs <- ftm %>%
  group_by(problem) %>%
  summarise(avg = mean(balanced_accuracy, na.rm = TRUE)) %>%
  ungroup()

# Calculate p-values using ECDF

p_vals <- vector(mode = "list", length = length(unique(ftm$problem)))

for(i in unique(ftm_null$problem)){
  true_bal_acc <- as.numeric(avgs[avgs$problem == i, 2])
  nulls_bal_acc <- as.numeric(ftm_null[ftm_null$problem == i, 3])
  fn_bal_acc <- stats::ecdf(nulls_bal_acc)
  p.value <- 1 - fn_bal_acc(true_bal_acc)
  p_vals[[i]] <- data.frame(p.value = p.value, problem = i, category = ifelse(p.value < .05, "Significant", "Non-Significant"))
}

p_vals <- do.call("rbind", p_vals)
save(p_vals, file = "data/p_vals.Rda")
