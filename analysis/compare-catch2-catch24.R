#------------------------------------------
# This script sets out to compare catch2
# against catch24
#
# NOTE: This script requires setup.R,
# analysis/summarise-problems.R,
# analysis/compute-mean-and-var.R,
# analysis/compute-catch24.R and 
# analysis/fit-catch24-classifiers.R to 
# have been run first
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 10 February 2023
#------------------------------------------

# Load results

load("data/mean_sd_outputs.Rda")
load("data/catch24.Rda")

# Load problem metadata

load("data/problem_summaries.Rda")

#-------------- Do statistical comparisons ---------------


