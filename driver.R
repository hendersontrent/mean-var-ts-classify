#-----------------------------------------
# This script can be used to run the whole
# project in order from start to finish
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 9 February 2023
#-----------------------------------------

source("setup.R")

#---------------- UEA & UCR Repository analysis ------------

# Prepare time-series datasets

source("analysis/prepare-time-series-data.R")

# Calculate mean and variance features

source("analysis/compute-mean-and-var.R")

# Calculate catch24 features and compute classification performance

source("analysis/compute-catch24.R")
source("analysis/fit-catch24-classifiers.R")

# Analyse case studies where mean + variance does very well

source("analysis/mean-and-var-case-studies.R")

# Compare classification performance between mean + variance and catch24

source("analysis/compare-catch2-catch24.R")

#---------------- Annie's work here ------------


