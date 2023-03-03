#-----------------------------------------
# This script can be used to run the whole
# project in order from start to finish.
# It requires 
# http://www.timeseriesclassification.com/Downloads/Archives/Univariate2018_arff.zip
# to be downloaded, unzipped, and put into 
# the data/ folder first.
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 9 February 2023
#-----------------------------------------

source("setup.R")

#---------------- UEA & UCR Repository analysis ------------

# Prepare time-series datasets

source("analysis/prepare-time-series-data.R")

# Summarise problems

source("analysis/summarise-problems.R")

# Calculate mean and variance features and classification performance

source("analysis/compute-mean-and-var.R")
source("analysis/fit-ftm-classifiers.R")
source("analysis/analyse-ftm-performance.R")

# Calculate catch24 features and compute classification performance

source("analysis/compute-catch24.R")
source("analysis/fit-catch24-classifiers.R")

# Analyse case studies where mean + variance does very well

source("analysis/mean-and-var-case-studies.R")

# Compare classification performance between mean + variance and catch24

source("analysis/compare-catch2-catch24.R")

#---------------- Annie's work here ------------


