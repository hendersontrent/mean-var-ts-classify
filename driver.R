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

# Calculate catch24 features

source("analysis/compute-catch24.R")

# Compare classification performance

# Placeholder

#---------------- Annie's work here ------------


