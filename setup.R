#-----------------------------------------
# This script sets out to load all the 
# packages and folders necessary for the 
# project
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 9 February 2023
#-----------------------------------------

library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggrepel)
library(scales)
library(foreign)
library(patchwork)
library(theft)
library(correctR)
library(RColorBrewer)
library(e1071)
library(caret)
library(glue)
library(cowplot)
library(ggseg)
library(broom)
library(colorspace)
library(ggpubr)

# Create important folders if none exist

if(!dir.exists('data')) dir.create('data')
if(!dir.exists('data/feature-calcs')) dir.create('data/feature-calcs')
if(!dir.exists('analysis')) dir.create('analysis')
if(!dir.exists('output')) dir.create('output')
if(!dir.exists('R')) dir.create('R')
if(!dir.exists('info')) dir.create('info')
if(!dir.exists('fMRI_analysis_data')) dir.create('fMRI_analysis_data')

# Re-usable "not in" operator

'%ni%' <- Negate('%in%')

# Load re-usable functions

r_files <- list.files("R", full.names = TRUE, pattern = "\\.[Rr]")

for(f in r_files){
  source(f)
}

rm(r_files)

# Suppress dplyr::summarise info

options(dplyr.summarise.inform = FALSE)
                    