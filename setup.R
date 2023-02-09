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
library(theft)
library(correctR)

# Create important folders if none exist

if(!dir.exists('analysis')) dir.create('analysis')
if(!dir.exists('output')) dir.create('output')
if(!dir.exists('R')) dir.create('R')

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

# Define reusable colour palette

mypal <- c("#FF0029", "#377EB8", "#66A61E", "#984EA3", "#00D2D5", 
                    "#FF7F00", "#AF8D00", "#7F80CD", "#B3E900")
                    