###################################################
# Prepare for analysis: Airbnb listings in Austin #
###################################################

 # clear global environment
rm(list=ls())

# Import libraries
library(tidyverse)
library(modelsummary)


# set working directory
setwd("C:/Users/diama/Documents/CEU-BA-Assignments/DataAnalysis3/Assignment1_Austin")

# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, data used
source("set-data-directory.R")             # data_dir must be first defined 
# alternative: give full path here, 
#            example data_dir="C:/Users/bekes.gabor/Dropbox (MTA KRTK)/bekes_kezdi_textbook/da_data_repo"

# load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

use_case_dir <- "ch16-airbnb-random-forest/"
data_in <- paste(data_dir,"airbnb","clean/", sep = "/")
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)