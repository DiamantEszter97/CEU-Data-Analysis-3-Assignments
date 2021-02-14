# clear working environment 
rm(list = ls())

# Import libraries ---------------------------------------------------
library(tidyverse)
library(stargazer)
library(Hmisc)
library(timeDate)
library(lubridate)
library(caret)
library(prophet)


# set working environment
setwd("c:/Users/diama/Documents/CEU-Business-Analytics-2020/Data_Analysis3/class_material/da_case_studies/")

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
source("set-data-directory.R") #data_dir must be first defined #

# load data:
