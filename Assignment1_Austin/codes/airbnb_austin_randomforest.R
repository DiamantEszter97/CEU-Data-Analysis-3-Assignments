# clear global environment
rm(list=ls())

# load libraries
library(rattle)
library(tidyverse)
library(caret)
library(ranger)
library(Hmisc)
library(knitr)
library(kableExtra)
library(xtable)

# load dataframe:
df <- read_csv("https://raw.githubusercontent.com/DiamantEszter97/CEU-Data-Analysis-3-Assignments/main/Assignment1_Austin/data/clean/austin_cleaned.csv")

# check for basic descriptives:
skimr::skim(df)
summary(df$price)
Hmisc::describe(df$price)
describe(df$room_type)
table(data$number_of_reviews)

#