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
library(skimr)
library(grid)
library(glmnet)
library(stargazer)
library(directlabels)
library(cowplot)
library(dplyr)
library(ggplot2)




# load dataframe:
austin_df <- read_csv("https://raw.githubusercontent.com/DiamantEszter97/CEU-Data-Analysis-3-Assignments/main/Assignment1_Austin/data/clean/austin_cleaned.csv")

# save austin_df into df to have a quicker way to fix problems:
df <- austin_df


# check for basic descriptives:
skimr::skim(df)
summary(df$price)
Hmisc::describe(df$price)
describe(df$room_type)
table(data$number_of_reviews)


# create train and holdout samples -------------------------------------------

set.seed(1050)

# First pick a smaller than usual training set so that models run faster and check if works
# If works, start anew without these two lines

# try <- createDataPartition(data$price, p = 0.2, list = FALSE)
#data <- data[try, ]



train_indices <- as.integer(createDataPartition(data$price, p = 0.7, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

dim(data_train)
dim(data_holdout)


# Basic Variables inc neighnourhood
basic_vars <- c(
  "n_accommodates", "n_beds", "n_days_since",
  "f_property_type","f_room_type", "f_bathroom", "f_cancellation_policy", "f_bed_type",
  "f_neighbourhood_cleansed")

# reviews
reviews <- c("n_number_of_reviews", "flag_n_number_of_reviews" ,"n_review_scores_rating", "flag_review_scores_rating")

# Dummy variables
amenities <-  grep("^d_.*", names(data), value = TRUE)

#interactions for the LASSO
# from ch14
X1  <- c("n_accommodates*f_property_type",  "f_room_type*f_property_type",  "f_room_type*d_familykidfriendly",
         "d_airconditioning*f_property_type", "d_cats*f_property_type", "d_dogs*f_property_type")
# with boroughs
X2  <- c("f_property_type*f_neighbourhood_cleansed", "f_room_type*f_neighbourhood_cleansed",
         "n_accommodates*f_neighbourhood_cleansed" )



