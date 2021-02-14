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

######################
# load dataframe
data_in <- "C:/Users/diama/Documents/CEU-Business-Analytics-2020/Data_Analysis3/"

first_df <- as.data.frame(read.table(paste0(data_in,"SwimmingPoolAdmissionsCABQ-en-us.csv"),
                                sep = "\t",
                                header = TRUE,
                                fileEncoding = "UCS-2LE",
                                strip.white = TRUE))

# save dataframe to another one to avoid long data loading
df <- first_df

# filter for ADMISTIER3
df <- df %>%  filter(Category %in%  c("ADMISTIER1", "ADMISTIER2", "ADMISTIER3" )) %>% 
                        filter(Location %in%  c("AQSP01", "AQEI01", "AQEJ01", "AQMP01", "AQRG01", "AQSV01", "AQWP01") )

# set date_time column for datetime and set workfile
df <- df %>%
  mutate(c1 =  (ITEM %in%  c("ADULT" , "SENIOR" ,"TEEN" ,"CHILD", "TOT"))) %>%
  mutate(c2 =  (ITEM %in%  c("CHILD PM","ADULT PM","SENIOR PM", "TOT PM", "TEEN PN"))) %>%
  filter(c1 | c2) %>%
  mutate(date = as.Date(Date_Time, format = "%Y-%m-%d"))


# Agrregate date to daily freq --------------------------------------

df <- aggregate(QUANTITY ~ date, data = df, sum)

# replace missing days with 0 
df <- df %>% 
  merge(data.frame(date = seq(from = min(df[,"date"]), to = max(df[,"date"]), by = 1)),
        all = TRUE) %>% 
  mutate(QUANTITY = ifelse(is.na(QUANTITY),0,QUANTITY))


# 2010-2016 only full years used. 
df <- df %>%
  filter(date >= as.Date("2010-01-01")) %>%
  filter(date < as.Date("2017-01-01"))
Hmisc::describe(df)


# Save workfile
data_out <- "C:/Users/diama/Documents/CEU-BA-Assignments/CEU-Data-Analysis-3-Assignments/Assignment3_PoolSales/data/clean/"
write.csv(df,paste(data_out,"swim_work.csv",sep=""), row.names = FALSE)             

