###################################################
# Prepare for analysis: Airbnb listings in Austin #
###################################################

 # clear global environment
rm(list=ls())

# Import libraries
library(tidyverse)
library(modelsummary)
library(geosphere)


# set working directory
dir <- "C:/Users/diama/Documents/CEU-BA-Assignments/CEU-Data-Analysis-3-Assignments/Assignment1_Austin"

# load data:
austin <- read_csv("https://raw.githubusercontent.com/DiamantEszter97/CEU-Data-Analysis-3-Assignments/main/Assignment1_Austin/data/raw/austin_listings.csv")

# save austin dataframe to another to make corrections easier:
austin_df <- austin

# add the latitude and longitude of the city center to calculate the distance
austin_df$long <- austin_df$longitude
austin_df$lat <- austin_df$latitude
austin_df$cen_long <- -97.7444
austin_df$cen_lat <- 30.2729

# calculate distance km
austin_df$distance <- (distHaversine(austin_df[,17:18], austin_df[,19:20])/1000)

# select coluns that are required for the further analysis:
df <- austin_df %>%  select(c(neighbourhood, room_type, price, minimum_nights, number_of_reviews,
                              reviews_per_month, calculated_host_listings_count, availability_365, distance))

# remove austin_df because anytime a correction is needed, it is needed to be reload with the original austin dataframe
# rm(austin_df)

# filter only apartments
df <- df %>% filter(room_type == 'Entire home/apt')

# replace 'Entire home/apt' to 'apartments' to make it clearer
df$room_type <- 'apartments'

# create columns for availability at least 1 day per year and more than 100 reviews binary variables
df <- df %>%  mutate(available = ifelse(availability_365 > 0 , 1, 0),
                     reviews_morethan_100 = ifelse(number_of_reviews > 100, 1, 0))


# save cleaned df to csv file:
write_csv(df, paste0(dir, '/data/clean/austin_cleaned.csv'))


