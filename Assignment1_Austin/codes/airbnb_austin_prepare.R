###################################################
# Prepare for analysis: Airbnb listings in Austin #
###################################################

 # clear global environment
rm(list=ls())

# Import libraries
library(tidyverse)
library(modelsummary)
library(geosphere)
library(ggplot2)

# set working environment
dir <- "C:/Users/diama/Documents/CEU-BA-Assignments/CEU-Data-Analysis-3-Assignments/Assignment1_Austin"
setwd("c:/Users/diama/Documents/CEU-Business-Analytics-2020/Data_Analysis3/class_material/da_case_studies/")

# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
source("set-data-directory.R") #data_dir must be first defined #
data_dir <- "c:/Users/diama/Documents/CEU-Business-Analytics-2020/Data_Analysis3/class_material/da_data_repo/"
data_in <- paste(data_dir,"airbnb","clean/", sep = "/")

use_case_dir <- "ch14-airbnb-reg/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)

options(digits = 3)


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


########
# check potential variables for setting theme as dummies and how to classfy theme

# check price based on distance:
ggplot(df, aes(x=distance, y=price)) +
  geom_point() +
  geom_smooth(method = "loess")


# price misiing value dropped
# there is one case where the price equals to 1, it is dropped:
df <- df %>% drop_na(price)
df <- df %>%  filter(price > 1)

# create price_per_night column:
df <- df %>%  mutate(price_per_night = price/minimum_nights)

# remove extreme values where price_per_night is over 200
df <- df %>% filter(price_per_night < 100)

# check again price_per_night based on distance:
# check price based on distance:
ggplot(df, aes(x=distance, y=price_per_night)) +
  geom_point() +
  geom_smooth(method = "loess")




# check price for availability:
ggplot(df, aes(x=availability_365, y=price_per_night)) +
  geom_point() +
  geom_smooth(method = "loess")

# remove where availability is lower than 1 day and more than 365
df <- df %>% filter(availability_365 > 0)
df <- df %>% filter(availability_365 < 366)




# check prices per night for number of reviews
ggplot(df, aes(x=number_of_reviews, y=price_per_night)) +
  geom_point() +
  geom_smooth(method = "loess")


# check for neighbourhood:
ggplot(df, aes(x=neighbourhood, y=price_per_night)) +
  geom_point() +
  geom_smooth(method = "loess")


#########################
# check frequency and basic descriptives for distance:
b1 <- ggplot(df, aes(x=distance)) +
  geom_histogram(color = "mintcream", fill = "cornflowerblue") +
  xlim(c(0,25)) +
  labs(x = "Distance from the city centre",y = "Count") +
  theme_bw()
b1

summary(df$distance)


# check frequency and basic descriptives for availability:
b2 <- ggplot(df, aes(x=availability_365)) +
  geom_histogram(color = "mintcream", fill = "cornflowerblue") +
  xlim(c(0,365)) +
  ylim(c(0,200)) +
  labs(x = "Number of days available in one year",y = "Count") +
  theme_bw()
b2

summary(df$availability_365)

# check frequency and basic descriptives for number of reviews
b3 <- ggplot(df, aes(x=number_of_reviews)) +
  geom_histogram(color = "mintcream", fill = "cornflowerblue") +
  labs(x = "Number of reviews",y = "Count") +
  xlim(c(0,400)) +
  ylim(c(0,500)) +
  theme_bw()
b3
summary(df$number_of_reviews)

# minimum nights:
ggplot(df, aes(x=minimum_nights)) +
  geom_histogram(color = "mintcream", fill = "cornflowerblue")  +
  labs(x = "Number of minimum nights to rent",y = "Count") +
  xlim(c(1,365)) +
  ylim(c(0,120)) +
  theme_bw()

summary(df$minimum_nights)

# dummy variables: binary variables, 1-yes, 0-no
# availability at least 30 day per year, more than 100 reviews, distance more than 15 km from the city centre
df <- df %>%  mutate(available_morethan_200 = ifelse(availability_365 > 200 , 1, 0),
                     reviews_morethan_100 = ifelse(number_of_reviews > 100, 1, 0),
                     distance_morethan_15 = ifelse(distance < 15, 1, 0),
                     good_neighbourhood = ifelse(neighbourhood > 78740, 1, 0))



# check ln prices:
df <- df %>%
  mutate(ln_price = log(price_per_night))


# scatterplots with regression lines
lnprice <- ggplot(df, aes(x = distance, y = ln_price)) +
  geom_point() +
  geom_smooth(method = "loess") +
  ylab("distance") +
  xlab("Log price")
lnprice


price <- ggplot(df, aes(x = distance, y = price_per_night)) +
  geom_point() +
  geom_smooth(method = "loess") +
  ylab("distance") +
  xlab("Price")
price


# save cleaned df to csv file:
write_csv(df, paste0(dir, '/data/clean/austin_cleaned.csv'))


