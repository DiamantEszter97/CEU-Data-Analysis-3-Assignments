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
library(viridis)

# set working environment
setwd("c:/Users/diama/Documents/CEU-Business-Analytics-2020/Data_Analysis3/class_material/da_case_studies/")

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
source("set-data-directory.R") #data_dir must be first defined #

# load data:
first_df <- read_csv("https://raw.githubusercontent.com/DiamantEszter97/CEU-Data-Analysis-3-Assignments/main/Assignment3_PoolSales/data/clean/swim_work.csv")

# save dataframe into another to avoid long data loading:
df <- first_df

# set date column to date type:
df <- df %>% mutate(date = as.Date(date))

# dow: 1=Monday, weekend: Sat and Sun.
df <- df %>%
  mutate(year = year(date),
         quarter = quarter(date),
         month = factor(month(date)),
         day = day(date)) %>%
  mutate(dow = factor(lubridate::wday(date, week_start = getOption("lubridate.week.start", 1)))) %>%
  mutate(weekend = factor(as.integer(dow %in% c(6,7))))

# find out whether that time of the year is summer holiday or not
df <- df %>% 
  mutate(school_off = ((day>15 & month==5 & day <=30) | (month==6 |  month==7) |
                         (day<15 & month==8) | (day>20 & month==12) ))

df <- df %>% 
  mutate(trend = c(1:dim(df)[1]))

summary(df$QUANTITY)


# get holiday calendar
holidays <-  as.Date(holidayNYSE(2010:2017))

df <- df %>% 
  mutate(isHoliday = ifelse(date %in% holidays,1,0))

Hmisc::describe(df)


# Define vars for analysis ----------------------------------

df <- 
  df %>% 
  group_by(month) %>% 
  mutate(q_month = mean(QUANTITY)) %>% 
  ungroup()

df <- df %>% 
  mutate(QUANTITY2 = ifelse(QUANTITY<1, 1, QUANTITY)) %>% 
  mutate(q_ln = log(QUANTITY2))

skimr::skim(df)

df <- 
  df %>% 
  group_by(month, dow) %>% 
  mutate(tickets = mean(QUANTITY),
         tickets_ln = mean(q_ln)) %>% 
  ungroup()

# named date vars for graphs
mydays <- c("Mon","Tue","Wed",
            "Thu","Fri","Sat",
            "Sun")
df$dow_abb   <-factor(   mydays[df$dow],  levels=mydays)
df$month_abb <-factor(month.abb[df$month],levels=month.abb)

################################
# Descriptive graphs ----------
#################################


g1 <-ggplot(data=df[df$year==2015,], aes(x=date, y=QUANTITY)) +
  geom_line(size=0.4, color=color[1]) +
  theme_bg() +
  scale_x_date(breaks = as.Date(c("2015-01-01","2015-04-01","2015-07-01","2015-10-01","2016-01-01")),
               labels = date_format("%d%b%Y"),
               date_minor_breaks = "1 month" ) +
  labs( x = "Date (day)", y="Daily ticket sales" ) +
  scale_color_discrete(name = "")
g1


g2<-ggplot(data=df[(df$year>=2012) & (df$year<=2014),], aes(x=date, y=QUANTITY)) +
  geom_line(size=0.2, color=color[1]) +
  theme_bg() +
  scale_x_date(breaks = as.Date(c("2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01","2015-01-01")),
               labels = date_format("%d%b%Y"),
               minor_breaks = "3 months") +
  labs( x = "Date (day)", y="Daily ticket sales" ) +
  scale_color_discrete(name = "")
g2



g3<-ggplot(data=df, aes(x=month_abb, y=QUANTITY)) +
  theme_bg() +
  labs( x = "Date (month)", y="Daily ticket sales" ) +
  geom_boxplot(color=color[1],outlier.color = color[4], outlier.alpha = 0.6, outlier.size = 0.4)
g3


g4<-ggplot(data=df, aes(x=dow_abb, y=QUANTITY)) +
  theme_bg() +
  labs( x = "Day of the week", y="Daily ticket sales" ) +
  geom_boxplot(color=color[1],outlier.color = color[4], outlier.alpha = 0.6, outlier.size = 0.4)
#geom_boxplot(color=color[1], outlier.shape = NA)
g4


# to check for interactions, look at the heatmap
swim_heatmap <- 
  ggplot(df, aes(x = dow_abb, y = month_abb, fill = tickets)) +
  geom_tile(colour = "white") +
  labs(x = 'Day of the week', y = 'Month ') +
  scale_fill_viridis(alpha = 0.7, begin = 1, end = 0.2, direction = 1, option = "D") +
  theme_bg() +
  theme(legend.position = "right",
        legend.text = element_text(size=6),
        legend.title =element_text(size=6)
  )
swim_heatmap

#####################################
# PREDICTION  ----------
#####################################


#############################
# Create train/houldout data
#############################

# Last year of data
data_holdout<- df %>%
  filter(year==2016)

# Rest of data for training
data_train <- df %>%
  filter(year<2016)

# Prepare for cross-validation
data_train <- data_train %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.integer(rowname))

test_index_list <- data_train %>% 
  split(f = factor(data_train$year)) %>% 
  lapply(FUN = function(x){x$rowname})

train_index_list <- test_index_list %>% 
  lapply(FUN = function(x){setdiff(data_train$rowname, x)})

train_control <- trainControl(
  method = "cv",
  index = train_index_list, #index of train data for each fold
  # indexOut = index of test data for each fold, complement of index by default
  # indexFinal = index of data to use to train final model, whole train data by default
  savePredictions = TRUE
)

# Fit models ---------------------------------------------------------


#Model 1 linear trend + monthly seasonality + days of week seasonality 
model1 <- as.formula(QUANTITY ~ 1 + trend + month + dow)
reg1 <- train(
  model1,
  method = "lm",
  data = data_train,
  trControl = train_control
)



#Model 2 linear trend + monthly seasonality + days of week  seasonality + holidays + interactions
model2 <- as.formula(QUANTITY ~ 1 + trend + month + dow + isHoliday + school_off*dow + weekend*month)
reg2 <- train(
  model2,
  method = "lm",
  data = data_train,
  trControl = train_control
)

#Model 3 =  multiplicative trend and seasonality (ie take logs, predict log values and transform back with correction term)
model3 <- as.formula(q_ln ~ 1 + trend + month + dow + isHoliday + school_off*dow)
reg3 <- train(
  model3,
  method = "lm",
  data = data_train,
  trControl = train_control
)



# Get CV RMSE ----------------------------------------------

model_names <- c("reg1","reg2")
rmse_CV <- c()

for (i in model_names) {
  rmse_CV[i]  <- get(i)$results$RMSE
}
rmse_CV

#had to cheat and use train error on full train set because could not obtain CV fold train errors
corrb <- mean((reg3$finalModel$residuals)^2)
rmse_CV["reg3"] <- reg3$pred %>% 
  mutate(pred = exp(pred  + corrb/2)) %>% 
  group_by(Resample) %>% 
  summarise(rmse = RMSE(pred, exp(obs))) %>% 
  as.data.frame() %>% 
  summarise(mean(rmse)) %>% 
  as.numeric()
rmse_CV


###########################x
# Evaluate best model on holdout set --------------------------------------------
###########################x

data_holdout <- data_holdout %>% 
  mutate(y_hat_5 = predict(reg2, newdata = .))

rmse_holdout_best <- RMSE(data_holdout$QUANTITY, data_holdout$y_hat_5)
rmse_holdout_best

###########################x
# Plot best predictions --------------------------------------------
###########################x

#graph relative RMSE (on holdout) per month 
rmse_monthly <- data_holdout %>% 
  mutate(month = factor(format(date,"%b"), 
                        levels= unique(format(sort(.$date),"%b")), 
                        ordered=TRUE)) %>% 
  group_by(month) %>% 
  summarise(
    RMSE = RMSE(QUANTITY, y_hat_5),
    RMSE_norm= RMSE(QUANTITY, y_hat_5)/mean(QUANTITY)
  ) 

g_predictions_rmse<- ggplot(rmse_monthly, aes(x = month, y = RMSE_norm)) +
  geom_col(bg=color[1], color=color[1]) +
  labs( x = "Date (month)", y="RMSE (normalized by monthly sales)" ) +
  theme_bg() 
g_predictions_rmse


g_predictions<-
  ggplot(data=data_holdout, aes(x=date, y=QUANTITY)) +
  geom_line(aes(size="Actual", colour="Actual", linetype = "Actual") ) +
  geom_line(aes(y=y_hat_5, size="Predicted" ,colour="Predicted",  linetype= "Predicted")) +
  scale_y_continuous(expand = c(0,0))+
  scale_x_date(expand=c(0,0), breaks = as.Date(c("2016-01-01","2016-03-01","2016-05-01","2016-07-01","2016-09-01","2016-11-01", "2017-01-01")),
               labels = date_format("%d%b%Y"),
               date_minor_breaks = "1 month" )+
  scale_color_manual(values=color[1:2], name="")+
  scale_size_manual(name="", values=c(0.4,0.7))+
  #scale_linetype_manual(name = "", values=c("solid", "solid")) +
  scale_linetype_manual(name = "", values=c("solid", "twodash")) +
  labs( x = "Date (day)", y="Daily ticket sales" ) +
  theme_bg() +
  #theme(legend.position = "none") +
  #annotate("text", x = as.Date("2016-07-15"), y = 50, label = "Predicted", color=color[2], size=3)+
  #annotate("text", x = as.Date("2016-09-01"), y = 125, label = "Actual", color=color[1], size=3)
  theme(legend.position=c(0.7,0.8),
        legend.direction = "horizontal",
        legend.text = element_text(size = 6),
        legend.key.width = unit(.8, "cm"),
        legend.key.height = unit(.3, "cm")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.8))
  )
g_predictions



g_predictions_m <- ggplot(data=data_holdout, aes(x=date, y=QUANTITY)) +
  geom_line(aes(size="Actual", colour="Actual", linetype = "Actual") ) +
  geom_line(aes(y=y_hat_5, size="Predicted" ,colour="Predicted",  linetype= "Predicted")) +
  geom_ribbon(aes(ymin=QUANTITY,ymax=y_hat_5), fill=color[4], alpha=0.3) +
  scale_x_date(expand=c(0.01,0.01), breaks = as.Date(c("2016-1-01","2016-04-01","2016-07-01","2016-10-01","2016-12-31")),
               limits = as.Date(c("2016-01-01","2016-12-31")),
               labels = date_format("%d%b")) +
  scale_color_manual(values=color[1:2], name="")+
  scale_size_manual(name="", values=c(0.4,0.7))+
  #scale_linetype_manual(name = "", values=c("solid", "solid")) +
  scale_linetype_manual(name = "", values=c("solid", "twodash")) +
  labs( x = "Date (day)", y="Daily ticket sales" ) +
  theme_bg() +
  #theme(legend.position = "none") +
  #annotate("text", x = as.Date("2016-08-04"), y = 55, label = "Actual", color=color[2], size=2)+
  #annotate("text", x = as.Date("2016-08-17"), y = 115, label = "Predicted", color=color[1], size=2)
  theme(legend.position=c(0.7,0.8),
        legend.direction = "horizontal",
        legend.text = element_text(size = 4),
        legend.key.width = unit(.8, "cm"),
        legend.key.height = unit(.2, "cm")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.6))
  )
g_predictions_m



g_predictions_m2 <- ggplot(data=data_holdout %>% filter(month==8), aes(x=date, y=QUANTITY)) +
  geom_line(aes(size="Actual", colour="Actual", linetype = "Actual") ) +
  geom_line(aes(y=y_hat_5, size="Predicted" ,colour="Predicted",  linetype= "Predicted")) +
  geom_ribbon(aes(ymin=QUANTITY,ymax=y_hat_5), fill=color[4], alpha=0.3) +
  scale_x_date(expand=c(0.01,0.01), breaks = as.Date(c("2016-08-01","2016-08-08","2016-08-15","2016-08-22","2016-08-29")),
               limits = as.Date(c("2016-08-01","2016-08-31")),
               labels = date_format("%d%b")) +
  scale_color_manual(values=color[1:2], name="")+
  scale_size_manual(name="", values=c(0.4,0.7))+
  #scale_linetype_manual(name = "", values=c("solid", "solid")) +
  scale_linetype_manual(name = "", values=c("solid", "twodash")) +
  labs( x = "Date (day)", y="Daily ticket sales" ) +
  theme_bg() +
  #theme(legend.position = "none") +
  #annotate("text", x = as.Date("2016-08-04"), y = 55, label = "Actual", color=color[2], size=2)+
  #annotate("text", x = as.Date("2016-08-17"), y = 115, label = "Predicted", color=color[1], size=2)
  theme(legend.position=c(0.7,0.8),
        legend.direction = "horizontal",
        legend.text = element_text(size = 4),
        legend.key.width = unit(.8, "cm"),
        legend.key.height = unit(.2, "cm")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.6))
  )
g_predictions_m2
