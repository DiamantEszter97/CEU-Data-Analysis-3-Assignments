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



# load dataframe:
austin_df <- read_csv("https://raw.githubusercontent.com/DiamantEszter97/CEU-Data-Analysis-3-Assignments/main/Assignment1_Austin/data/clean/austin_cleaned.csv")

# save austin_df into df to have a quicker way to fix problems:
df <- austin_df

# set price_per_night to price:
df <- df %>% select(-price)
df <- df %>% mutate(price=price_per_night)

# check for basic descriptives:
skimr::skim(df)
summary(df$price)
Hmisc::describe(df$price)
describe(df$room_type)


# Distribution of price per nights

# Histograms
# price
g3a <- ggplot(data=df, aes(x=price)) +
  geom_histogram_da(type="percent", binwidth = 10) +
  #geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 10, boundary=0,
  #               color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F,  na.rm=TRUE) +
  #  coord_cartesian(xlim = c(0, 400)) +
  labs(x = "Price (US dollars)",y = "Percent")+
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.03), labels = scales::percent_format(1)) +
  scale_x_continuous(expand = c(0.00,0.00),limits=c(10,100), breaks = seq(0,400, 50)) +
  theme_bg() 
g3a


g3b<- ggplot(data=df, aes(x=ln_price)) +
  geom_histogram_da(type="percent", binwidth = 0.2) +
  #  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.18,
  #               color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F,  na.rm=TRUE) +
  coord_cartesian(xlim = c(2.5, 4.8)) +
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.05), labels = scales::percent_format(5L)) +
  scale_x_continuous(expand = c(0.00,0.01),breaks = seq(2.4,4.8, 0.6)) +
  labs(x = "ln(price, US dollars)",y = "Percent")+
  theme_bg() 
g3b

## Boxplot of price by room type
g4 <- ggplot(data = df, aes(x = room_type, y = price)) +
  stat_boxplot(aes(group = room_type), geom = "errorbar", width = 0.3,
               color = c(color[2],color[1], color[3]), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = room_type),
               color = c(color[2],color[1], color[3]), fill = c(color[2],color[1], color[3]),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,100), breaks = seq(0,300,100)) +
  labs(x = "Room type",y = "Price (US dollars)")+
  theme_bg()
g4

#####################
# Setting up models #
#####################

# Basic Variables
basic_lev  <- c("distance", "neighbourhood", "availability_365")

# reviews
reviews <- c("number_of_reviews","reviews_per_month")

# binaries
binaries <- c("available_morethan_200", 'reviews_morethan_100', "distance_morethan_15", "good_neighbourhood")


#################################################
# Look for interactions
################################################

#Look up room type interactions
p1 <- price_diff_by_variables2(df, "distance", "distance_morethan_15", "number_of_reviews", "reviews_morethan_100")
p2 <- price_diff_by_variables2(df, "availability_365", "available_morethan_200", "distance", "distance_morethan_15")

g_interactions <- plot_grid(p1, p2, nrow=1, ncol=2)
g_interactions


# Create models in levels models: 1-8
modellev1 <- " ~ distance"
modellev2 <- paste0(" ~ ",paste(basic_lev,collapse = " + "))
modellev3 <- paste0(" ~ ",paste(c(basic_lev,reviews),collapse = " + "))
modellev4 <- paste0(" ~ ",paste(c(basic_lev,reviews,binaries),collapse = " + "))

#################################
# Separate hold-out set #
#################################

# create a holdout set (20% of observations)
smp_size <- floor(0.2 * nrow(df))

# Set the random number generator: It will make results reproducable
set.seed(1050)

# create ids:
# 1) seq_len: generate regular sequences
# 2) sample: select random rows from a table
holdout_ids <- sample(seq_len(nrow(df)), size = smp_size)
df$holdout <- 0
df$holdout[holdout_ids] <- 1


#Hold-out set Set
data_holdout <- df %>% filter(holdout == 1)

#Working data set
data_work <- df %>% filter(holdout == 0)


##############################
#      cross validation      #
##############################

## N = 5
n_folds=5
# Create the folds

folds_i <- sample(rep(1:n_folds, length.out = nrow(data_work) ))
# Create results
model_results_cv <- list()


for (i in (1:4)){
  model_name <-  paste0("modellev",i)
  model_pretty_name <- paste0("(",i,")")
  
  yvar <- "price"
  xvars <- eval(parse(text = model_name))
  formula <- formula(paste0(yvar,xvars))
  
  # Initialize values
  rmse_train <- c()
  rmse_test <- c()
  
  model_work_data <- lm(formula,data = data_work)
  BIC <- BIC(model_work_data)
  nvars <- model_work_data$rank -1
  r2 <- summary(model_work_data)$r.squared
  
  # Do the k-fold estimation
  for (k in 1:n_folds) {
    test_i <- which(folds_i == k)
    # Train sample: all except test_i
    data_train <- data_work[-test_i, ]
    # Test sample
    data_test <- data_work[test_i, ]
    # Estimation and prediction
    model <- lm(formula,data = data_train)
    prediction_train <- predict(model, newdata = data_train)
    prediction_test <- predict(model, newdata = data_test)
    
    # Criteria evaluation
    rmse_train[k] <- mse_lev(prediction_train, data_train[,yvar] %>% pull)**(1/2)
    rmse_test[k] <- mse_lev(prediction_test, data_test[,yvar] %>% pull)**(1/2)
    
  }
  
  model_results_cv[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model_work_data=model_work_data,
                                         rmse_train = rmse_train,rmse_test = rmse_test,BIC = BIC,
                                         model_name = model_pretty_name, nvars = nvars, r2 = r2)
}




model <- lm(formula,data = data_train)
prediction_train <- predict(model, newdata = data_train)
prediction_test <- predict(model, newdata = data_test)


t1 <- imap(model_results_cv,  ~{
  as.data.frame(.x[c("rmse_test", "rmse_train")]) %>%
    dplyr::summarise_all(.funs = mean) %>%
    mutate("model_name" = .y , "model_pretty_name" = .x[["model_name"]] ,
           "nvars" = .x[["nvars"]], "r2" = .x[["r2"]], "BIC" = .x[["BIC"]])
}) %>%
  bind_rows()
t1
column_names <- c("Model", "N predictors", "R-squared", "BIC", "Training RMSE",
                  "Test RMSE")


t14_2 <- t1 %>%
  select("model_pretty_name", "nvars", "r2" , "BIC", "rmse_train", "rmse_test")
colnames(t14_2) <- column_names
print(xtable(t14_2, type = "latex", digits=c(0,0,0,2,0,2,2)), file = paste0(dir, "/output/modellev.tex"),
      include.rownames=FALSE, booktabs=TRUE, floating = FALSE)



# RMSE training vs test graph
t1_levels <- t1 %>%
  dplyr::select("nvars", "rmse_train", "rmse_test") %>%
  gather(var,value, rmse_train:rmse_test) %>%
  mutate(nvars2=nvars+1) %>%
  mutate(var = factor(var, levels = c("rmse_train", "rmse_test"),
                      labels = c("RMSE Training","RMSE Test")))

t1_levels

model_result_plot_levels <- ggplot(data = t1_levels,
                                   aes(x = factor(nvars2), y = value, color=factor(var), group = var)) +
  geom_line(size=1,show.legend=FALSE, na.rm = TRUE) +
  scale_color_manual(name="",
                     values=c(color[2],color[1])) +
  scale_y_continuous(name = "RMSE", limits = c(34, 44), breaks = seq(34,44, 2)) +
  scale_x_discrete( name = "Number of coefficients", expand=c(0.01, 0.01)) +
  geom_dl(aes(label = var),  method = list("last.points", dl.trans(x=x-1), cex=0.4))
  #scale_colour_discrete(guide = 'none') 
model_result_plot_levels



###################################################
# Diagnsotics #
###################################################
model3_level <- model_results_cv[["modellev3"]][["model_work_data"]]
model4_level <- model_results_cv[["modellev4"]][["model_work_data"]]


# look at holdout RMSE
model4_level_work_rmse <- mse_lev(predict(model4_level, newdata = data_work), data_work[,"price"] %>% pull)**(1/2)
model4_level_holdout_rmse <- mse_lev(predict(model4_level, newdata = data_holdout), data_holdout[,"price"] %>% pull)**(1/2)
model4_level_holdout_rmse

# probably in training, it is overfitted

###################################################
# FIGURES FOR FITTED VS ACTUAL OUTCOME VARIABLES #
###################################################

# Target variable
Ylev <- data_holdout[["price"]]

meanY <-mean(Ylev)
sdY <- sd(Ylev)
meanY_m2SE <- meanY -1.96 * sdY
meanY_p2SE <- meanY + 1.96 * sdY
Y5p <- quantile(Ylev, 0.05, na.rm=TRUE)
Y95p <- quantile(Ylev, 0.95, na.rm=TRUE)

# Predicted values
predictionlev_holdout_pred <- as.data.frame(predict(model34_level, newdata = data_holdout, interval="predict")) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model4_level, newdata = data_holdout, interval="confidence")) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("price","distance")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])


# Create data frame with the real and predicted values
d <- data.frame(ylev=Ylev, predlev=predictionlev_holdout[,"fit"] )
# Check the differences
d$elev <- d$ylev - d$predlev

# Plot predicted vs price
level_vs_pred <- ggplot(data = d) +
  geom_point(aes(y=ylev, x=predlev), color = color[1], size = 1,
             shape = 16, alpha = 0.7, show.legend=FALSE, na.rm=TRUE) +
  #geom_smooth(aes(y=ylev, x=predlev), method="lm", color=color[2], se=F, size=0.8, na.rm=T)+
  geom_segment(aes(x = 0, y = 0, xend = 350, yend =350), size=0.5, color=color[2], linetype=2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 350), breaks=seq(0, 350, by=50)) +
  scale_y_continuous(expand = c(0.01,0.01),limits=c(0, 350), breaks=seq(0, 350, by=50)) +
  labs(y = "Price (US dollars)", x = "Predicted price  (US dollars)") +
  theme_bg() 
level_vs_pred

###################
## Random forest ##
###################
# create train and holdout samples -------------------------------------------

set.seed(1050)

train_indices <- as.integer(createDataPartition(df$price_per_night, p = 0.7, list = FALSE))
data_train <- df[train_indices, ]
data_holdout <- df[-train_indices, ]

dim(data_train)
dim(data_holdout)


# Basic Variables regarding distance and neighbourhood
basic_vars <- c(  "distance", "neighbourhood")

# reviews
reviews <- c("number_of_reviews", "reviews_per_month")


# binaries
binaries <- c("available_morethan_200", 'reviews_morethan_100', "distance_morethan_15", "good_neighbourhood")

predictors_1 <- c(basic_vars)
predictors_2 <- c(basic_vars, reviews, binaries)

# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)


# set tuning
tune_grid <- expand.grid(
  .mtry = c(1, 3),
  .splitrule = "variance",
  .min.node.size = c(5, 10)
)


# simpler model for model A (1)
set.seed(1050)
system.time({
  rf_model_1 <- train(
    formula(paste0("price ~", paste0(predictors_1, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})
rf_model_1

# set tuning for benchamrk model (2)
tune_grid <- expand.grid(
  .mtry = c(2, 4),
  .splitrule = "variance",
  .min.node.size = c(5, 10)
)

set.seed(1050)
system.time({
  rf_model_2 <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

rf_model_2


results <- resamples(
  list(
    model_1  = rf_model_1,
    model_2  = rf_model_2
    
  )
)
summary(results)


# Save outputs -------------------------------------------------------

# Show Model B rmse shown with all the combinations
rf_tuning_modelB <- rf_model_2$results %>%
  dplyr::select(mtry, min.node.size, RMSE) %>%
  dplyr::rename(nodes = min.node.size) %>%
  spread(key = mtry, value = RMSE)

kable(x = rf_tuning_modelB, format = "latex", digits = 2, caption = "CV RMSE") %>%
  add_header_above(c(" ", "vars" = 3)) %>%
  cat(.,file= paste0(dir, "/output/foresttest.tex"))

      
# Turning parameter choice 1
result_1 <- matrix(c(
  rf_model_1$finalModel$mtry,
  rf_model_2$finalModel$mtry,
  rf_model_2auto$finalModel$mtry,
  rf_model_1$finalModel$min.node.size,
  rf_model_2$finalModel$min.node.size,
  rf_model_2auto$finalModel$min.node.size
  
),
nrow=3, ncol=2,
dimnames = list(c("Model A", "Model B","Model B auto"),
                c("Min vars","Min nodes"))
)
kable(x = result_1, format = "latex", digits = 3) %>%
  cat(.,file= paste0(dir, "/output/foresttest.tex"))

# Turning parameter choice 2
result_2 <- matrix(c(mean(results$values$`model_1~RMSE`),
                     mean(results$values$`model_2~RMSE`),
                     mean(results$values$`model_2b~RMSE`)
),
nrow=3, ncol=1,
dimnames = list(c("Model A", "Model B","Model B auto"),
                c(results$metrics[2]))
)


kable(x = result_2, format = "latex", digits = 3) %>%
  cat(.,file= paste0(dir, "/output/foresttest.tex"))




