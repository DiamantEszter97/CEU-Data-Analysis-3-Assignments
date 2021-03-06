# clear working environment 
rm(list = ls())

# Import libraries
library(haven)
library(glmnet)
library(purrr)
library(margins)
library(skimr)
library(kableExtra)
library(Hmisc)
library(cowplot)
library(gmodels) 
library(lspline)
library(sandwich)
library(modelsummary)

library(rattle)
library(caret)
library(pROC)
library(ranger)
library(rpart)
library(partykit)
library(rpart.plot)
library(randomForest)


# set working environment
setwd("c:/Users/diama/Documents/CEU-Business-Analytics-2020/Data_Analysis3/class_material/da_case_studies/")

# for output:
output <- "C:/Users/diama/Documents/CEU-BA-Assignments/CEU-Data-Analysis-3-Assignments/Assignment2_Growth/output/"

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
source("set-data-directory.R") #data_dir must be first defined #

######################
# load data:
first_df <-read_rds(("C:/Users/diama/Documents/CEU-BA-Assignments/CEU-Data-Analysis-3-Assignments/Assignment2_Growth/data/clean/bisnode_firms_clean.rds"))

# save it to another dataframe to avoid long data loading
df <- first_df

#summary
skim(df)

############################
# simple multiple regression

# Define variable sets ----------------------------------------------
# (making sure we use ind2_cat, which is a factor)

rawvars <-  c("curr_assets", "curr_liab", "extra_exp", "extra_inc", "extra_profit_loss", "fixed_assets",
              "inc_bef_tax", "intang_assets", "inventories", "liq_assets", "material_exp", "personnel_exp",
              "profit_loss_year", "sales", "share_eq", "subscribed_cap")

ass_liab_log <- c("curr_assets_log", "curr_liab_log")

qualityvars <- c("balsheet_flag", "balsheet_length", "balsheet_notfullyear")

engvar <- c("total_assets_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs",
            "share_eq_bs", "subscribed_cap_bs", "intang_assets_bs", "extra_exp_pl",
            "extra_inc_pl", "extra_profit_loss_pl", "inc_bef_tax_pl", "inventories_pl",
            "material_exp_pl", "profit_loss_year_pl", "personnel_exp_pl")

engvar2 <- c("extra_profit_loss_pl_quad", "inc_bef_tax_pl_quad",
             "profit_loss_year_pl_quad", "share_eq_bs_quad")

engvar3 <- c(grep("*flag_low$", names(df), value = TRUE),
             grep("*flag_high$", names(df), value = TRUE),
             grep("*flag_error$", names(df), value = TRUE),
             grep("*flag_zero$", names(df), value = TRUE))

d1 <-  c("d1_sales_mil_log_mod", "d1_sales_mil_log_mod_sq",
         "flag_low_d1_sales_mil_log", "flag_high_d1_sales_mil_log")

hr <- c("female", "ceo_age", "flag_high_ceo_age", "flag_low_ceo_age",
        "flag_miss_ceo_age", "ceo_count", "labor_avg_mod",
        "flag_miss_labor_avg", "foreign_management")

firm <- c("age", "age2", "new", "ind2_cat", "m_region_loc", "urban_m")

reg1 <- lm(formula(paste0("sales_growth ~", paste0(ass_liab_log, collapse = " + "))),
           data = df)

m <- margins(reg1, vce = "none")

reg1_table <- summary(reg1) %>%
  coef() %>%
  as.data.frame() %>%
  select(Estimate, `Std. Error`) %>%
  mutate(factor = row.names(.)) %>%
  merge(summary(m)[,c("factor","AME")])

reg1_table

reg1_glm <- glm(formula(paste0("sales_growth ~", paste0(ass_liab_log, collapse = " + "))),
                data = df)

m <- margins(reg1_glm, vce = "none")

glm_reg1_table <- summary(reg1_glm) %>%
  coef() %>%
  as.data.frame() %>%
  select(Estimate, `Std. Error`) %>%
  mutate(factor = row.names(.)) %>%
  merge(summary(m)[,c("factor","AME")])

glm_reg1_table

#######################
# logit 
#######################


# interactions for logit, LASSO
interactions1 <- c("ind2_cat*age", "ind2_cat*age2",
                   "ind2_cat*d1_sales_mil_log_mod", "ind2_cat*sales_mil_log",
                   "ind2_cat*ceo_age", "ind2_cat*foreign_management",
                   "ind2_cat*female",   "ind2_cat*urban_m", "ind2_cat*labor_avg_mod")
interactions2 <- c("sales_mil_log*age", "sales_mil_log*female",
                   "sales_mil_log*profit_loss_year_pl", "sales_mil_log*foreign_management")




X1 <- c(ass_liab_log, "sales_mil_log", "sales_mil_log_sq", "d1_sales_mil_log_mod", "profit_loss_year_pl", "ind2_cat")
X2 <- c(ass_liab_log, "sales_mil_log", "sales_mil_log_sq", "d1_sales_mil_log_mod", "profit_loss_year_pl", "fixed_assets_bs","share_eq_bs","curr_liab_bs ",   "curr_liab_bs_flag_high ", "curr_liab_bs_flag_error",  "age","foreign_management" , "ind2_cat")
X3 <- c(ass_liab_log, "sales_mil_log", "sales_mil_log_sq", firm, engvar,                   d1)
X4 <- c(ass_liab_log, "sales_mil_log", "sales_mil_log_sq", firm, engvar, engvar2, engvar3, d1, hr, qualityvars)
X5 <- c(ass_liab_log, "sales_mil_log", "sales_mil_log_sq", firm, engvar, engvar2, engvar3, d1, hr, qualityvars, interactions1, interactions2)

# for LASSO
logitvars <- c("sales_mil_log", "sales_mil_log_sq", engvar, engvar2, engvar3, d1, hr, firm, qualityvars, interactions1, interactions2)

# for RF (no interactions, no modified features)
rfvars  <-  c("sales_mil", "d1_sales_mil_log", rawvars, hr, firm, qualityvars)


# Check simplest model X1
ols_modelx1 <- lm(formula(paste0("sales_growth ~", paste0(X1, collapse = " + "))),
                  data = df)


glm_modelx1 <- glm(formula(paste0("sales_growth ~", paste0(X1, collapse = " + "))),
                   data = df, family = "binomial")


m <- margins(ols_modelx1, vce = "none")

ols_table <- summary(ols_modelx1) %>%
  coef() %>%
  as.data.frame() %>%
  select(Estimate, `Std. Error`) %>%
  mutate(factor = row.names(.)) %>%
  merge(summary(m)[,c("factor","AME")])


ols_table

m <- margins(glm_modelx1, vce = "none")

glm_ols_table <- summary(glm_modelx1) %>%
  coef() %>%
  as.data.frame() %>%
  select(Estimate, `Std. Error`) %>%
  mutate(factor = row.names(.)) %>%
  merge(summary(m)[,c("factor","AME")])

glm_ols_table

#############################
# Random Forest
#############################


# create training and holdaout set
# create training and holdaout set
set.seed(30000)
lenght_t <- nrow(df)
index <- rep(NA, lenght_t)
index <- sample(2, lenght_t, replace = TRUE, prob = c(0.7, 0.3))
training <- df[index ==1,]
testing <- df[index ==2,]



predictors_1 <- c(rawvars, firm)
predictors_2 <- c(rawvars, firm, engvar, engvar2, hr)

# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)


# set tuning
tune_grid <- expand.grid(
  .mtry = c(1, 2),
  .splitrule = "variance",
  .min.node.size = c(2, 4, 6)
)


# simpler model for model A (1)
set.seed(30000)
system.time({
  rf_model_1 <- train(
    formula(paste0("sales_growth ~", paste0(predictors_1, collapse = " + "))),
    data = training,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})
rf_model_1


# set tuning for benchamrk model (2)
tune_grid <- expand.grid(
  .mtry = c(1, 2),
  .splitrule = "variance",
  .min.node.size = c(2, 4, 6)
)

set.seed(30000)
system.time({
  rf_model_2 <- train(
    formula(paste0("sales_growth ~", paste0(predictors_2, collapse = " + "))),
    data = training,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

rf_model_2

# put findings into table
results <- resamples(
  list(
    model_1  = rf_model_1,
    model_2  = rf_model_2
    
  )
)

rf <- summary(results)

rf



#######################################################x
# PART I PREDICT PROBABILITIES
# Predict logit models ----------------------------------------------
#######################################################x

# 5 fold cross-validation
train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)


# Train Logit Models ----------------------------------------------

logit_model_vars <- list("X1" = X1, "X2" = X2, "X3" = X3, "X4" = X4, "X5" = X5)

CV_RMSE_folds <- list()
logit_models <- list()

for (model_name in names(logit_model_vars)) {
  
  features <- logit_model_vars[[model_name]]
  
  set.seed(30000)
  glm_model <- train(
    formula(paste0("sales_growth_f ~", paste0(features, collapse = " + "))),
    method = "glm",
    data = training,
    family = binomial,
    trControl = train_control
  )
  
  logit_models[[model_name]] <- glm_model
  # Calculate RMSE on test for each fold
  CV_RMSE_folds[[model_name]] <- glm_model$resample[,c("Resample", "RMSE")]
  
}


CV_RMSE_folds

# Logit lasso -----------------------------------------------------------

lambda <- 10^seq(-1, -4, length = 10)
grid <- expand.grid("alpha" = 1, lambda = lambda)

set.seed(30000)
system.time({
  logit_lasso_model <- train(
    formula(paste0("sales_growth_f ~", paste0(logitvars, collapse = " + "))),
    data = training,
    method = "glmnet",
    preProcess = c("center", "scale"),
    family = "binomial",
    trControl = train_control,
    tuneGrid = grid,
    na.action=na.exclude
  )
})

tuned_logit_lasso_model <- logit_lasso_model$finalModel
best_lambda <- logit_lasso_model$bestTune$lambda
logit_models[["LASSO"]] <- logit_lasso_model
lasso_coeffs <- as.matrix(coef(tuned_logit_lasso_model, best_lambda))

CV_RMSE_folds[["LASSO"]] <- logit_lasso_model$resample[,c("Resample", "RMSE")]

CV_RMSE_folds

#############################################x
# PART I
# No loss fn
########################################

# Draw ROC Curve and calculate AUC for each folds --------------------------------
CV_AUC_folds <- list()

for (model_name in names(logit_models)) {
  
  auc <- list()
  model <- logit_models[[model_name]]
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <-
      model$pred %>%
      filter(Resample == fold)
    
    roc_obj <- roc(cv_fold$obs, cv_fold$high_growth)
    auc[[fold]] <- as.numeric(roc_obj$auc)
  }
  
  CV_AUC_folds[[model_name]] <- data.frame("Resample" = names(auc),
                                           "AUC" = unlist(auc))
}

CV_AUC_folds

# For each model: average RMSE and average AUC for models ----------------------------------

CV_RMSE <- list()
CV_AUC <- list()

for (model_name in names(logit_models)) {
  CV_RMSE[[model_name]] <- mean(CV_RMSE_folds[[model_name]]$RMSE)
  CV_AUC[[model_name]] <- mean(CV_AUC_folds[[model_name]]$AUC)
}

CV_RMSE
CV_AUC

# We have 6 models, (5 logit and the logit lasso). For each we have a 5-CV RMSE and AUC.
# We pick our preferred model based on that. -----------------------------------------------

nvars <- lapply(logit_models, FUN = function(x) length(x$coefnames))
nvars[["LASSO"]] <- sum(lasso_coeffs != 0)

logit_summary1 <- data.frame("Number of predictors" = unlist(nvars),
                             "CV RMSE" = unlist(CV_RMSE),
                             "CV AUC" = unlist(CV_AUC))

logit_summary1


# Take best model and estimate RMSE on holdout  -------------------------------------------


best_logit_no_loss <- logit_models[["X4"]]

logit_predicted_probabilities_holdout <- predict(best_logit_no_loss, newdata = testing, type = "prob")
testing[,"best_logit_no_loss_pred"] <- logit_predicted_probabilities_holdout[,"high_growth"]
RMSE(testing[, "best_logit_no_loss_pred", drop=TRUE], testing$sales_growth)

# discrete ROC (with thresholds in steps) on holdout -------------------------------------------------
thresholds <- seq(0.05, 0.75, by = 0.05)



cm <- list()
true_positive_rates <- c()
false_positive_rates <- c()
for (thr in thresholds) {
  holdout_prediction <- ifelse(testing[,"best_logit_no_loss_pred"] < thr, "no_high_growth", "high_growth") %>%
    factor(levels = c("no_high_growth", "high_growth"))
  cm_thr <- confusionMatrix(holdout_prediction,testing$sales_growth_f)$table
  cm[[as.character(thr)]] <- cm_thr
  true_positive_rates <- c(true_positive_rates, cm_thr["high_growth", "high_growth"] /
                             (cm_thr["high_growth", "high_growth"] + cm_thr["no_high_growth", "high_growth"]))
  false_positive_rates <- c(false_positive_rates, cm_thr["high_growth", "no_high_growth"] /
                              (cm_thr["high_growth", "no_high_growth"] + cm_thr["no_high_growth", "no_high_growth"]))
}

tpr_fpr_for_thresholds <- tibble(
  "threshold" = thresholds,
  "true_positive_rate" = true_positive_rates,
  "false_positive_rate" = false_positive_rates
)

discrete_roc_plot <- ggplot(
  data = tpr_fpr_for_thresholds,
  aes(x = false_positive_rate, y = true_positive_rate, color = threshold)) +
  labs(x = "False positive rate (1 - Specificity)", y = "True positive rate (Sensitivity)") +
  geom_point(size=2, alpha=0.8) +
  scale_x_continuous(expand = c(0.01,0.01), limit=c(0,1), breaks = seq(0,1,0.1)) +
  scale_y_continuous(expand = c(0.01,0.01), limit=c(0,1), breaks = seq(0,1,0.1)) +
  theme_bg() +
  theme(legend.position ="right") +
  theme(legend.title = element_text(size = 4), 
        legend.text = element_text(size = 4),
        legend.key.size = unit(.4, "cm")) 

discrete_roc_plot


# continuous ROC on holdout with best model (Logit 4) -------------------------------------------

roc_obj_holdout <- roc(testing$sales_growth, testing$best_logit_no_loss_pred)

createRocPlot(roc_obj_holdout, "best_logit_no_loss_roc_plot_holdout")

# Confusion table with different tresholds ----------------------------------------------------------

# default: the threshold 0.5 is used to convert probabilities to binary classes
logit_class_prediction <- predict(best_logit_no_loss, newdata = testing)
summary(logit_class_prediction)

# confusion matrix: summarize different type of errors and successfully predicted cases
# positive = "yes": explicitly specify the positive case
cm_object1 <- confusionMatrix(logit_class_prediction, testing$sales_growth_f, positive = "high_growth")
cm1 <- cm_object1$table
cm1

# we can apply different thresholds

# 0.5 same as before
holdout_prediction <-
  ifelse(testing$best_logit_no_loss_pred < 0.5, "no_high_growth", "high_growth") %>%
  factor(levels = c("no_high_growth", "high_growth"))
cm_object1b <- confusionMatrix(holdout_prediction,testing$sales_growth_f)
cm1b <- cm_object1b$table
cm1b

# a sensible choice: mean of predicted probabilities
mean_predicted_default_prob <- mean(testing$best_logit_no_loss_pred)
mean_predicted_default_prob
holdout_prediction <-
  ifelse(testing$best_logit_no_loss_pred < mean_predicted_default_prob, "no_high_growth", "high_growth") %>%
  factor(levels = c("no_high_growth", "high_growth"))
cm_object2 <- confusionMatrix(holdout_prediction,testing$sales_growth_f)
cm2 <- cm_object2$table
cm2







#############################################x
# PART II.
# We have a loss function
########################################

# Introduce loss function
# relative cost of of a false negative classification (as compared with a false positive classification)
FP=2
FN=10
cost = FN/FP
# the prevalence, or the proportion of cases in the population (n.cases/(n.controls+n.cases))
prevelance = sum(training$sales_growth)/length(training$sales_growth)

# Draw ROC Curve and find optimal threshold with loss function --------------------------

best_tresholds <- list()
expected_loss <- list()
logit_cv_rocs <- list()
logit_cv_threshold <- list()
logit_cv_expected_loss <- list()

for (model_name in names(logit_models)) {
  
  model <- logit_models[[model_name]]
  colname <- paste0(model_name,"_prediction")
  
  best_tresholds_cv <- list()
  expected_loss_cv <- list()
  
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <-
      model$pred %>%
      filter(Resample == fold)
    
    roc_obj <- roc(cv_fold$obs, cv_fold$high_growth)
    best_treshold <- coords(roc_obj, "best", ret="all", transpose = FALSE,
                            best.method="youden", best.weights=c(cost, prevelance))
    best_tresholds_cv[[fold]] <- best_treshold$threshold
    expected_loss_cv[[fold]] <- (best_treshold$fp*FP + best_treshold$fn*FN)/length(cv_fold$high_growth)
  }
  
  # average
  best_tresholds[[model_name]] <- mean(unlist(best_tresholds_cv))
  expected_loss[[model_name]] <- mean(unlist(expected_loss_cv))
  
  # for fold #5
  logit_cv_rocs[[model_name]] <- roc_obj
  logit_cv_threshold[[model_name]] <- best_treshold
  logit_cv_expected_loss[[model_name]] <- expected_loss_cv[[fold]]
  
}

logit_summary2 <- data.frame("Avg of optimal thresholds" = unlist(best_tresholds),
                             "Threshold for Fold5" = sapply(logit_cv_threshold, function(x) {x$threshold}),
                             "Avg expected loss" = unlist(expected_loss),
                             "Expected loss for Fold5" = unlist(logit_cv_expected_loss))


logit_summary2

# Create plots based on Fold5 in CV ----------------------------------------------

for (model_name in names(logit_cv_rocs)) {
  
  r <- logit_cv_rocs[[model_name]]
  best_coords <- logit_cv_threshold[[model_name]]
  createLossPlot(r, best_coords,
                 paste0(model_name, "_loss_plot"))
  createRocPlotWithOptimal(r, best_coords,
                           paste0(model_name, "_roc_plot"))
}

# Pick best model based on average expected loss ----------------------------------

best_logit_with_loss <- logit_models[["X4"]]
best_logit_optimal_treshold <- best_tresholds[["X4"]]

logit_predicted_probabilities_holdout <- predict(best_logit_with_loss, newdata = testing, type = "prob")
testing[,"best_logit_with_loss_pred"] <- logit_predicted_probabilities_holdout[,"high_growth"]

# ROC curve on holdout
roc_obj_holdout <- roc(testing$sales_growth, testing[, "best_logit_with_loss_pred", drop=TRUE])

# Get expected loss on holdout
holdout_treshold <- coords(roc_obj_holdout, x = best_logit_optimal_treshold, input= "threshold",
                           ret="all", transpose = FALSE)
expected_loss_holdout <- (holdout_treshold$fp*FP + holdout_treshold$fn*FN)/length(testing$sales_growth)
expected_loss_holdout

# Confusion table on holdout with optimal threshold
best_logit_optimal_treshold

holdout_prediction <-
  ifelse(testing$best_logit_with_loss_pred < best_logit_optimal_treshold, "no_high_growth", "high_growth" ) %>%
  factor(levels = c("no_high_growth", "high_growth"  ))
cm_object3 <- confusionMatrix(holdout_prediction,testing$sales_growth_f)
cm3 <- cm_object3$table
cm3



