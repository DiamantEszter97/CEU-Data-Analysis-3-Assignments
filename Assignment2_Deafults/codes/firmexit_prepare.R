# clear working environment 
rm(list = ls())

# load libraries:
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

# set working environment
setwd("c:/Users/diama/Documents/CEU-Business-Analytics-2020/Data_Analysis3/class_material/da_case_studies/")

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
source("set-data-directory.R") #data_dir must be first defined #

######################
# load data:
first_df <- read_csv("https://raw.githubusercontent.com/DiamantEszter97/CEU-Data-Analysis-3-Assignments/main/Assignment2_Deafults/data/clean/cs_bisnode_panel.csv")



# save it to another dataframe to avoid long data loading
df <- first_df

# drop variables with many missing values
df <- df %>%
  select(-c(COGS, finished_prod, net_dom_sales, net_exp_sales, wages)) %>%
  filter(year !=2016)

###########################################################
# label engineering
###########################################################

# add all missing year and comp_id combinations -
# originally missing combinations will have NAs in all other columns
df <- df %>%
  complete(year, comp_id)

# generate status_alive; if sales larger than zero and not-NA, then firm is alive
df  <- df %>%
  mutate(status_alive = sales > 0 & !is.na(sales) %>%
           as.numeric(.))

# defaults in two years if there are sales in this year but no sales two years later
df <- df %>%
  group_by(comp_id) %>%
  mutate(default = ((status_alive == 1) & (lead(status_alive, 2) == 0)) %>%
           as.numeric(.)) %>%
  ungroup()

# filter out until 2014:
df <- df %>%
  filter(year <=2014)

# Size and growth
summary(df$sales) # There will be NAs, we'll drop them soon

# if sales less than 0, it is set to 1, otherwise it stays as it is. 
# if sales greater than 0, make log(sales), otherwise 0
# sales calculated to millions
# if sales is greater than 0, log(million sales), otherwise 0
df <- df %>%
  mutate(sales = ifelse(sales < 0, 1, sales),
         ln_sales = ifelse(sales > 0, log(sales), 0),
         sales_mil=sales/1000000,
         sales_mil_log = ifelse(sales > 0, log(sales_mil), 0))

# calculates million sales log minus where it is missing value set to 1
df <- df %>%
  group_by(comp_id) %>%
  mutate(d1_sales_mil_log = sales_mil_log - Lag(sales_mil_log, 1) ) %>%
  ungroup()

# replace w 0 for new firms + add dummy to capture it
df <- df %>%
  mutate(age = (year - founded_year) %>%
           ifelse(. < 0, 0, .),
         new = as.numeric(age <= 1) %>% #  (age could be 0,1 )
           ifelse(balsheet_notfullyear == 1, 1, .),
         d1_sales_mil_log = ifelse(new == 1, 0, d1_sales_mil_log),
         new = ifelse(is.na(d1_sales_mil_log), 1, new),
         d1_sales_mil_log = ifelse(is.na(d1_sales_mil_log), 0, d1_sales_mil_log))


# look at cross section
df <- df %>%
  filter((year == 2012) & (status_alive == 1)) %>%
  # look at firms below 10m euro revenues and above 1000 euros
  filter(!(sales_mil > 10)) %>%
  filter(!(sales_mil < 0.001))

Hmisc::describe(df$default)


###########################################################
# Feature engineering
###########################################################


# change some industry category codes
df <- df %>%
  mutate(ind2_cat = ind2 %>%
           ifelse(. > 56, 60, .)  %>%
           ifelse(. < 26, 20, .) %>%
           ifelse(. < 55 & . > 35, 40, .) %>%
           ifelse(. == 31, 30, .) %>%
           ifelse(is.na(.), 99, .)
  )

table(df$ind2_cat)

# Firm characteristics
df <- df %>%
  mutate(age2 = age^2,
         foreign_management = as.numeric(foreign >= 0.5),
         gender_m = factor(gender, levels = c("female", "male", "mix")),
         m_region_loc = factor(region_m, levels = c("Central", "East", "West")))


###########################################################
# look at more financial variables, create ratios
###########################################################

# assets can't be negative. Change them to 0 and add a flag.
df <-df  %>%
  mutate(flag_asset_problem=ifelse(intang_assets<0 | curr_assets<0 | fixed_assets<0,1,0  ))
table(df$flag_asset_problem)

df <- df %>%
  mutate(intang_assets = ifelse(intang_assets < 0, 0, intang_assets),
         curr_assets = ifelse(curr_assets < 0, 0, curr_assets),
         fixed_assets = ifelse(fixed_assets < 0, 0, fixed_assets))

# generate total assets
df <- df %>%
  mutate(total_assets_bs = intang_assets + curr_assets + fixed_assets)
summary(df$total_assets_bs)


pl_names <- c("extra_exp","extra_inc",  "extra_profit_loss", "inc_bef_tax" ,"inventories",
              "material_exp", "profit_loss_year", "personnel_exp")
bs_names <- c("intang_assets", "curr_liab", "fixed_assets", "liq_assets", "curr_assets",
              "share_eq", "subscribed_cap", "tang_assets" )

# divide all pl_names elements by sales and create new column for it
df <- df %>%
  mutate_at(vars(pl_names), funs("pl"=./sales))

# divide all bs_names elements by total_assets_bs and create new column for it
df <- df %>%
  mutate_at(vars(bs_names), funs("bs"=ifelse(total_assets_bs == 0, 0, ./total_assets_bs)))

########################################################################
# creating flags, and winsorizing tails
########################################################################

# Variables that represent accounting items that cannot be negative (e.g. materials)
zero <-  c("extra_exp_pl", "extra_inc_pl", "inventories_pl", "material_exp_pl", "personnel_exp_pl",
           "curr_liab_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs", "subscribed_cap_bs",
           "intang_assets_bs")

df <- df %>%
  mutate_at(vars(zero), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(zero), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(zero), funs("flag_error"= as.numeric(.< 0))) %>%
  mutate_at(vars(zero), funs(ifelse(.< 0, 0, .)))

# for vars that could be any, but are mostly between -1 and 1
any <-  c("extra_profit_loss_pl", "inc_bef_tax_pl", "profit_loss_year_pl", "share_eq_bs")

df <- df %>%
  mutate_at(vars(any), funs("flag_low"= as.numeric(.< -1))) %>%
  mutate_at(vars(any), funs(ifelse(.< -1, -1, .))) %>%
  mutate_at(vars(any), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(any), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(any), funs("flag_zero"= as.numeric(.== 0))) %>%
  mutate_at(vars(any), funs("quad"= .^2))

# dropping flags with no variation
variances<- df %>%
  select(contains("flag")) %>%
  apply(2, var, na.rm = TRUE) == 0

df <- df %>%
  select(-one_of(names(variances)[variances]))


########################################################################
# additional
# including some imputation
########################################################################

# CEO age
df <- df %>%
  mutate(ceo_age = year-birth_year,
         flag_low_ceo_age = as.numeric(ceo_age < 25 & !is.na(ceo_age)),
         flag_high_ceo_age = as.numeric(ceo_age > 75 & !is.na(ceo_age)),
         flag_miss_ceo_age = as.numeric(is.na(ceo_age)))

df <- df %>%
  mutate(ceo_age = ifelse(ceo_age < 25, 25, ceo_age) %>%
           ifelse(. > 75, 75, .) %>%
           ifelse(is.na(.), mean(., na.rm = TRUE), .),
         ceo_young = as.numeric(ceo_age < 40))

# number emp, very noisy measure
df <- df %>%
  mutate(labor_avg_mod = ifelse(is.na(labor_avg), mean(labor_avg, na.rm = TRUE), labor_avg),
         flag_miss_labor_avg = as.numeric(is.na(labor_avg)))

summary(df$labor_avg)
summary(df$labor_avg_mod)

df <- df %>%
  select(-labor_avg)

# create factors
df <- df %>%
  mutate(urban_m = factor(urban_m, levels = c(1,2,3)),
         ind2_cat = factor(ind2_cat, levels = sort(unique(df$ind2_cat))))

df <- df %>%
  mutate(default_f = factor(default, levels = c(0,1)) %>%
           recode(., `0` = 'no_default', `1` = "default"))


########################################################################
# sales 
########################################################################

df <- df %>%
  mutate(sales_mil_log_sq=sales_mil_log^2)


ggplot(data = df, aes(x=sales_mil_log, y=as.numeric(default))) +
  geom_point(size=2,  shape=20, stroke=2, fill="blue", color="blue") +
  geom_smooth(method = "lm", formula = y ~ poly(x,2), color=color[4], se = F, size=1)+
  geom_smooth(method="loess", se=F, colour=color[5], size=1.5, span=0.9) +
  labs(x = "sales_mil_log",y = "default") +
  theme_bg()


ols_s <- lm(default~sales_mil_log+sales_mil_log_sq,
            data = df)
summary(ols_s)


########################################################################
# sales change
########################################################################


# lowess
Hmisc::describe(df$d1_sales_mil_log) # no missing

ggplot(data = df, aes(x=d1_sales_mil_log, y=as.numeric(default))) +
  geom_point(size=2,  shape=20, stroke=2, fill="blue", color="blue") +
  geom_smooth(method="loess", se=F, colour="black", size=1.5, span=0.9) +
  labs(x = "d1_sales_mil_log",y = "default") +
  theme_bg() +
  scale_x_continuous(limits = c(-6,10), breaks = seq(-5,10, 5))

# generate variables ---------------------------------------------------

df <- df %>%
  mutate(flag_low_d1_sales_mil_log = ifelse(d1_sales_mil_log < -1.5, 1, 0),
         flag_high_d1_sales_mil_log = ifelse(d1_sales_mil_log > 1.5, 1, 0),
         d1_sales_mil_log_mod = ifelse(d1_sales_mil_log < -1.5, -1.5,
                                       ifelse(d1_sales_mil_log > 1.5, 1.5, d1_sales_mil_log)),
         d1_sales_mil_log_mod_sq = d1_sales_mil_log_mod^2
  )

# no more imputation, drop obs if key vars missing
df <- df %>%
  filter(!is.na(liq_assets_bs),!is.na(foreign), !is.na(ind))

# drop missing
df <- df %>%
  filter(!is.na(age),!is.na(foreign), !is.na(material_exp_pl), !is.na(m_region_loc))
Hmisc::describe(df$age)

# drop unused factor levels
df <- df %>%
  mutate_at(vars(colnames(df)[sapply(df, is.factor)]), funs(fct_drop))

ggplot(data = df, aes(x=d1_sales_mil_log_mod, y=as.numeric(default))) +
  geom_point(size=2,  shape=20, stroke=2, fill="blue", color="blue") +
  geom_smooth(method="loess", se=F, colour="black", size=1.5, span=0.9) +
  labs(x = "d1_sales_mil_log",y = "default") +
  theme_bg() +
  scale_x_continuous(limits = c(-1.5,1.5), breaks = seq(-1.5,1.5, 0.5))

# check variables
skimr::skim(df)


# save:
out <- "C:/Users/diama/Documents/CEU-BA-Assignments/CEU-Data-Analysis-3-Assignments/Assignment2_Deafults/data/clean/"
write_csv(df, paste0(out, "bisnode_firms_clean.csv"))
write_rds(df, paste0(out, "bisnode_firms_clean.rds"))



