# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

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
library(pander)


## Call important theme and graph functions
source("D:/CEU/Data_Analysis_3/DA3_Yuri/HW2/codes/Theme_GraphFunctions/theme_bg.R")
source("D:/CEU/Data_Analysis_3/DA3_Yuri/HW2/codes/Theme_GraphFunctions/da_helper_functions.R")


###########################################################
# Import data
###########################################################

data <- read_csv("D:/CEU/Data_Analysis_3/DA3_Yuri/HW2/data/raw/cs_bisnode_panel.csv")

#Quick dataset check
skim(data)

# drop variables with many NAs
data <- data %>%
  select(-c(COGS, finished_prod, net_dom_sales, net_exp_sales, wages, D)) 

# Filter the year range of analysis
# Check the distribution in terms of quantity of data per year
describe(data$year)
hist(data$year)

# Filter the dataset based on the project requirements
data <- data %>% 
  filter(year > 2011, 
         year < 2015,
         !is.na(sales))

# Leave only the company id's that have data for the entire period

unique_comp_id <- as.vector(data %>% 
    group_by(comp_id) %>% 
    summarize(n_obs = n()) %>% 
    rename (comp_id_agg = comp_id) %>% 
    filter(n_obs == 3) %>% 
    select(comp_id_agg))

data <- data %>% 
        filter(comp_id %in% unique_comp_id$comp_id_agg)

###########################################################
# label engineering
###########################################################

# Create fast growth variable (Dummy)
# The dummy variable is gonna be created based on the sales column only, 
# not enough financial knowledge for the other ones. More conservative analysis

########################## Fast growth coefficient ###############################

CAGR <- data %>% 
  spread(year, sales) %>% 
  rename(c_2012  = "2012",
         c_2013  = "2013",
         c_2014  = "2014") %>%
  mutate(c_2012 = as.numeric(c_2012),
         c_2013 = as.numeric(c_2013),
         c_2014 = as.numeric(c_2014)) %>% 
  select(comp_id, "c_2012", "c_2013", "c_2014") %>% 
  group_by(comp_id) %>% 
  summarise(m_2012 = mean(c_2012, na.rm = TRUE),
            m_2013 = mean(c_2013, na.rm = TRUE),
            m_2014 = mean(c_2014, na.rm = TRUE))

## Add Compound Annual Growth Rate and Absolute Sales Difference 

CAGR <- CAGR %>% 
  mutate(coefficient =  ifelse(is.infinite((m_2014/m_2012)^(1/2)-1) | is.na((m_2014/m_2012)^(1/2)-1), 0, (m_2014/m_2012)^(1/2)-1), 
         abs_difference = m_2014 - m_2012)

## Analyze the coefficients and absolute difference distribution 
coeff_parameter  <- data.frame(t(unclass(summary(CAGR$coefficient))))
absdif_parameter <- data.frame(t(unclass(summary(CAGR$abs_difference)))) 

# fast-growth: coefficient > mean(coefficient) and abs_difference > 3rdQuartile (abs_difference) 
CAGR <- CAGR %>% 
  mutate(fast_growth = ifelse( coefficient > coeff_parameter$Mean & abs_difference > absdif_parameter$X3rd.Qu. , 1 , 0))

describe(CAGR$fast_growth)  
hist(CAGR$fast_growth) 

CAGR <- CAGR %>% 
      select("comp_id", "fast_growth")

################################# Factor Variables ########################################################################

# As I'm looping through the same table, the order of the values (comp_id's) remain the same, so I can simply add the columns instead of merging the tables

factor_df <- NULL
factor_df <- data.frame(comp_id = unique_comp_id$comp_id_agg)

factor_variables <- c("gender", "ceo_count" , "foreign" , "inoffice_days", "ind" , "ind2" , "region_m", "origin" , "urban_m")

for (i in factor_variables) {

  factor_var <- data %>% 
    spread(year, i) %>% 
    rename(c_2012  = "2012",
           c_2013  = "2013",
           c_2014  = "2014") %>%
    select(comp_id, "c_2012", "c_2013", "c_2014") %>% 
    group_by(comp_id) %>% 
    summarise(m_2012 = max(c_2012, na.rm = TRUE),
              m_2013 = max(c_2013, na.rm = TRUE),
              m_2014 = max(c_2014, na.rm = TRUE))
  
  column_name_adj <- paste0("changed_", i)
  
  factor_df[[column_name_adj]] <- ifelse(factor_var$m_2014 == factor_var$m_2012, 0 , 1) 
  
}

factor_df[is.na(factor_df)] <- 0

rm(absdif_parameter, coeff_parameter, factor_var, unique_comp_id)

###########################################################
# Feature engineering
###########################################################

# Filter only the values used for the prediction
data <- data %>% 
  filter(year == 2012)

data <- merge(data, factor_df, by = "comp_id")
data <- merge(data, CAGR, by = "comp_id")

# change some industry category codes
data <- data %>%
  mutate(ind2_cat = ind2 %>%
           ifelse(. > 56, 60, .)  %>%
           ifelse(. < 26, 20, .) %>%
           ifelse(. < 55 & . > 35, 40, .) %>%
           ifelse(. == 31, 30, .) %>%
           ifelse(is.na(.), 99, .)
           )

table(data$ind2_cat)

data <- data %>%
  mutate(sales = ifelse(sales < 0, 1, sales),
         ln_sales = ifelse(sales > 0, log(sales), 0),
         sales_mil=sales/1000000,
         sales_mil_log = ifelse(sales > 0, log(sales_mil), 0))


# replace w 0 for new firms + add dummy to capture it
data <- data %>%
  mutate(age = (year - founded_year) %>%
           ifelse(. < 0, 0, .),
         new = as.numeric(age <= 1) %>% #  (age could be 0,1 )
           ifelse(balsheet_notfullyear == 1, 1, .))

# Firm characteristics
data <- data %>%
  mutate(age2 = age^2,
         foreign_management = as.numeric(foreign >= 0.5),
         gender_m = factor(gender, levels = c("female", "male", "mix")),
         m_region_loc = factor(region_m, levels = c("Central", "East", "West")))

###########################################################
# look at more financial variables, create ratios
###########################################################

# assets can't be negative. Change them to 0 and add a flag.
data <-data  %>%
  mutate(flag_asset_problem = ifelse(intang_assets < 0 | curr_assets < 0 | fixed_assets < 0,1,0 ))
table(data$flag_asset_problem)

data <- data %>%
  mutate(intang_assets = ifelse(intang_assets < 0, 0, intang_assets),
         curr_assets = ifelse(curr_assets < 0, 0, curr_assets),
         fixed_assets = ifelse(fixed_assets < 0, 0, fixed_assets))

# generate total assets
data <- data %>%
  mutate(total_assets_bs = intang_assets + curr_assets + fixed_assets)
summary(data$total_assets_bs)


pl_names <- c("extra_exp","extra_inc",  "extra_profit_loss", "inc_bef_tax" ,"inventories",
              "material_exp", "profit_loss_year", "personnel_exp")
bs_names <- c("intang_assets", "curr_liab", "fixed_assets", "liq_assets", "curr_assets",
              "share_eq", "subscribed_cap", "tang_assets" )

# divide all pl_names elements by sales and create new column for it
data <- data %>%
  mutate_at(vars(pl_names), funs("pl"=./sales))

# divide all bs_names elements by total_assets_bs and create new column for it
data <- data %>%
  mutate_at(vars(bs_names), funs("bs"=ifelse(total_assets_bs == 0, 0, ./total_assets_bs)))


########################################################################
# creating flags, and winsorizing tails
########################################################################

# Variables that represent accounting items that cannot be negative (e.g. materials)
zero <-  c("extra_exp_pl", "extra_inc_pl", "inventories_pl", "material_exp_pl", "personnel_exp_pl",
           "curr_liab_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs", "subscribed_cap_bs",
           "intang_assets_bs")

data <- data %>%
  mutate_at(vars(zero), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(zero), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(zero), funs("flag_error"= as.numeric(.< 0))) %>%
  mutate_at(vars(zero), funs(ifelse(.< 0, 0, .)))


# for vars that could be any, but are mostly between -1 and 1
any <-  c("extra_profit_loss_pl", "inc_bef_tax_pl", "profit_loss_year_pl", "share_eq_bs")

data <- data %>%
  mutate_at(vars(any), funs("flag_low"= as.numeric(.< -1))) %>%
  mutate_at(vars(any), funs(ifelse(.< -1, -1, .))) %>%
  mutate_at(vars(any), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(any), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(any), funs("flag_zero"= as.numeric(.== 0))) %>%
  mutate_at(vars(any), funs("quad"= .^2))



# dropping flags with no variation
variances<- data %>%
  select(contains("flag")) %>%
  apply(2, var, na.rm = TRUE) == 0

data <- data %>%
  select(-one_of(names(variances)[variances]))

########################################################################
# additional
# including some imputation
########################################################################

# CEO age
data <- data %>%
  mutate(ceo_age = year-birth_year,
         flag_low_ceo_age = as.numeric(ceo_age < 25 & !is.na(ceo_age)),
         flag_high_ceo_age = as.numeric(ceo_age > 75 & !is.na(ceo_age)),
         flag_miss_ceo_age = as.numeric(is.na(ceo_age)))

data <- data %>%
  mutate(ceo_age = ifelse(ceo_age < 25, 25, ceo_age) %>%
           ifelse(. > 75, 75, .) %>%
           ifelse(is.na(.), mean(., na.rm = TRUE), .),
         ceo_young = as.numeric(ceo_age < 40))

# number emp, very noisy measure
data <- data %>%
  mutate(labor_avg_mod = ifelse(is.na(labor_avg), mean(labor_avg, na.rm = TRUE), labor_avg),
         flag_miss_labor_avg = as.numeric(is.na(labor_avg)))

summary(data$labor_avg)
summary(data$labor_avg_mod)

data <- data %>%
  select(-labor_avg)

# create factors
data <- data %>%
  mutate(urban_m = factor(urban_m, levels = c(1,2,3)),
         ind2_cat = factor(ind2_cat, levels = sort(unique(data$ind2_cat))))

data <- data %>%
  mutate(fast_growth_f = factor(fast_growth, levels = c(0,1)) %>%
           recode(., `0` = "no_fast_growth", `1` = "fast_growth"))

########################################################################
 # sales 
########################################################################

data <- data %>%
  mutate(sales_mil_log_sq=sales_mil_log^2)

#############################################################################

# no more imputation, drop obs if key vars missing
data <- data %>%
  filter(!is.na(liq_assets_bs),!is.na(foreign), !is.na(ind))

# drop missing
data <- data %>%
  filter(!is.na(age),!is.na(foreign), !is.na(material_exp_pl), !is.na(m_region_loc))


# drop unused factor levels
data <- data %>%
  mutate_at(vars(colnames(data)[sapply(data, is.factor)]), funs(fct_drop))

# filter out NA Values

data <- data %>% 
  filter(!is.na(profit_loss_year_pl),
         !is.na(extra_exp_pl),
         !is.na(extra_inc_pl),
         !is.na(extra_profit_loss_pl),
         !is.na(inc_bef_tax_pl),
         !is.na(inventories_pl),
         !is.na(personnel_exp_pl))
        
# check variables
write_csv(data,"D:/CEU/Data_Analysis_3/DA3_Yuri/HW2/data/clean/fast_growth_clean.csv")
write_rds(data,"D:/CEU/Data_Analysis_3/DA3_Yuri/HW2/data/clean/fast_growth_clean.rds")

