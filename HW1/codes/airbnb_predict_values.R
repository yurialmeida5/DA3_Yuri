# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Descriptive statistics and regressions
library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(stargazer)
library(xtable)
library(directlabels)
library(knitr)
library(cowplot)
library(caret)
library(rpart.plot)


#Import set up functions
source("D:/CEU/Data_Analysis_3/da_case_studies/ch00-tech-prep/theme_bg.R")
source("D:/CEU/Data_Analysis_3/DA3_Yuri/HW1/codes/da_helper_functions.R")

#-------------------------------------------------------
# Import data
data <- read_csv("D:/CEU/Data_Analysis_3/DA3_Yuri/HW1/data/clean/buenos_aires_clean.csv")

data <- data %>% 
  filter(!f_neighbourhood_cleansed == "Villa Lugano",
         !f_neighbourhood_cleansed == "Mataderos",
         !f_neighbourhood_cleansed == "Villa Soldati",
         !f_neighbourhood_cleansed == "Parque Avellaneda",
         !f_neighbourhood_cleansed == "Velez Sarsfield")

# First look at data

glimpse(data)
skim(data)

# Some statistics

data %>%
  group_by(n_accommodates) %>%
  dplyr::summarize(mean_price = mean(ars_price_day, na.rm=TRUE))

Hmisc::describe(data$ars_price_day)

# Check to see if there are any duplicates
duplicated(names(data))

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# what to do with missing values?
# 1. drop if no target # Done in prepare file 
data <- data %>%
  drop_na(ars_price_day)



# 2. imput when few, not that important
data <- data %>%
  mutate(
    n_bathrooms =  ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms), #assume at least 1 bath
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds), #assume n_beds=n_accomodates
    f_bathroom=ifelse(is.na(f_bathroom),1, f_bathroom),
    f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
    f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
    ln_beds=ifelse(is.na(ln_beds),0, ln_beds),
  )

# 3. drop columns when many missing: p_host_response_rate
data <- data %>%
  select(-p_host_response_rate)

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# 4. Replace missing variables reviews with zero, when no review + add flags
data <- data %>%
  mutate(
    flag_days_since = ifelse(is.na(n_days_since),1, 0),
    n_days_since =  ifelse(is.na(n_days_since), median(n_days_since, na.rm = T), n_days_since),
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    flag_reviews_per_month=ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month)
  )
table(data$flag_days_since)

# Look at data
summary(data$ars_price_day)

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

###################################
# Business logic- define our prediction problem
###################################

# that's gonna be our sample
skimr::skim(data)

# Distribution of price 

# Histograms
# Price

g3a <- ggplot(data= data, aes(x= ars_price_day)) +
  geom_histogram_da(type="percent", binwidth = 500, color = "green") +
  labs(x = "Price (ARS)",y = "Percent")+
  theme_bg() 
g3a

# lnprice
g3b<- ggplot(data=data, aes(x=ln_price)) +
  geom_histogram_da(type="percent", binwidth = 0.5, color = "green") +
  labs(x = "ln(price, Eu)",y = "Percent")+
  theme_bg() 
g3b


# Boxplot
g4 <- ggplot(data, aes(x = factor(n_accommodates), y = ars_price_day,
                        fill = factor(property_type))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  labs(x = "Accomodates (Persons)",y = "Price (ARS)" , fill = "Property Type")+
  theme_bg() +
  theme(legend.position = "bottom")
g4

g5 <- ggplot(data, aes(x = factor(n_accommodates), y = ars_price_day,
                       fill = factor(d_instant_bookable))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  labs(x = "Accomodates (Persons)",y = "Price (ARS)" , fill = "Instant_Bookable")+
  theme_bg() +
  theme(legend.position = "bottom")
g5


########################################
# PART II.
########################################


#####################
# Setting up models #
#####################


# Basic Variables
basic_lev  <- c("n_accommodates", "n_beds", "f_bathroom")

# Additional variables
basic_add <- c("f_property_type", "n_minimum_nights", "ln_days_since", "d_location", "d_security" , "d_free_parking" ,
               "d_paid_parking", "d_pets", "d_garden_balcony_backyard", "d_swimming_pool" , "d_cooking" , "d_instant_bookable" )
reviews <- c("f_number_of_reviews","n_review_scores_rating", "flag_review_scores_rating")
# Higher orders
poly_lev <- c("n_accommodates2", "ln_days_since2")


#################################################
# Look for interactions
################################################

#Look up room type interactions
p1 <- price_diff_by_variables2(data, "f_property_type", "d_garden_balcony_backyard", "Property Type", "Garden or backyard")
p2 <- price_diff_by_variables2(data, "f_property_type", "d_swimming_pool", "Property Type", "Swimming Pool")
#Look up property type
p3 <- price_diff_by_variables2(data, "f_property_type", "d_pets", "Property Type", "Pets allowed")
p4 <- price_diff_by_variables2(data, "f_property_type", "d_laundry" , "Property Type", "Laundry")
#Look up parking
p5 <- price_diff_by_variables2(data, "f_property_type", "d_free_parking", "Property Type", "Free Parking")
p6 <- price_diff_by_variables2(data, "f_property_type", "d_paid_parking" , "Property Type", "Paid Parking")

g_interactions <- plot_grid(p1, p2, p3, p4, p5, p6, nrow=3, ncol=2)
g_interactions

# dummies suggested by graphs
X1  <- c("f_property_type*d_garden_balcony_backyard", "f_property_type*d_swimming_pool" , "f_property_type*d_laundry" )


# Additional dummies
X2  <- c("d_baby_utilities", "d_ac" , "d_amendities" , "d_children" , "d_bath", "d_wifi" ,
         "d_sauna", "d_office", "d_fridge" , "d_cloth_storage" , "d_sound_system", "f_neighbourhood_cleansed")
X3  <- c("d_barbecue", "d_coffee_maker", "d_bike", "d_eletric_car_charger", "d_luggage_storage", 
         "d_self_checkin", "d_mandatory_cleaning" , "d_smoking" , "d_videogame")


# Create models in levels models: 1-7
modellev1 <- " ~ n_accommodates"
modellev2 <- paste0(" ~ ",paste(basic_lev,collapse = " + "))
modellev3 <- paste0(" ~ ",paste(c(basic_lev, basic_add,reviews),collapse = " + "))
modellev4 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev),collapse = " + "))
modellev5 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1),collapse = " + "))
modellev6 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2),collapse = " + "))
modellev7 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2,X3),collapse = " + "))


#################################
# Separate hold-out set #
#################################

# create a holdout set (20% of observations)
smp_size <- floor(0.2 * nrow(data))

# Set the random number generator: It will make results reproducable
set.seed(20180123)

# create ids:
# 1) seq_len: generate regular sequences
# 2) sample: select random rows from a table
holdout_ids <- sample(seq_len(nrow(data)), size = smp_size)
data$holdout <- 0
data$holdout[holdout_ids] <- 1

#Hold-out set Set
data_holdout <- data %>% filter(holdout == 1)

#Working data set
data_work <- data %>% filter(holdout == 0)


##############################
#      cross validation      #
##############################

## N = 5
n_folds=5
# Create the folds
set.seed(20180124)

folds_i <- sample(rep(1:n_folds, length.out = nrow(data_work) ))
# Create results
model_results_cv <- list()

for (i in (1:7)){
  model_name <-  paste0("modellev",i)
  model_pretty_name <- paste0("(",i,")")
  
  yvar <- "ars_price_day" 
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
    rmse_train[k] <- mse_lev(prediction_train, data_train$ars_price_day)**(1/2)
    rmse_test[k] <- mse_lev(prediction_test, data_train$ars_price_day)**(1/2)
    
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

# RMSE training vs test graph
t1_levels <- t1 %>%
  dplyr::select("nvars", "rmse_train", "rmse_test") %>%
  gather(var,value, rmse_train:rmse_test) %>%
  mutate(nvars2=nvars+1) %>%
  mutate(var = factor(var, levels = c("rmse_train", "rmse_test"),
                      labels = c("RMSE Training","RMSE Test")))

model_result_plot_levels <- ggplot(data = t1_levels,
                                   aes(x = factor(nvars2), y = value, color=factor(var), group = var)) +
  geom_line(size=1,show.legend=FALSE, na.rm = TRUE) +
  scale_color_manual(name="",
                     values=c(color[2],color[1])) +
  scale_x_discrete( name = "Number of coefficients", expand=c(0.01, 0.01)) +
  geom_dl(aes(label = var),  method = list("last.points", dl.trans(x=x-1), cex=0.4)) +
  #scale_colour_discrete(guide = 'none') +
  theme_bg()
model_result_plot_levels


# Model 1 is best in terms of RMSE, but model 6 is best in terms of  BIC


#################################
#           LASSO               #
#################################

# take model 7 (and find observations where there is no missing data)
vars_model_6 <- c("ars_price_day", basic_lev,basic_add,reviews,poly_lev,X1,X2)
vars_model_7 <- c("ars_price_day", basic_lev,basic_add,reviews,poly_lev,X1,X2,X3)

# Set lasso tuning parameters
train_control <- trainControl(method = "cv", number = n_folds)
tune_grid <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))

# We use model 7 without the interactions so that it is easy to compare later to post lasso ols
formula <- formula(paste0("ars_price_day ~ ", paste(setdiff(vars_model_7, "ars_price_day"), collapse = " + ")))

set.seed(1234)
lasso_model <- caret::train(formula,
                            data = data_work,
                            method = "glmnet",
                            preProcess = c("center", "scale"),
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            na.action=na.exclude)

print(lasso_model$bestTune$lambda)

lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `1`)

print(lasso_coeffs)

lasso_coeffs_nz<-lasso_coeffs %>%
  filter(coefficient!=0)
print(nrow(lasso_coeffs_nz))

# Evaluate model. CV error:
lasso_cv_rmse <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) %>%
  dplyr::select(RMSE)
print(lasso_cv_rmse[1, 1])


########################################
# PART III.
########################################


###################################################
# Diagnsotics #
###################################################
model6_level <- model_results_cv[["modellev6"]][["model_work_data"]]
model1_level <- model_results_cv[["modellev1"]][["model_work_data"]]

# look at holdout RMSE
model6_level_work_rmse <- mse_lev(predict(model6_level, newdata = data_work), data_work$ars_price_day)**(1/2)
model6_level_holdout_rmse <- mse_lev(predict(model6_level, newdata = data_holdout), data_holdout$ars_price_day)**(1/2)
model6_level_holdout_rmse

# look at holdout RMSE
model1_level_work_rmse <- mse_lev(predict(model1_level, newdata = data_work), data_work$ars_price_day)**(1/2)
model1_level_holdout_rmse <- mse_lev(predict(model1_level, newdata = data_holdout), data_holdout$ars_price_day)**(1/2)
model1_level_holdout_rmse


# The difference between is quite significant, Model 6 performs better on the holdout set.
# Further it is simpler and therefore easier to use and explain.

###################################################
# FIGURES FOR FITTED VS ACTUAL OUTCOME VARIABLES #
###################################################

# Target variable
Ylev <- data_holdout[["ars_price_day"]] 

meanY <-mean(Ylev)
sdY <- sd(Ylev)
meanY_m2SE <- meanY -1.96 * sdY
meanY_p2SE <- meanY + 1.96 * sdY
Y5p <- quantile(Ylev, 0.05, na.rm=TRUE)
Y95p <- quantile(Ylev, 0.95, na.rm=TRUE)

# Predicted values
predictionlev_holdout_pred <- as.data.frame(predict(model6_level, newdata = data_holdout, interval="predict")) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model6_level, newdata = data_holdout, interval="confidence")) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("price","n_accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])

predictionlev_holdout

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
  labs(y = "Price (ARS)", x = "Predicted price  (ARS)") +
  theme_bg() 
level_vs_pred

# Redo predicted values at 80% PI
predictionlev_holdout_pred <- as.data.frame(predict(model6_level, newdata = data_holdout, interval="predict", level=0.8)) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model6_level, newdata = data_holdout, interval="confidence", level=0.8)) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("price","n_accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])

summary(predictionlev_holdout_pred)

predictionlev_holdout_summary <-
  predictionlev_holdout %>%
  group_by(n_accommodates) %>%
  dplyr::summarise(fit = mean(fit, na.rm=TRUE), pred_lwr = mean(pred_lwr, na.rm=TRUE), pred_upr = mean(pred_upr, na.rm=TRUE),
                   conf_lwr = mean(conf_lwr, na.rm=TRUE), conf_upr = mean(conf_upr, na.rm=TRUE))

kable(x = predictionlev_holdout_summary, format = "latex", booktabs=TRUE,  digits = 3, row.names = FALSE,
      linesep = "", col.names = c("Accomodates","Prediction","Pred. interval lower",
                                  "Pred. interval upper","Conf.interval lower","Conf.interval upper")) %>%
  cat(.,file= "D:/CEU/Data_Analysis_3/DA3_Yuri/HW1/out/modellev6_holdout_summary.tex")


F14_CI_n_accomodate <- ggplot(predictionlev_holdout_summary, aes(x=factor(n_accommodates))) +
  geom_bar(aes(y = fit ), stat="identity",  fill = color[1], alpha=0.7 ) +
  geom_errorbar(aes(ymin=pred_lwr, ymax=pred_upr, color = "Pred. interval"),width=.2) +
  #geom_errorbar(aes(ymin=conf_lwr, ymax=conf_upr, color = "Conf. interval"),width=.2) +
  scale_y_continuous(name = "Predicted price (ARS)") +
  scale_x_discrete(name = "Accomodates (Persons)") +
  scale_color_manual(values=c(color[2], color[2])) +
  theme_bg() +
  theme(legend.title= element_blank(),legend.position="none")
F14_CI_n_accomodate

summary(data$f_property_type)

#NOT USED
# Density chart (not in book)
g3 <- ggplot(data = data, aes(x= ars_price_day)) +
  geom_density(aes(color= f_property_type, fill=f_property_type),  na.rm =TRUE, alpha= 0.3) +
  labs(x="Price (ARS)", y="Density", color = "") +
  scale_color_manual(name="",
                     values=c(color[2],color[1]),
                     labels=c("Entire Apartment","Serviced Apartment")) +
  scale_fill_manual(name="",
                    values=c(color[2],color[1]),
                    labels=c("Entire Apartment","Serviced Apartment")) +
theme_bg() + 
theme(legend.position = c(0.7,0.7),
      legend.direction = "horizontal",
      legend.background = element_blank(),
      legend.box.background = element_rect(color = "white"))
g3


# Barchart  (not in book)
plot_dist <- ggplot(data = data, aes(x = factor(n_accommodates), color = f_property_type, fill = f_property_type)) +
  geom_bar(alpha=0.8, na.rm=T, width = 0.8) +
  scale_color_manual(name="",
                     values=c(color[2],color[1])) +
  scale_fill_manual(name="",
                    values=c(color[2],color[1])) +
  labs(x = "Accomodates (Persons)",y = "Frequency")+
  theme_bg() +
  theme(legend.position = "top")
plot_dist


###################################################
#                       CART                      #
###################################################

# CART model with two slipts
# Formula
model1 <- formula(ars_price_day ~ n_accommodates)

cart1 <- train(
  model1, data = data_train, method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth=5))

summary(cart1)
pred_cart1 <- predict(cart1, data_test)
rmse_cart1 <- sqrt(mean((pred_cart1 - data_test$ars_price_day)^2))

rpart.plot(cart1$finalModel, tweak=1.2, digits=-1, extra=1)

# CART with an rport default

cart2 <- train(
  model1, data = data_train, method = "rpart",
  trControl = trainControl(method="none"),
  tuneGrid= expand.grid(cp = 0.01))

summary(cart2)
pred_cart2 <- predict(cart2, data_test)
rmse_cart2 <- sqrt(mean((pred_cart2 - data_test$ars_price_day)^2))

# Tree graph
rpart.plot(cart2$finalModel, tweak=1.2, digits=-1, extra=1)

# Since there are only a few factors in this variable there is not many splits that can happen.

# CART with multiple factors

# Design models
yvar <- "ars_price_day" 
model5 <- formula(formula(paste0(yvar,modellev5)))
model6 <- formula(formula(paste0(yvar,modellev6)))
model7 <- formula(formula(paste0(yvar,modellev7)))

# Tree model based on model 5
cart3 <- train(
  model5, data=data_train, method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth=4),
  na.action = na.pass)

summary(cart3)
pred_cart3 <- predict(cart3, data_test, na.action = na.pass)
rmse_cart3 <- sqrt(mean((pred_cart3 - data_test$ars_price_day)^2))
# Tree graph
rpart.plot(cart3$finalModel, tweak=1.2, digits=-1, extra=1)


# Tree model based on model 7
cart4 <- train(
  model7, data=data_train, method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth=10),
  na.action = na.pass)

summary(cart4)
pred_cart4 <- predict(cart4, data_test, na.action = na.pass)
rmse_cart4 <- sqrt(mean((pred_cart4 - data_test$ars_price_day)^2))
# Tree graph
rpart.plot(cart4$finalModel, tweak=1.2, digits=-1, extra=1)

# Try the pruning method 

cart5 <- train(
  model7, data=data_train, method = "rpart",
  trControl = trainControl(method="none"),
  tuneGrid= expand.grid(cp = 0.001),
  control = rpart.control(minsplit = 4),
  na.action = na.pass)

pfit <-prune(cart5$finalModel, cp=0.01)
summary(pfit)

# Tree graph
rpart.plot(pfit, digits=-1, extra=1, tweak=1)

############################################################

#pred_cart5 <- predict(pfit, data_test, na.action = na.pass)
#rmse_cart5 <- sqrt(mean((pred_cart5 - data_test$ars_price_day)^2))
#rmse_cart5

#not running
#############################################################################

# Variable importance

cart3_var_imp <- varImp(cart3)$importance
cart3_var_imp_df <-
  data.frame(varname = rownames(cart3_var_imp),imp = cart3_var_imp$Overall) %>%
  mutate(varname = gsub("cond_", "Condition:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))

cart3_var_imp_plot <- ggplot(cart3_var_imp_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=2) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=1.5) +
  ylab("Importance") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(expand = c(0.01,0.01),labels = scales::percent_format(accuracy = 1)) +
  theme_bg()

cart3_var_imp_plot


cart4_var_imp <- varImp(cart4)$importance
cart4_var_imp_df <-
  data.frame(varname = rownames(cart4_var_imp),imp = cart4_var_imp$Overall) %>%
  mutate(varname = gsub("cond_", "Condition:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))

cart4_var_imp_plot <- ggplot(cart4_var_imp_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=2) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=1.5) +
  ylab("Importance") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(expand = c(0.01,0.01),labels = scales::percent_format(accuracy = 1)) +
  theme_bg()

cart4_var_imp_plot



###################################################
#                   Random Forest                 #
###################################################

# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)

tune_grid <- expand.grid(
  .mtry = c(5, 7, 9), # number of features to use for the splits
  .splitrule = "variance", # based on what to make the split (minimize variance)
  .min.node.size = c(5, 10) # controlling the complexity of individual trees, minimum node size in this case
)

# simpler model for model A (1)
set.seed(1234)
system.time({
  rf_model_1 <- train(
    model6,
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity", 
    na.action = na.omit
  )
})

rf_model_1

###################################################
#                     GBM                         #
###################################################


gbm_grid <-  expand.grid(interaction.depth = c(1, 5, 10), # complexity of the tree
                         n.trees = (4:10)*50, # number of iterations, i.e. trees
                         shrinkage = 0.1, # learning rate: how quickly the algorithm adapts
                         n.minobsinnode = 20 # the minimum number of training set samples in a node to commence splitting
)


set.seed(1234)
system.time({
  gbm_model <- train(formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
                     data = data_train,
                     method = "gbm",
                     trControl = train_control,
                     verbose = FALSE,
                     tuneGrid = gbm_grid)
})
gbm_model
