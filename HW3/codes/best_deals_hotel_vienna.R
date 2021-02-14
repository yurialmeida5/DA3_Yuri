#####################
#                   #  
# HW3- Find best    #
#  deals for hotels #
#   in Vienna       #
#                   #
#####################

# Clear environment
rm(list = ls())

# Load necessary libraries

library(tidyverse)
library(ggplot2)
library(Hmisc)
library(GGally)
library(caret)
library(rpart.plot)
library(lmtest)
library(estimatr)
library(survPen)
library(skimr)
library(regclass)

# Import data

data <- read_csv("D:/CEU/Data_Analysis_3/DA3_Yuri/HW3/data/raw/hotels-vienna.csv")

# Take a quick look at the data
glimpse(data)


# Creating sample ---------------------------------------------------------

# Check if the requirements are satisfied: One Single Day in November (data came already filtered), Weekday and a non-holiday

describe(data$month)
describe(data$weekend)
describe(data$holiday)


# Looks like we only have the day(s) we need for the exploration


# Label engineering -------------------------------------------------------

# Analyzing dependent variable distribution (Y = Price)

data %>% 
  group_by(price) %>% 
  ggplot(aes(x = price)) +
  geom_histogram() + 
  theme_minimal()

boxplot(data$price,
        ylab = "Price (Eur)")

describe(data$price)

# As it's possible to visualize, the distribution follows a right-skewed tail with a great amount of outliers.
# Considering the size of the following dataset and the pratical importance of those outliers, they won't be excluded
# As our final goal is to find the 5 best deals (not focusing primary on the price itself), 
# the variable data was transformed into log

data <- data %>%  
  mutate(log_price = log(price))

data %>% 
  group_by(log_price) %>% 
  ggplot(aes(x = log_price)) +
  geom_histogram() +
  theme_classic()

# Now the distribution looks way more closer to normal


# Feature engineering -----------------------------------------------------

# Let's take a look at all the different variables that we have in this dataframe

glimpse(data)

# Let's take a look on the explanatory variables

################# Location City Center

data %>% 
  group_by(distance) %>% 
  ggplot(aes(x = distance)) +
  geom_histogram(stat="count") +
  xlab("Average Distance (miles)")

data %>% 
  group_by(distance_alter) %>% 
  ggplot(aes(x = distance_alter)) +
  geom_histogram(stat="count") +
  xlab("Average Distance (miles)")

data %>% 
  ggplot(aes(x = distance, y = log_price)) +
  geom_point() +
  geom_smooth(method="loess")

data %>% 
  ggplot(aes(x = distance_alter, y = log_price)) +
  geom_point() +
  geom_smooth(method="loess")

# Analyze the log distance transformations

data <- data %>% 
  mutate(log_distance = distance,
         log_distance_alter = distance_alter)

# log_distance

data %>% 
  ggplot(aes(x = log_distance, y = log_price)) +
  geom_point() +
  geom_smooth(method="loess")

bptest(lm(log_price ~ distance, data = data))

# Definitely not homoscedastic, big p-value can reject the null hypothesis (homoscedasticity)  
# Use robust model in this case

summary(lm_robust(log_price ~ distance, data = data))
summary(lm_robust(log_price ~ log_distance, data = data))

# no practical/significant difference observed 

# log_distance_alter

data %>% 
  ggplot(aes(x = log_distance_alter, y = log_price)) +
  geom_point() +
  geom_smooth(method="loess")

bptest(lm(log_price ~ distance_alter , data = data))

# Definitely not homoscedastic, big p-value can reject the null hypothesis (homoscedasticity)  
# Use robust model in this case

summary(lm_robust(log_price ~ distance_alter, data = data))
summary(lm_robust(log_price ~ log_distance_alter , data = data))

# no practical/significant difference observed 

########### Location neighborhood

# This one has some variation, so we will convert it into a factor

data <- data %>% 
  mutate(neighbourhood_f = factor(neighbourhood))

# Some only have one or two values though, so we make it "other" for ease of use
filter_data <- data %>% 
  group_by(neighbourhood) %>% 
  summarise(n = n())

# Exclude neighborhoods lower than 5 observations, this will give less noise in the models
filter_data <- filter_data %>% 
  filter(n < 5)

data <- data %>% 
  filter(!neighbourhood %in% filter_data$neighbourhood)

data %>% 
  group_by(neighbourhood_f) %>% 
  ggplot(aes(x = neighbourhood_f)) +
  geom_histogram(stat="count")

rm(filter_data)

# Offer variable

data %>% 
  group_by(offer_cat) %>% 
  ggplot(aes(x = offer_cat)) +
  geom_histogram(stat = "count")

data %>% 
  group_by(offer_cat) %>% 
  summarise(
    n = n())

# Since there is only one value with 75%, I will remove it from our dataset. 
# Reduce the model noise

data <- data %>% 
  filter(!offer_cat == "75%+ offer")

# Convert to factor
data <- data %>% 
  mutate(offer_cat_f = factor(offer_cat))

# Accommodation type variable

data %>% 
  group_by(accommodation_type) %>% 
  ggplot(aes(x = accommodation_type)) +
  geom_histogram(stat = "count")

data %>% 
  group_by(accommodation_type) %>% 
  summarise(
    n = n())

# Looks like the two main types are Apartments and Hotels, so I'm going to make everything else "Other"

filter_data <- data %>% 
  group_by(accommodation_type) %>% 
  summarise(
    n = n())

filter_data <- filter_data %>% 
  filter(n < 50)

data <- data %>% mutate(accommodation_type = ifelse(accommodation_type %in% filter_data$accommodation_type, "Other", accommodation_type))

# Convert to Factor

data  <- data %>% mutate(accommodation_type_f = factor(accommodation_type))

# Let's take a look at what our data looks like now

skim(data)

# The rating columns have some missing values which I will keep though since there are already few observations
# If missing, flag added

data <- data %>% 
          mutate(ratingta = ifelse(is.na(ratingta) == TRUE, 0, ratingta),
          ratingta_flag = ifelse(is.na(ratingta) == TRUE, 1, 0),
          ratingta_count = ifelse(is.na(ratingta_count) == TRUE, 0, ratingta_count),
          ratingta_count_flag = ifelse(is.na(ratingta_count) == TRUE, 1, 0),
          rating_count = ifelse(is.na(rating_count) == TRUE, 0, rating_count),
          rating_count_flag = ifelse(is.na(rating_count) == TRUE, 1, 0),
         rating = ifelse(is.na(rating) == TRUE, 0, rating),
         rating_flag = ifelse(is.na(rating) == TRUE, 1, 0),
)

# rating

data %>% 
  ggplot(aes(x = rating, y = log_price)) +
  geom_point() +
  geom_smooth(method="loess")

# No need for transformation - same for all the rating fields

data %>% 
  ggplot(aes(x = stars, y = price)) +
  geom_point() +
  geom_smooth(method="loess")

# No need for transformation

# Drop non-used variables
data <- data %>% 
  subset(select = -c(country, city_actual, city, center1label, center2label, neighbourhood, nnights,
                     offer_cat, year, month, weekend, holiday, log_distance, log_distance_alter, accommodation_type))

write.csv(data,"D:/CEU/Data_Analysis_3/DA3_Yuri/HW3/data/clean/hotels-vienna_clean.csv")

# X variable selection ----------------------------------------------------

# correlation table of all the variables

ggcorr(data)


# no really strong correlation that should be treated 

# Model building ----------------------------------------------------------

# Variable groupings

yvar <- "log_price"

basics <- c("accommodation_type_f", "neighbourhood_f", "stars")

distance <- c("distance_alter", "distance")

rating <- c("rating", "rating_count", "ratingta", "ratingta_count", 
            "rating_flag", "rating_count_flag", "ratingta_flag" , "ratingta_count_flag")

hotel_related <- c("offer_cat_f", "scarce_room")

interactions_non_num <- c("accommodation_type_f*neighbourhood_f", "accommodation_type_f*offer_cat_f" , "accommodation_type_f*scarce_room",
                           "offer_cat_f*neighbourhood_f", "offer_cat_f*scarce_room", 
                           "neighbourhood_f*scarce_room")

# Model building

X1 <- c(basics)
X2 <- c(basics, distance)
X3 <- c(basics, distance, rating)
X4 <- c(basics, distance, rating, hotel_related)
# Think about using vs not using interactions
LASSO <- c(basics, distance, rating, hotel_related, interactions_non_num)


# The actual models -------------------------------------------------------

model1 <- paste0(" ~ ",paste(X1,collapse = " + "))
model2 <- paste0(" ~ ",paste(X2,collapse = " + "))
model3 <- paste0(" ~ ",paste(X3,collapse = " + "))
model4 <- paste0(" ~ ",paste(X4,collapse = " + "))
LASSO <- paste0(" ~ ",paste(LASSO,collapse = " + "))


# Create holdout set ------------------------------------------------------

set.seed(19920828)

train_indices <- as.integer(createDataPartition(data$log_price, p = 0.8, list = FALSE))
data_work <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

dim(data_work)
dim(data_holdout)

# Linear regression -------------------------------------------------------

# Mean squared loss function
mse_lev <- function(pred, y) {
  # Mean Squared Error for log models
  (mean((pred - y)^2, na.rm=T))
}

## N = 5
n_folds=5
# Create the folds
set.seed(19920828)

folds_i <- sample(rep(1:n_folds, length.out = nrow(data_work) ))
# Create results
model_results_cv <- list()

for (i in (1:4)){
  model_name <-  paste0("model",i)
  model_pretty_name <- paste0("(",i,")")
  
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
    data_test <- data_work
    # Estimation and prediction
    model <- lm(formula,data = data_train)
    prediction_train <- predict(model, newdata = data_train)
    prediction_test <- predict(model, newdata = data_test)
    
    # Criteria evaluation
    rmse_train[k] <- mse_lev(prediction_train, data_train$log_price)**(1/2)
    rmse_test[k] <- mse_lev(prediction_test, data_train$log_price)**(1/2)
    
  }
  
  model_results_cv[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model_work_data=model_work_data,
                                         rmse_train = rmse_train,rmse_test = rmse_test,BIC = BIC,
                                         model_name = model_pretty_name, nvars = nvars, r2 = r2)
}


t1 <- imap(model_results_cv,  ~{
  as.data.frame(.x[c("rmse_test", "rmse_train")]) %>%
    dplyr::summarise_all(.funs = mean) %>%
    mutate("model_name" = .y , "model_pretty_name" = .x[["model_name"]] ,
           "nvars" = .x[["nvars"]], "r2" = .x[["r2"]], "BIC" = .x[["BIC"]])
}) %>%
  bind_rows()
t1

# Model 1 best in terms of BIC and RMSE. (Lowest values)

# LASSO -------------------------------------------------------------------

# Set lasso tuning parameters
train_control <- trainControl(method = "cv", number = n_folds)
tune_grid <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))

# Formula
formula <- formula(paste0(yvar,LASSO))

set.seed(19920828)
lasso_model <- caret::train(formula,
                            data = data_work,
                            method = "glmnet",
                            preProcess = c("center", "scale"),
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            na.action=na.exclude)

lasso_final_model <- lasso_model$finalMode

lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `1`)

lasso_coeffs_nz <-lasso_coeffs %>%
  filter(coefficient!=0)
print(nrow(lasso_coeffs_nz))

lasso_cv_rmse <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) %>%
  dplyr::select(RMSE)
print(lasso_cv_rmse[1, 1])

# The Lasso model turned to have reduced significantly the RMSE error,
# Penalizing the errors and reducing the coefficients to 13.


# CART model --------------------------------------------------------------

# Formula
model_X1 <- formula(formula(paste0(yvar,model1)))
model_LASSO <- formula(formula(paste0(yvar,LASSO)))

# CART model with two slipts

# Simple tree
cart1 <- train(
  model_X1, data=data_work, method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth = 5),
  na.action = na.pass)

summary(cart1)
pred_cart1 <- predict(cart1, data_work, na.action = na.pass)
rmse_cart1 <- sqrt(mean((pred_cart1 - data_work$log_price)^2))
# Tree graph
rpart.plot(cart1$finalModel, tweak=1.2, digits=-1, extra=1)


# Complex tree
cart2 <- train(
  model_LASSO, 
  data= data_work,
  method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth = 5),
  na.action = na.pass)

summary(cart2)
pred_cart2 <- predict(cart2, data_work, na.action = na.pass)
rmse_cart2 <- sqrt(mean((pred_cart2 - data_work$log_price)^2))
# Tree graph
rpart.plot(cart2$finalModel, tweak=1.2, digits=-1, extra=1)

# Compare the two models

cart_compare <- data.frame(
  "Model" = c("CART Model 1", "CART Model LASSO"),
  "RMSE in Train" = c(rmse_cart1, rmse_cart2)
)

# Looks like the RMSE of CART2 is lower, so that one will be used for further reference

# Random Forest -----------------------------------------------------------

# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)

# Tuning parameters
tune_grid <- expand.grid(
  .mtry = c(3, 5, 7), # Since 5 is about the square root, I have picked it plus or minus 2
  .splitrule = "variance",
  .min.node.size = c(5, 10) # While we do not have many variables going smaller than this, seems too niche
)

# Simpler random forest
set.seed(19920828)
system.time({
  rf_model_1 <- train(
    model_X1,
    data = data_work,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity",
    na.action = na.omit
  )
})

# Complex random forest
set.seed(19920828)
system.time({
  rf_model_2 <- train(
    model_LASSO,
    data = data_work,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity",
    na.action = na.omit
  )
})

rf_results <- resamples(
  list(
    model_X1  = rf_model_1,
    model_LASSO  = rf_model_2
  )
) 

summary(rf_results)

# RF model 2 has the lower RSME so that will be used for further reference

# GBM ---------------------------------------------------------------------

###################################################
#                     GBM                         #
###################################################

# X1

gbm_grid <-  expand.grid(interaction.depth = c(1, 5, 10), # complexity of the tree
                         n.trees = (4:10)*50, # number of iterations, i.e. trees
                         shrinkage = 0.1, # learning rate: how quickly the algorithm adapts
                         n.minobsinnode = 20 # the minimum number of training set samples in a node to commence splitting
)


set.seed(19920828)
system.time({
  gbm_model_1 <- train(model_X1,
                       data = data_work,
                       method = "gbm",
                       trControl = train_control,
                       verbose = FALSE,
                       tuneGrid = gbm_grid)
})


# LASSO

gbm_grid <-  expand.grid(interaction.depth = c(1, 5, 10), # complexity of the tree
                         n.trees = (4:10)*50, # number of iterations, i.e. trees
                         shrinkage = 0.1, # learning rate: how quickly the algorithm adapts
                         n.minobsinnode = 20 # the minimum number of training set samples in a node to commence splitting
)


set.seed(19920828)
system.time({
  gbm_model_2 <- train(model_LASSO,
                       data = data_work,
                       method = "gbm",
                       trControl = train_control,
                       verbose = FALSE,
                       tuneGrid = gbm_grid)
})


gbm_results <- resamples(
  list(
    model_X1  = gbm_model_1,
    model_LASSO  = gbm_model_2
  )
) 

summary(gbm_results)



# Compare on holdout set --------------------------------------------------

# The linear model
model1_level <- model_results_cv[["model1"]][["model_work_data"]]

# look at holdout RMSE
model1_level_train_rmse <- mse_lev(predict(model1_level, newdata = data_work), data_work$log_price)**(1/2)
model1_level_holdout_rmse <- mse_lev(predict(model1_level, newdata = data_holdout), data_holdout$log_price)**(1/2)
model1_level_holdout_rmse


# Lasso model

lasso_predictions_train_rmse <-  mse_lev(predict(lasso_model, s = lasso_model$bestTune$lambda, 
                                   newdata = data_work, 
                                   type = "raw"), data_work$log_price)**(1/2)

lasso_predictions_holdout_rmse <- mse_lev(predict(lasso_model, s = lasso_model$bestTune$lambda, 
                                   newdata = data_holdout, 
                                   type = "raw"), data_holdout$log_price)**(1/2)
lasso_predictions_holdout_rmse

# CART model

# Training set
cart2_pred_train <- predict(cart2, data_work, na.action = na.pass)
cart2_rmse_train <- sqrt(mean((pred_cart2 - data_work$log_price)^2))

# Holdout set
cart2_pred_holdout <- predict(cart2, data_holdout, na.action = na.pass)
cart2_rmse_holdout <- sqrt(mean((pred_cart2 - data_holdout$log_price)^2))


# Random Forest

# Training set

rf_predicted_probabilities_train <- predict(rf_model_2, newdata = data_work, type = "raw")
data_work$rf_prediction <- rf_predicted_probabilities_train
rf_rmse_train <- RMSE(data_work$rf_prediction, data_work$log_price)

rf_predicted_probabilities_holdout <- predict(rf_model_2, newdata = data_holdout, type = "raw")
data_holdout$rf_prediction <- rf_predicted_probabilities_holdout
rf_rmse_holdout <- RMSE(data_holdout$rf_prediction, data_holdout$log_price)

# GBM

gbm_predicted_probabilities_train <- predict(gbm_model_2, newdata = data_work, type = "raw")
data_work$gbm_prediction <- gbm_predicted_probabilities_train
gbm_rmse_train <- RMSE(data_work$gbm_prediction, data_work$log_price)

gbm_predicted_probabilities_holdout <- predict(gbm_model_2, newdata = data_holdout, type = "raw")
data_holdout$gbm_prediction <- gbm_predicted_probabilities_holdout
gbm_rmse_holdout <- RMSE(data_holdout$gbm_prediction, data_holdout$log_price)

# Summary

rmse_compare <- data.frame(
  "Model" = c("Linear Model", "LASSO Model","CART Tree", "Random Forest" , "GBM"),
  "RMSE in Train" = c(model1_level_train_rmse, lasso_predictions_train_rmse, cart2_rmse_train, rf_rmse_train , gbm_rmse_train),
  "RMSE in Holdout" = c(model1_level_holdout_rmse, lasso_predictions_holdout_rmse, cart2_rmse_holdout, rf_rmse_holdout, gbm_rmse_holdout)
)

rmse_compare


# Predictions -------------------------------------------------------------

# Run the prediction on the whole dataset

gbm_predicted_probabilities <- predict(gbm_model_2, newdata = data, type = "raw")
data$gbm_prediction <- gbm_predicted_probabilities

# Calculate residuals

data$gbm_prediction_res <- data$log_price - data$gbm_prediction 

# Check the ones with the smallest (negative) residuals 

best_deals <- data %>% top_n( -5 , gbm_prediction_res ) %>% 
  select( hotel_id , price, log_price , gbm_prediction , gbm_prediction_res )

data$bestdeals <- ifelse(data$gbm_prediction_res %in% tail(sort(data$gbm_prediction_res, decreasing=TRUE),5),TRUE,FALSE)


# Graph the predictions vs the actual numbers

data %>% 
  ggplot() +
  geom_point(aes(x = gbm_prediction, y = log_price)) +
  geom_line( aes( x = log_price , y = log_price ) , color = "darkgreen", size = 1.2) +
  geom_point(aes(x = gbm_prediction, y = log_price, color=bestdeals, shape=bestdeals), size = 1.2, fill= "deeppink", alpha = 0.8, show.legend=F, na.rm = TRUE) +
  scale_shape_manual(name='',values=c(16,21)) +
  scale_colour_manual(name='',values=c("azure3", "black")) +
  labs( x = "Predicted prices (log)", y = "Actual prices (log)")

# Graph using actual prices


data %>% 
  ggplot() +
  geom_point(aes(x = exp(gbm_prediction), y = price)) +
  geom_line( aes( x = price , y = price ) , color = "darkgreen", size = 1.2) +
  geom_point(aes(x = exp(gbm_prediction), y = price, color=bestdeals, shape=bestdeals), size = 1.2, fill= "deeppink", alpha = 0.8, show.legend=F, na.rm = TRUE) +
  scale_shape_manual(name='',values=c(16,21)) +
  scale_colour_manual(name='',values=c("azure3", "black")) +
  labs( x = "Predicted prices", y = "Actual prices") +
  xlim(0,500) +
  ylim(0,500)

