
# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

library(tidyverse)
library(stargazer)
library(Hmisc)
library(skimr)

source("D:/CEU/Data_Analysis_3/da_case_studies/ch00-tech-prep/theme_bg.R")

#-------------------------------------------------------
# Import data
data <- read_csv("D:/CEU/Data_Analysis_3/DA3_Yuri/HW1/data/raw/buenos_aires_raw.csv")

# Keep if property type is Entire Apartment, Entire serviced apartment
# Selecting only the data determined as 
view(table(data$property_type))

# Check some specific type of property 
data %>%
  filter(property_type %in% c("Entire apartment", 	
                              "Entire serviced apartment")) %>% 
  select(name, property_type, room_type, accommodates, bathrooms_text, bedrooms, beds)
  
data <- data %>%
  filter(property_type %in% c("Entire apartment", "Entire serviced apartment"))

view(data$property_type)

# Rename room type because it is too long
data$room_type <- ifelse(data$room_type== "Entire home/apt", "Entire/Apt")

#---------------------------------------------------------------------------------------------------------------------------

## Create Numerical variables
data <- data %>%
  mutate(
    ars_price_day = as.numeric(gsub("[^0-9.]", "", price)),
    p_host_response_rate = parse_number(host_response_rate))

#-------------------------------------------------------------------

## Redefine bathrooms variable

view(data %>% 
        select(bathrooms,bathrooms_text)
)

data <- data  %>% 
  mutate(bathrooms = as.numeric(gsub("[a-zA-Z ]", "", data$bathrooms_text)))
data$bathrooms_text <- NULL

# add new numeric columns from certain columns
numericals <- c("accommodates", "bathrooms" ,"review_scores_rating","number_of_reviews",
                "reviews_per_month","minimum_nights","beds")
data <- data %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))

# rename columns so they start with n_ as opposed to end with _n
nnames <- data %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(data))
colnames(data)[nnames_i] <- paste0("n_", numericals)

#create days since first review
data <- data %>%
  mutate(
    n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%m/%d/%Y") -
                                as.Date(data$first_review,format="%m/%d/%Y")))

# split the amenities column 

data$amenities <-gsub("\\[","",data$amenities)
data$amenities <-gsub("\\]","",data$amenities)
data$amenities<-gsub('\\ "',"",data$amenities)
data$amenities<-gsub('\\"',"",data$amenities)
data$amenities<-as.list(strsplit(data$amenities, ","))


#define levels and dummies 
levs <- levels(factor(unlist(data$amenities)))
data <-cbind(data,as.data.frame(do.call(rbind, lapply(lapply(data$amenities, factor, levs), table))))
data$amenities <- NULL

#checking the column names
colnames(data)

# Write all columns name under a CSV file for categorization
write.csv(colnames(data), "D:/CEU/Data_Analysis_3/DA3_Yuri/HW1/out/categories_raw.csv")

# Import file with categories
category_data  <- read.csv("D:/CEU/Data_Analysis_3/DA3_Yuri/HW1/out/categories_set.csv" , header =  TRUE)

# Categories
eliminate_columns <- as.vector(t(category_data %>% 
                      filter(Category == "Delete") %>% 
                      select(Name)))

category_data <- category_data[!category_data$Category == "Delete",]

data <- data %>% 
  select(-eliminate_columns)

# Aggregate Dummy Variables -> Transform them into bigger categories

for (i in unique(category_data$Category)){
  
  values <- as.vector(t(category_data %>% 
                          filter(Category == i) %>% 
                          select(Name)))
  
 
  data[i] <- ifelse(rowSums(data[values]) == 0,0,1)  
  
}

# Remove old Amendities Columns 

data <- data %>% 
  select(-category_data$Name)

# create dummy vars
dummies <- names(data)[seq(83,111)]
data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))

# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))

# rename and transform instant_bookable column
data <- data %>% 
  mutate(d_instant_bookable = ifelse(data$instant_bookable == TRUE, 1, 0))

# transform into factors
data <- data %>% 
mutate(
  f_neighbourhood_cleansed = factor(neighbourhood_cleansed))

data <- data %>% 
  mutate( 
    f_property_type = factor(property_type))

# keep columns if contain d_, n_,f_, p_, usd_ and some others (instant_bookable = business decision may affect the price)
data <- data %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^ars_.*"), price, id)

# with price info only
data <- data %>%
  drop_na(price)

# filter n_accomodates: 2 to 6 
data <- data %>% 
    filter(!n_accommodates == 1,
           !n_accommodates > 6)

# filter n_beds: (some intriguing values like "0")

data <- data %>% 
  filter(!n_beds == 0,
         !n_beds > 6)

# filter n_bathrooms: (some intriguing values like "0")

data <- data %>% 
  filter(!n_bathrooms == 0,
         !n_bathrooms > 4)

# filter n_minimum_nights: (some intriguing values like "1100")

data <- data %>% 
  filter(n_minimum_nights < 30)


##################################
# DESCRIBE


#####################
### look at price ###
#####################
summary(data$ars_price_day)
describe(data$ars_price_day)

data <- data %>%
  mutate(ln_price = log(ars_price_day))

R_F14_h_price <- ggplot(data, aes(ars_price_day)) +
  geom_histogram(binwidth = 500, fill = color[1], color = color.outline, alpha = 0.8) +
  ylab("count") +
  xlab("Price") +
  theme_bg()
R_F14_h_price

# Remove really extreme values  
outliers <- boxplot.stats(data$ars_price_day)$out

# Decided to do the cut under 7000 pesos to reflect better the goal of the analysis: small and mid-size apartments
data <- data %>% 
  filter( ars_price_day < 7000)

# Histograms
R_F14_h_lnprice <- ggplot(data, aes(ln_price)) +
  geom_histogram(binwidth = 0.15, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("Count") +
  xlab("Log price") +
  theme_bg()
R_F14_h_lnprice




################################################
# look at some cnts. key vars, functional form #
################################################

## n_accomodates: look at distribution

data %>%
  group_by(n_accommodates) %>%
  summarise(mean_price = mean(ars_price_day), min_price= min(ars_price_day), max_price = max(ars_price_day), n = n())

R_14_s_n_accommodates <- ggplot(data = data, aes(x=n_accommodates, y= ars_price_day)) +
  geom_point(size=1, colour=color[3], shape=16)+
  labs(x="Number of people accomodated",y="Price")+
  geom_smooth(method="lm", colour=color[1], se=FALSE)+
  theme_bg()

R_14_s_n_accommodates

####################### n_accomodates ###########################################

# Squares and further values to create
data <- data %>%
  mutate(n_accommodates2= n_accommodates^2, ln_accommodates=log(n_accommodates) ,
         ln_accommodates2=log(n_accommodates)^2)

# Regression 1: ln price and num of accomodates and squares
summary(lm(ln_price ~ n_accommodates + n_accommodates2, data=data))
# Regression 2: ln price and num of accomodates
summary(lm(ln_price ~ n_accommodates, data=data))

####################### n_beds ############################################

data %>%
  group_by(n_beds) %>%
  summarise(mean_price = mean(ars_price_day), min_price= min(ars_price_day), max_price = max(ars_price_day), n = n())
# maybe best is to have log beds
data <- data %>%
  mutate(ln_beds = log(n_beds))

# Regression 1: ln price and num of beds
summary(lm(ln_price ~ n_beds, data=data))
# Regression 2: ln price and num of beds
summary(lm(ln_price ~ ln_beds, data=data))

####################### n_bathrooms ###########################################

ggplot(data, aes(n_bathrooms)) +
  geom_histogram(binwidth = 0.5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of bathrooms") +
  theme_bg()

# Pool accomodations with 0,1,2,10 bathrooms

data <- data %>%
  mutate(f_bathroom = cut(n_bathrooms, c(0,1,2,3,10), labels=c(0,1,2,3), right = F))

data %>%
  group_by(f_bathroom) %>%
  summarise(mean_price = mean(ars_price_day), n = n())

# Regression 1: ln price and num of bathrooms
summary(lm(ln_price ~ f_bathroom, data=data))

####################### n_number_of_reviews ###########################################

nreview_plot <- data %>%
  filter(n_number_of_reviews < 100)

ggplot(nreview_plot, aes(n_number_of_reviews)) +
  geom_histogram(binwidth = 5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of reviews") +
  theme_bg()

# number of reviews: use logs as well
data <- data %>%
  mutate(ln_number_of_reviews = log(n_number_of_reviews+1))

ggplot(data, aes(ln_number_of_reviews)) +
  geom_histogram(binwidth = 0.5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("Log N of reviews") +
  theme_bg()

# Pool num of reviews to 3 categories: none, 1-51 and >51
data <- data %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(data$n_number_of_reviews)), labels=c(0,1,2), right = F))
data %>%
  group_by(f_number_of_reviews) %>%
  summarise(median_price = median(ars_price_day) ,mean_price = mean(ars_price_day) ,  n=n())

# Remove missing values: f_number_of_reviews
data <- data %>% 
  filter(!is.na(f_number_of_reviews))

# Regression 3: log-price and number of reviews
summary(lm(ln_price ~ f_number_of_reviews, data=data))
# Regression 4: log-price and log number of reviews
summary(lm(ln_price ~ ln_number_of_reviews, data=data))

####################### n_days_since ###########################################

# Create variables, measuring the time since: squared, cubic, logs

data <- data %>%
  mutate(
    ln_days_since = log(n_days_since),
    ln_days_since2 = log(n_days_since)^2,
    ln_days_since3 = log(n_days_since)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=n_days_since^3)

# Check the effect
lndays_plot <- data %>%
  filter(ln_days_since>2)

# Inf values
summary(lm(ln_price ~ ln_days_since, data=data)) # simpler / almost same r-square
summary(lm(ln_price ~ ln_days_since + ln_days_since2 , data=data))
summary(lm(ln_price ~ ln_days_since + ln_days_since2 + ln_days_since3, data=data))

####################### n_review_scores_rating ###########################################

## review score effect
ggplot(data = data, aes(x=n_review_scores_rating , y= ars_price_day)) +
  geom_point(size=1.5, colour=color[3], shape=4) +
  xlim(20,100)+
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Review score",y="Daily price (ARS)")+
  theme_bg()

# Create log of review scores 
data <- data %>%
  mutate(ln_review_scores_rating = log(n_review_scores_rating))

# Regression 5) ln price - num of review scores
summary(lm(ln_price ~ n_review_scores_rating,data=data)) 
# Regression 6) ln price - log num of review scores
summary(lm(ln_price ~ ln_review_scores_rating,data=data))
# Don't use it for OLS

####################### n_minimum_nights ###########################################

# Create log of number of minimum nights 
data <- data %>%
  mutate(ln_n_minimum_nights = log(n_minimum_nights))

## minimum nights
summary(lm(ln_price ~ n_minimum_nights,data=data)) # Better fit

## log of minimum nights
summary(lm(ln_price ~ ln_n_minimum_nights,data=data))

# Pool and categorize the number of minimum nights: 1,2,3, 3+

data <- data %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,3,max(data$n_minimum_nights)), labels=c(1,2,3), right = F))

summary(lm(ln_price ~ f_minimum_nights,data=data))

####################### n_reviews_per_month ###########################################

data <- data %>%
  mutate(ln_n_reviews_per_month = log(n_reviews_per_month))

summary(lm(ln_price ~ n_reviews_per_month,data=data)) #use this one
summary(lm(ln_price ~ ln_n_reviews_per_month,data=data))

####################### f_neighbourhood_cleansed ###########################################

summary(lm(ln_price ~ f_neighbourhood_cleansed ,data=data)) 

####################### f_property_type ###########################################

summary(lm(ln_price ~ f_property_type ,data=data))

###########################
## look at categoricals  ##
###########################

categoricals <- c("property_type")

for (i in 1:length(categoricals)) {
  data %>%
    group_by(get(categoricals[i])) %>%
    summarise(mean_price = mean(ars_price_day) ,  n=n()) %>%
    print
}

#####################################

# Change Infinite values with NaNs
for (j in 1:ncol(data)) data.table::set(data, which(is.infinite(data[[j]])), j, NA)

write_csv(data, "D:/CEU/Data_Analysis_3/DA3_Yuri/HW1/data/clean/buenos_aires_clean.csv")

#------------------------------------------------------------------------------------------------

