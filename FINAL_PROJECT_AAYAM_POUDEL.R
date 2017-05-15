#Aayam Poudel
#Final Project

#Load Neccessary Packages
library(tidyverse)
library(broom)
#sexy themes 
library(ggthemes)

# Id - an Id that represents a (Store, Date) duple within the test set
# Store - a unique Id for each store
# Sales - the turnover for any given day (this is what you are predicting)
# Customers - the number of customers on a given day
# Open - an indicator for whether the store was open: 0 = closed, 1 = open
# StateHoliday - indicates a state holiday. Normally all stores, with few exceptions, are closed on state holidays. Note that all schools are closed on public holidays and weekends. a = public holiday, b = Easter holiday, c = Christmas, 0 = None
# SchoolHoliday - indicates if the (Store, Date) was affected by the closure of public schools
# StoreType - differentiates between 4 different store models: a, b, c, d
# Assortment - describes an assortment level: a = basic, b = extra, c = extended
# CompetitionDistance - distance in meters to the nearest competitor store
# CompetitionOpenSince[Month/Year] - gives the approximate year and month of the time the nearest competitor was opened
# Promo - indicates whether a store is running a promo on that day
# Promo2 - Promo2 is a continuing and consecutive promotion for some stores: 0 = store is not participating, 1 = store is participating
# Promo2Since[Year/Week] - describes the year and calendar week when the store started participating in Promo2
# PromoInterval - describes the consecutive intervals Promo2 is started, naming the months the promotion is started anew. E.g. "Feb,May,Aug,Nov" means each round starts in February, May, August, November of any given year for that store

#The working directory. Make sure to modify it when you run your code
setwd("~/Desktop/Junior Spring/Statistical Learning/Homework/Final Project/")

#David's working directory run
setwd("~/Stat_Learning_Final/")

#First let's import some important info about the stores
store <- read_csv("store.csv")

#Read the train data
train <- read_csv("train.csv") 

#Read the test data
test <- read_csv("test.csv")

#since the store info is relevant to both train and test datasets let's join them to the datasets
train <- left_join(train,store, by="Store")
test <- left_join(test,store,by="Store")


#Since the dataset is too big, I couldn't do proper conputations so I am going to select a subset
train <- train %>%
  sample_frac(0.1)

#Read the sample submissions
sample_submissions <- read_csv("sample_submission.csv")


#1. Exploratory data analysis:

#1.a. EDA on Customers
#The first way of doing exploratory data analysis is looking at the data. Just looking at a dataset might not give
#us too much information about the data, but it provides us with some information.
View(train)

#Maybe we should investigate and see if there is an relationship between the day of the week and the number of sales!

#Lets manipulate our data set and sum it up

group_by_day <- train %>%
  group_by(DayOfWeek) %>%
  summarise(TotalSales = mean(Sales))


# Lets take a look at the mean of the sales by day of week!
dayofweek_sales <- ggplot(group_by_day, aes(y=TotalSales, x=DayOfWeek)) +
  geom_bar(stat="identity") +
  xlab("Day of Week") + 
  ylab("Total Number of Sales") + 
  ggtitle("Mean Number of Sales by Customers") + theme_economist()


# Interesting, maybe it would be useful to investigate the days of the week further...
# Lets look at the total sum of sales by day of week!
group_by_day_sum <- train %>%
  group_by(DayOfWeek) %>%
  summarise(TotalSales = sum(Sales))

dayofweek_sum_sales <- ggplot(group_by_day_sum, aes(y=TotalSales, x=DayOfWeek)) +
  geom_bar(stat="identity") +
  xlab("Day of Week") + 
  ylab("Total Number of Sales") + 
  ggtitle("Total Number of Sales by Customers") + theme_economist()



#It seems like there is definitey a correlation between Sales and Customer. This is valuable information.

#1.b. EDA on Promo
#Let's look at the correlation between Sales and whether there was a promo event or not.
promo_vs_sales <- ggplot(train, aes(x=Promo, y=Sales, group=Promo)) +
  geom_boxplot() 
promo_vs_sales

#It's a little hard to understand. Log scale to the rescue!
promo_vs_sales <- ggplot(train, aes(x=Promo, y=Sales, group=Promo)) +
  geom_boxplot() +
  scale_y_log10() +
  ylab("Sales") + 
  xlab("Promotion VS Non-Promotion") + theme_economist()

promo_vs_sales

#Seems like there is some information that promo provides about Sales! 

#1.c. EDA on School Holiday

#Let's look at the correlation between Sales and type of School Holiday
school_holiday_vs_sales <- ggplot(train, aes(x=SchoolHoliday, y=Sales, group=SchoolHoliday)) +
  geom_boxplot() + theme_economist()
  
school_holiday_vs_sales


#1.d EDA on Competitive Stores
comp_stores <- ggplot(train, aes(x=CompetitionDistance, y=Sales)) +
  geom_point() +
  geom_smooth()
comp_stores
#DO MORE EDA



#Before doing anything else I want to fit a regression model just based on a bunch of predictors and see
#if I can get it submitted on Kaggle properly!
model_SL <- lm(Sales~Store+Promo+StateHoliday+SchoolHoliday+DayOfWeek, data=train)

fitted_values <- augment(model_SL, newdata = test)

submission_example <- fitted_values %>%
  mutate (Sales = .fitted) %>%
  select(Id, Sales)

submission_example %>% 
  readr::write_csv("submission_example.csv")


#It works, so now we can go ahead and fit some models and do cross validation on them





