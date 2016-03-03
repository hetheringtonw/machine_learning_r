library(plyr)
library(stringr)
library(reshape2)
library(data.table)
library(gtools)
library(psych)
library(GPArotation)
library(dplyr)
library(C50)
library(gmodels)


  setwd("/Users/williamhetherington/Desktop/Machine Learning R")
list.files()

credit <- read.csv('credit.csv')
credit$default <- as.factor(credit$default)

### lets take a random sample from the data set to be our training set###
set.seed(123)
train_sample <- sample(1000, 900)

credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]


### check that there is about 30% defaults in both groups ####
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))
##yep

### running the c5.0 decision tree ###
### need to exclude the default (dependent variable) from the training set but supply it as the 
# output vector
credit_model <- C5.0(credit_train[,-21], credit_train$default)

### now checking on the test data set ###
credit_pred <- predict(credit_model, credit_test)

CrossTable(credit_test$default, credit_pred,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual default', 'predicted default'))

credit_boost10 <- C5.0(credit_train[-21], credit_train$default,
                         trials = 10)

boost_pred <- predict(credit_boost10,credit_test)

CrossTable(credit_test$default, boost_pred,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual default', 'predicted default'))


