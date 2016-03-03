library(plyr)
library(stringr)
library(reshape2)
library(data.table)
library(gtools)
library(psych)
library(GPArotation)
library(gmodels)

setwd("/Users/williamhetherington/Desktop/Machine Learning R")


list.files()

###opening the sheet ####
teens <- data.frame(fread("snsdata.csv"))
str(teens)

## checks for missing values ###
table(teens$gender,useNA = 'ifany')
#     F     M  <NA> 
# 22054  5222  2724 

### checking for nas in age ###
summary(teens$age)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   3.086  16.310  17.290  17.990  18.260 106.900    5086 

### the min and max look weird - recall we are supposed to be talking
# about uni students here.  also over 5k nas ##
CrossTable(x=teens$age,y=teens$gender)

hist(teens$age)

### lots of weird values but as a % of the total sample not much###
## going to set the weird values to NA###

teens$age[teens$age<13|teens$age>20] <- NA

### creating the dummy variables to include 'no_gender' as a gender category ##
teens$female <- ifelse(teens$gender=='F' & !is.na(teens$gender),1,0)
teens$no_gender <- ifelse(is.na(teens$gender),1,0)

### imputing the age variable, but how to do this?###do we ahve a variable that is a good predictor?
### use average age of the grad year ###
ave_age <- ave(teens$age, teens$gradyear, FUN =
                  function(x) mean(x, na.rm = TRUE))

test <- cbind(teens,ave_age)
test <- test[,c(1:3,43,4:42)]

teens$age[is.na(teens$age)] <- ave_age

### creating a cluster solution with just the numeric vars ###
interests <- teens[,5:40]

## scaling/standardising the dataset ###
interests_z <- as.data.frame(lapply(interests,scale))

### running the kmeans ###
set.seed(2345)  ### sets the starting point of the random gen in R
teen_clusters <- kmeans(interests_z, 5)

teens$cluster <- teen_clusters$cluster

CrossTable(x=teens$cluster,y=teens$female)

chisq.test(table(teens$female,teens$cluster))







  