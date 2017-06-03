#-------------------------------------------------------------------------
#     This is the code for Walmart sales prediction
#     provided by STAT542 TA
#     Data from Kaggle website
#     Feb 2nd 2017
#-------------------------------------------------------------------------

# set working space
setwd("~/Documents/Classes_taken/STAT542/Rcode/WalmartSaleForcasting/")

# load the data
#train = read.csv(unz('train.csv.zip', 'train.csv'))
#test = read.csv(unz('test.csv.zip', 'test.csv'))

## If you have already unzipped the files, 
train=read.csv("train.csv")
test=read.csv("test.csv")

# Check the structure of the data:
#str(train)
#str(test)

startt = Sys.time()
startt
## How many depts?  81 Depts, from 1 to 99 but with missing values
#length(unique(train$Dept))
#sort(unique(train$Dept))
## How many stores? 45, from 1 to 45
#length(unique(train$Store))
#sort(unique(train$Store))

dept_store_train = table(train$Dept,train$Store)
#dept_store_train
dept_store_test  = table(test$Dept, test$Store)
#dept_store_test
#dim(dept_store_train)
#dim(dept_store_test)

## 11 stores in the test do not have historical data
tmp=(dept_store_train ==0 )*(dept_store_test>0)
#tmp
#sum(tmp)

## Names for the 81 departments
dept.names = sort(unique(train$Dept))
#dept.names
missing_dept_store = which(tmp>0, arr.ind=TRUE, useNames = FALSE)
#missing_dept_store
# arr.ind stands for array index
#missing_dept_store[,1]
missing_dept_store[,1] = dept.names[missing_dept_store[,1]]
#missing_dept_store[,1]
#order(missing_dept_store[,1], missing_dept_store[,2])
missing_dept_store = missing_dept_store[order(missing_dept_store[,1], missing_dept_store[,2]),]
#missing_dept_store
# order the missing dept+store by stores and dept
#missing_dept_store


# According to the competition winner, the store and features are not useful in prediction. 
# In case you need them, you can use the following code to add store and feature to the data
stores = read.csv('stores.csv')
features = read.csv(unz('features.csv.zip', 'features.csv'))

# Merge train, store, and feature
# train = merge(x=train, y=store, all.x=TRUE)
# train = merge(x=train, y=feature, all.x=TRUE)
# test = merge(x=test, y=store, all.x=TRUE)
# test = merge(x=test, y=feature, all.x=TRUE)

# time format
train$Date = as.Date(train$Date, '%Y-%m-%d')
test$Date = as.Date(test$Date, '%Y-%m-%d')

#use the lubridate package to extract various date information, like year, month, etc, from the “Date” column.
library(lubridate)
train$Yr = year(train$Date)
test$Yr = year(test$Date)

train$Mon = month(train$Date)
test$Mon = month(test$Date)

#table(train$Yr, train$Mon)
#table(test$Yr, test$Mon)

# use the week number to do the prediction
train.wk = train$Date
train.wk = train.wk - train.wk[1]  # date is now 0, 7, 14, ...
train.wk = train.wk/7 + 5  # make 2010-2-5 as '5', and date becomes continuous integers, i.e., 5, 6, 7, ...
train.wk = as.numeric(train.wk) %% 52  ## 52 weeks in a year
train$Wk = train.wk

test.wk = test$Date
test.wk = test.wk - test.wk[1]
test.wk = test.wk/7 + 44 # make 2012-11-02 as '44'.
test.wk = as.numeric(test.wk) %% 52
test$Wk = test.wk

# Wk = 0, Christmas
# Wk = 6, Super Bowl
# Wk = 36, Labor Day
# Wk = 47, Thanksgiving

# note that the test data does not contain Labor day
#table(train$Wk[train$IsHoliday])
#table(test$Wk[test$IsHoliday])

# exploratory data analysis
# Next we plot weekly sales of 4 dpartments from the 1st department. 
# You can change the code to plot the sales of other departments and stores.
library(lattice)  # xyplot

d = 5  # first department
tempData = subset(train, Dept == d)
tempData = subset(tempData, Store %in% c(1, 2, 3, 4))
xyplot(Weekly_Sales ~ Wk | Store, tempData, main=paste("Dept No. ", d),
       par.strip.text=list(cex=1.0), pch="*",
       strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
       layout = c(4, 1)
)


# A simple model
## create a new column of predicted sales in the test set
test$Weekly_Sales = NA  

store = sort(unique(test$Store))
n.store = length(store)
dept = sort(unique(test$Dept))
n.dept = length(dept)

for (s in 1:n.store){
  for (d in 1:n.dept){
    
    #cat("Store: ", store[s], "\t Dept ", dept[d], "\n")
    
    # find the data for (store, dept) = (s, d)
    test.id = which(test$Store == store[s] & test$Dept == dept[d])
    test.temp = test[test.id, ]
    train.id = which(train$Store == store[s] & train$Dept == dept[d])
    train.temp = train[train.id, ]
    
    for (i in 1:length(test.id)){
      id = which(train.temp$Wk == test.temp[i,]$Wk & train.temp$Yr == test.temp[i,]$Yr - 1)
      threeWeeksId = c(id - 1, id, id + 1)  ## three weeks in the last year
      tempSales = train.temp[threeWeeksId, 'Weekly_Sales']
      if (length(tempSales) == 0){
        test$Weekly_Sales[test.id[i]] = 0
      }else{
        test$Weekly_Sales[test.id[i]] = median(tempSales)
      }
    }
  }
}

# still 5 NA's, for simplicity use 0.
test$Weekly_Sales[which(is.na(test$Weekly_Sales))] = 0

# Next, prepare the submission file:
submission = read.csv(unz('sampleSubmission.csv.zip', 'sampleSubmission.csv'))
submission$Weekly_Sales = test$Weekly_Sales
write.table(submission, file = 'simple_model.csv', sep = ',', row.names = FALSE)

endt = Sys.time()
endt
cat(" my running time: ", (endt - startt))

#---------------------------------------------------------------------------------
# This is the end of this code
#---------------------------------------------------------------------------------