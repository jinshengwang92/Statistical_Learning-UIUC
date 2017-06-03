setwd("~/Documents/Classes_taken/STAT542/Rcode/WalmartSaleForcasting/jifu/")

options(warn=-1)

# import required library
if (!require(lubridate)) {
  install.packages('lubridate')
}
if (!require(forecast)) {
  install.packages('forecast')
}
if (!require(tseries)) {
  install.packages('tseries')
}

library(lubridate)  # convert date information
library(forecast)  # make forecast
library(tseries)

# load the data
train = read.csv('./train.csv')
test = read.csv('./test.csv')

# transform the date
train$Date = as.Date(train$Date, '%Y-%m-%d')
test$Date = as.Date(test$Date, '%Y-%m-%d')

# get the year and month information
train$year = year(train$Date)
test$year = year(test$Date)

train$month = month(train$Date)
test$month = month(test$Date)

# get week information
train.week = train$Date
start_date = train.week[1]
train.week = train.week - start_date  # date is now 0, 7, 14, ...
train.week = train.week / 7 + 5  # make 2010-02-05 as '5'
train.week = as.numeric(train.week) %% 52  ## 52 weeks in a year
train$week = train.week

test.week = test$Date
test.week = test.week - test.week[1]
test.week = test.week / 7 + 9 # make 2011-03-04 as '9'.
test.week = as.numeric(test.week) %% 52
test$week = test.week

start.time = Sys.time()  # get time information

for (t in 1:20) {
  month = 2 + t
  year = 2011
  if (month > 12) {
    month = month - 12
    year = 2011 + 1
  }
  
  # get the tmp test data
  tmp.test = test[(test$year == year) & (test$month == month), ]
  
  # print useful information
  cat('Current t is:\t', t, year, month, nrow(tmp.test), nrow(train), '\n')
  time1 = Sys.time()
  cat('Used time is:\t', t, time1 - start.time, '\n')
  
  # get the length of unique store and department
  store = sort(unique(tmp.test$Store))
  n.store = length(store)
  dept = sort(unique(tmp.test$Dept))
  n.dept = length(dept)
  
  # choose the median value from the last year, in week - 1, week, and week + 1
  for (s in store){
    for (d in dept){
      test.id = which(test$Store == s & test$Dept == d &
                        test$year == year & test$month == month)
      test.temp = test[test.id, ]
      ll = length(test.id)
      train.id = which(train$Store == s & train$Dept == d)
      if(length(test.id)==0 | length(train.id)==0){
        test$Weekly_Pred2[test.id] = 0
      }
      else{
        # # train must be one year earlier
        cat(s, '\t', d, '\n')
        train.temp = train[train.id, 'Weekly_Sales']
        
        data_ts = ts(train.temp,frequency = 52)
        if(length(train.id)>70){
          data_ts = stlf(data_ts,h=ll,s.window = 3,method='arima',ic='bic')
          pred = as.numeric(data_ts$mean)
        }else{
        pred = forecast(auto.arima(data_ts,stationary=FALSE,
                        allowmean=TRUE,seasonal=FALSE,
                        ic='bic',stepwise=TRUE),robust=TRUE)
        pred = fitted(pred)[1:ll]
        }
        test$Weekly_Pred2[test.id]=pred
      }
      
      
    }
  }
  
  # read new input file
  tmp.filename = paste('xxx', t, '.csv', sep='');
  newtest = read.csv(tmp.filename)
  
  # transform the date
  newtest$Date = as.Date(newtest$Date, '%Y-%m-%d')
  
  # get the year and month information
  newtest$year = year(newtest$Date)
  newtest$month = month(newtest$Date)
  
  # process the date
  tmp.week = newtest$Date
  tmp.week = tmp.week - start_date
  tmp.week = tmp.week / 7 + 5  # make 2010-02-05 as '5'
  tmp.week = as.numeric(tmp.week) %% 52
  newtest$week = tmp.week
  
  # merge together
  train = rbind(train, newtest[, names(train)])
}

end.time = Sys.time()
cat('Total used time is:\t', end.time - start.time, 'min\n')

# define weight w
weight = 4 * test$IsHoliday + 1
test[is.na(test$Weekly_Pred2),'Weekly_Pred2'] = 0.0   # set NA value to 0.0 
# calculate the performance of different models
WMAE1 = sum(weight * abs(test$Weekly_Pred1 - test$Weekly_Sales)) / sum(weight)
WMAE2 = sum(weight * abs(test$Weekly_Pred2 - test$Weekly_Sales)) / sum(weight)
WMAE3 = sum(weight * abs(test$Weekly_Pred3 - test$Weekly_Sales)) / sum(weight)

# output the performance of different models
cat('Model performace:\n')
cat(WMAE1, '\t', WMAE2, '\t', WMAE3, '\n')