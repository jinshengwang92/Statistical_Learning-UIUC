setwd("~/Documents/Classes_taken/STAT542/Rcode/WalmartSaleForcasting/jifu/")
options(warn=-1)

# import required library
if (!require(lubridate)) {
    install.packages('lubridate')
}
if (!require(forecast)) {
    install.packages('forecast')
}

library(lubridate)  # convert date information
library(forecast)  # make forecast

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
train.week = train.week - train.week[1]  # date is now 0, 7, 14, ...
train.week = train.week / 7 + 5  # make 2010-02-05 as '5'
train.week = as.numeric(train.week) %% 52  ## 52 weeks in a year
train$week = train.week

test.week = test$Date
test.week = test.week - test.week[1]
test.week = test.week / 7 + 9 # make 2011-03-04 as '9'.
test.week = as.numeric(test.week) %% 52
test$week = test.week

# output the head of training and test set
head(train)
head(test)

start.time = Sys.time()  # get time information

for (t in 1:2) {
    month = 2 + t
    year = 2011
    if (month > 12) {
        month = month - 12
        year = 2011 + 1
    }
    
    # get the tmp test data
    tmp.test = test[(test$year == year) & (test$month == month), ]
    
    # print useful information
    cat('Current date:\t', year, month, nrow(tmp.test), nrow(train), '\n')
    
    # get the length of unique store and department
    store = sort(unique(tmp.test$Store))
    n.store = length(store)
    dept = sort(unique(tmp.test$Dept))
    n.dept = length(dept)
    
    # choose the median value from the last year, in week - 1, week, and week + 1
    for (s in 1:n.store){
        for (d in 1:n.dept){
            # find the data for (store, dept) = (s, d)
            test.id = which(test$Store == store[s] & test$Dept == dept[d] &
                            test$year == year & test$month == month)
            test.temp = test[test.id, ]
            train.id = which(train$Store == store[s] & train$Dept == dept[d])
            train.temp = train[train.id, ]

            for (i in 1:length(test.id)){
                id = which(train.temp$week == test.temp[i,]$week & 
                           train.temp$year == test.temp[i,]$year - 1)
                # three weeks in the last year
                threeWeeksId = c(id - 1, id, id + 1)  
                tempSales = train.temp[threeWeeksId, 'Weekly_Sales']
                
                if (length(tempSales) == 0){
                    test$Weekly_Pred1[test.id[i]] = 0
                }else{
                    test$Weekly_Pred1[test.id[i]] = median(tempSales)
                }
            }
        }
    }
    
    # read new input file
    tmp.filename = paste('xxx', t, '.csv', sep='');
    newtest = read.csv(tmp.filename)
    
    # merge together
    train = rbind(train[, names(newtest)], newtest)
    
    # transform the date
    train$Date = as.Date(train$Date, '%Y-%m-%d')

    # get the year and month information
    train$year = year(train$Date)
    train$month = month(train$Date)

    # get week information
    train.week = train$Date
    train.week = train.week - train.week[1]  # date is now 0, 7, 14, ...
    train.week = train.week / 7 + 5  # make 2010-02-05 as '5'
    train.week = as.numeric(train.week) %% 52  ## 52 weeks in a year
    train$week = train.week
}

end.time = Sys.time()
cat(end.time - start.time)
head(train)

# # get the length of unique store and department
# store = sort(unique(test$Store))
# n.store = length(store)
# dept = sort(unique(test$Dept))
# n.dept = length(dept)

# # choose the median value from the last year, in week - 1, week, and week + 1
# for (s in 1:n.store){
# #     cat("Store: ", store[s], "\n")
#     for (d in 1:n.dept){
#         # find the data for (store, dept) = (s, d)
#         test.id = which(test$Store == store[s] & test$Dept == dept[d])
#         test.temp = test[test.id, ]
#         train.id = which(train$Store == store[s] & train$Dept == dept[d])
#         train.temp = train[train.id, ]
        
#         for (i in 1:length(test.id)){
#             id = which(train.temp$week == test.temp[i,]$week & train.temp$year == test.temp[i,]$year - 1)
#             threeWeeksId = c(id - 1, id, id + 1)  ## three weeks in the last year
#             tempSales = train.temp[threeWeeksId, 'Weekly_Sales']
#             if (length(tempSales) == 0){
#                 test$Weekly_Pred1[test.id[i]] = 0
#             }else{
#                 test$Weekly_Pred1[test.id[i]] = median(tempSales)
#             }
#         }
#     }
# }

# # get the length of unique department
# dept = sort(unique(test$Dept))
# n.dept = length(dept)

# # choose the median value from the last year, in week - 1, week, and week + 1
# for (d in 1:n.dept){
# #     cat("Department: ", dept[d], "\n")
#     # find the data for (store, dept) = (s, d)
#     test.id = which(test$Dept == dept[d])
#     test.temp = test[test.id, ]
#     train.id = which(train$Dept == dept[d])
#     train.temp = train[train.id, ]

#     for (i in 1:length(test.id)){
#         id = which(train.temp$week == test.temp[i,]$week & train.temp$year < test.temp[i,]$year)
#         tempSales = train.temp[id, 'Weekly_Sales']
#         if (length(tempSales) == 0){
#             test$Weekly_Pred2[test.id[i]] = 0
#         }else{
#             test$Weekly_Pred2[test.id[i]] = median(tempSales)
#         }
#     }
# }





# define weight w
weight = 4 * test$IsHoliday + 1

# calculate the performance of different models
WMAE1 = sum(weight * abs(test$Weekly_Pred1 - test$Weekly_Sales)) / sum(weight)
WMAE2 = sum(weight * abs(test$Weekly_Pred2 - test$Weekly_Sales)) / sum(weight)
WMAE3 = sum(weight * abs(test$Weekly_Pred3 - test$Weekly_Sales)) / sum(weight)

# output the performance of different models
cat(WMAE1, '\t', WMAE2, '\t', WMAE3, '\n')


