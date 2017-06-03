# To use these models, you need to load the corresponding packages:
library(randomForest)  # random forest
library(gbm)  # gbm
library(xgboost)

# Let’s load the data.
flag = read.csv(file = 'https://archive.ics.uci.edu/ml/machine-learning-databases/flags/flag.data',
                header = FALSE)

head(flag[,10:28])

#str(flag)  # check what the data looks like, e.g. dimension, variables set. 
flag = flag[, c(2:17, 19:28)]  # take the numerical values
flag.y = as.numeric(flag$V13)  # Response Y: 0 if blue absent, 1 if blue present in the flag
head(flag)
flag.x = flag[, -12]  # Predictors X: the predictors should not contain v13
#head(flag.x)

#Then seperate data into trainning and test data set. We can record the row ID of the test data.
testidx = which((1:length(flag[,1])) %% 5 == 1)  # about 1/5 are test data.
length(testidx)

# Random Forest
cat.names = paste('V', c(2, 3, 6, 7), sep = '')  # names of the categorical features
cat.names
flag.rf = flag
# convert the categorical features to factors. Originally, they are integers.
flag.rf[, cat.names] = lapply(flag[, cat.names], as.factor)
head(flag.rf[, cat.names])
rfModel = randomForest(as.factor(V13) ~ ., data = flag.rf[-testidx, ],
                       importance = T, ntree = 500)
yhat = predict(rfModel, flag.rf[testidx, ])
table(yhat, flag.rf[testidx, 'V13'])
mean(yhat != flag.rf[testidx, 'V13'])

#GBM
gbm1 = gbm(V13 ~ ., data = flag.rf[-testidx, ], distribution = "adaboost",
           n.trees = 1000, shrinkage = 1, bag.fraction = 1, cv.folds = 5)
gbm.perf(gbm1, method = "cv")  # Estimates the optimal number of boosting iterations
yhat = predict(gbm1, flag.rf[testidx, ], type = 'response')  # returns probability
yhat = ifelse(yhat > 0.5, 1, 0)
table(yhat, flag.rf[testidx, 'V13'])
mean(yhat != flag.rf[testidx, 'V13'])


# Try different parameters: smaller shrinkage (lower learning rate)
gbm2 = gbm(V13 ~ ., data = flag.rf[-testidx, ], distribution = "adaboost",
           n.trees = 1000, shrinkage = 0.05, bag.fraction = 1, cv.folds = 5)
gbm.perf(gbm2, method = "cv")
yhat = predict(gbm2, flag.rf[testidx, ], type = 'response')  # probability
yhat = ifelse(yhat > 0.5, 1, 0)
table(yhat, flag.rf[testidx, 'V13'])
mean(yhat != flag.rf[testidx, 'V13'])

#XGBoost
#Now that we have four categorical predictors. XGBoost can only take numerical features. 
# If you feed a categorical feature to XGBoost, it will just code it as numerical with 1, 2, etc. 
# Let’s check this approach.

flag.x = apply(flag.x, 2, as.numeric)  # convert all predictors to numeric
# split to training and test sets
train.x = flag.x[-testidx,]
train.y = flag.y[-testidx]
test.x = flag.x[testidx,]
test.y = flag.y[testidx]

#We use 5-fold CV to select best number of iterations:

# ?xgb.cv  # open help file for the CV function.
set.seed(100)
cv.res = xgb.cv(data = train.x, label = train.y, 
                nfold = 5, nrounds = 15, max.depth = 2, eta = 1,
                objective = "binary:logistic")
# Set nrounds = 7

#Then fit the model. You can set parameters for “xgboost” function, and the help file is 
# here: http://xgboost.readthedocs.io/en/latest/parameter.html

bst = xgboost(data = train.x, label = train.y, max.depth = 2, eta = 1, 
              nround = 7 , objective = "binary:logistic")
yhat = predict(bst, test.x)  # predictions are probabilities.
yhat = as.numeric(yhat > 0.5)  # hard thresholding: 0 or 1
mean(yhat != test.y)


str(flag)
class(flag)

# The above model treats the categorical features as continuous. A better approach might be to 
# create dummy variables for the categorical variables, which enlarges pp and increases the computing time. 
# Les’s check what the prediction accuracy is by taking this approach.
cat.names = paste('V', c(2, 3, 6, 7), sep = '')  # names of the categorical features, used to subset X
flag.dum.x = data.frame(flag)  # convert to data frame before lapply the as.factor function
flag.dum.x[, cat.names] = lapply(flag.dum.x[, cat.names], as.factor)
head(flag.dum.x[, cat.names])
# ?model.matrix
# model.matrix creates a design (or model) matrix, e.g., by expanding factors to a set of dummary variables 
# (depending on the contrasts) and expanding interactions similarly.
dum.x = model.matrix(V13 ~.-1, data = flag.dum.x)   # Here -1 means no intercept, while no -1 means with intercept
head(dum.x)
names(dum.x)
train.dum.x = dum.x[-testidx, ]
test.dum.x = dum.x[testidx, ]

# train the model
set.seed(100)
cv.dum = xgb.cv(data =train.dum.x, label = train.y, nfold = 5, nrounds = 10,
                max.depth = 2, eta = 1, objective = "binary:logistic")
# set nround = 7
bst.dum = xgboost(data = train.dum.x, label = train.y, max.depth = 2, 
                  eta = 1, nround = 7, objective = "binary:logistic")
yhat = predict(bst.dum, test.dum.x)  # predictions are probabilities.
yhat = as.numeric(yhat > 0.5)  # hard thresholding: 0 or 1
mean(yhat != test.y)

names(dum.x)
dim(dum.x)
dim(flag.dum.x)
head(dum.x[,1:10])
head(dum.x[,11:20])
head(dum.x[,21:30])
head(dum.x[,31:40])
head(dum.x[,41:46])




