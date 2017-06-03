# split into train and test
setwd("~/Documents/Classes_taken/STAT542/Projects_submission/")
train1 = read.csv('./train.csv')
test_id = sample(1:nrow(train1), 0.25*nrow(train1))   # 25% test data
# Split data
cols  = ncol(train1)
test_tem = train1[test_id,]
test = test_tem[,-cols]
train = train1[-test_id,]
true_y = log(test_tem[,cols]+1)  # after log



# import required library
if (!require(dummies)) {
    install.packages("dummies")
}
if (!require(DAAG)) {
    install.packages("DAAG")
}
if (!require(xgboost)) {
    install.packages("xgboost")
}
if (!require(randomForest)) {
    install.packages("randomForest")
}
if (!require(gbm)) {
    install.packages("gbm")
}
library(xgboost)
library(randomForest)
library(gbm)
library(dummies)  # dummy variable
library(moments)  # skewness
library(corrplot)  # corrplot
library(DAAG)  # cross-validation
library(glmnet)    # ridge and lasso

# load the data set
#train = read.csv('./train.csv')
#test = read.csv('./test.csv')

start.time = Sys.time()
#==========================  Here starts the data processing  ==============================

# drop column of "LotFrontage", "Alley", "FireplaceQu", "PoolQC", "Fence", "MiscFeature"
drop.names = c("LotFrontage", "Alley", "FireplaceQu", "PoolQC", "Fence", "MiscFeature")
train = train[ , !(names(train) %in% drop.names)]
test = test[ , !(names(test) %in% drop.names)]

# find all categorical and numerical variables
data.type = sapply(train[ , -c(1, ncol(train))], class)
categorical.var = names(train)[which(c(NA, data.type, NA) == 'factor')]
numerical.var = names(train)[which(c(NA, data.type, NA) == 'integer')]

# create new feature named "NA" for categorical variables
for (i in categorical.var) {
    train[, i] = addNA(train[, i])
    test[, i] = addNA(test[, i])
}

# create new feature using the median value for numerical variables
for (i in numerical.var) {
    na.id = is.na(train[, i])
    if (!any(na.id)) {
        next
    }
    train[which(na.id), i] = median(train[, i], na.rm=TRUE)
}

# create new feature using the median value for numerical variables
for (i in numerical.var) {
    na.id = is.na(test[, i])
    if (!any(na.id)) {
        next
    }
    test[which(na.id), i] = median(train[, i], na.rm=TRUE)
}

# combine into one data frame
data = rbind(train[, -ncol(train)], test)

# transform numerical feature whose skewness is larger than 0.75
skewed.features = sapply(data[, numerical.var], skewness)
skewed.features = numerical.var[which(skewed.features > 0.75)]
for (i in skewed.features) {
    data[, i] = log(data[, i] + 1)
}

# find new categorical variable to create dummy variables
dummy.var = data.frame(dummy.data.frame(data[, categorical.var], sep='.'))
data = cbind(data, dummy.var)

# drop original categorical variables
data = data[ , !(names(data) %in% categorical.var)]

data.train = data[1:nrow(train), ]
data.test = data[(nrow(train) + 1):nrow(data), ]

data.train['SalePrice'] = train$SalePrice

# transform the response variable into log scale
data.train$SalePrice = log(data.train$SalePrice + 1)
#======================= This is the end of data preprocessing =======================
end.time = Sys.time()
time_pre_data = end.time - start.time
cat("Data processing time: ", time_pre_data)


start.time = Sys.time()
#=======================  linear regression with all variables  ======================
# first the cross-validation
model.cv = cv.lm(data.train, SalePrice ~ ., m=5, seed=29, printit=FALSE)
attr(model.cv, "ms")

# build model and make predictions
model = lm(SalePrice ~ ., data = data.train)
predict.test.y = predict(model, newdata=data.test)
predict.test.y = exp(predict.test.y) - 1

# make submission file
submission = read.csv('./result/sample_submission.csv')
#submission$SalePrice = predict.test.y
#write.table(submission, './result/LS_all.csv', row.names = FALSE, sep = ',')
MSE_LS = sum((log(predict.test.y+1) - true_y)^2)/nrow(test)
cat("MSE of LS model: ", MSE_LS)
#====================================================================================
end.time = Sys.time()
time_ls = end.time - start.time
cat("Linear regression time: ", time_ls)


start.time = Sys.time()
#======================   Lasso regression model   =================================
# build lasso regression
cv.lasso = cv.glmnet(as.matrix(data.train[, -c(1, ncol(data.train))]), 
                     data.train[, 'SalePrice'], nfolds=10)
lambda_lasso = cv.lasso$lambda.min   # this is the optimal lambda with minimal shrinkage

#lambda_lasso, alpha = 1 is lasso regression
lasso.fit = glmnet(as.matrix(data.train[,-c(1, ncol(data.train))]), 
                   data.train[, 'SalePrice'], alpha=1, lambda=lambda_lasso)
predict.test.y = predict(lasso.fit, s=lambda_lasso, newx=as.matrix(data.test[, -1]))
predict.test.y = exp(predict.test.y) - 1

#cv.lasso$cvm
submission = read.csv('./result/sample_submission.csv')
#submission$SalePrice = predict.test.y
#write.table(submission, './result/lasso.csv', row.names = FALSE, sep = ',')
MSE_Lasso = sum((log(predict.test.y+1) - true_y)^2)/nrow(test)
cat("MSE of LASSO model: ", MSE_Lasso)
#==================================================================================
end.time = Sys.time()
time_lasso = end.time - start.time
cat("Lasso regression time: ", time_lasso)



start.time = Sys.time()
#======================   Random Forest regression model   =================================
# build random forest regression
# build the model
model = randomForest(SalePrice ~ ., data=data.train, importance=T, ntree=500)
predict.train.y = predict(model, data.train)
train.mse = sum((predict.train.y - data.train$SalePrice) ^ 2) / length(predict.train.y)

predict.test.y = predict(model, data.test)
predict.test.y = exp(predict.test.y) - 1

# make submission file
submission = read.csv('./result/sample_submission.csv')
#submission$SalePrice = predict.test.y
#write.table(submission, './result/randomForest.csv', row.names = FALSE, sep = ',')
MSE_rf = sum((log(predict.test.y+1) - true_y)^2)/nrow(test)
cat("MSE of Random forest model: ", MSE_rf)
#==================================================================================
end.time = Sys.time()
time_rf = end.time - start.time
cat("Radom forest regression time: ", time_rf)


start.time = Sys.time()
#===========================  XG boost model   =====================================
# get x and y variables
train.y = as.numeric(data.train$SalePrice)
train.x = data.train[, c(-1, -289)]
train.x = apply(train.x, 2, as.numeric)

test.x = data.test[, -1]
test.x = apply(test.x, 2, as.numeric)

# cross-validation
set.seed(100)
cv.dum = xgb.cv(data=train.x, label=train.y, nfold=5, max.depth=2, 
                eta=1, nround=100, objective='reg:linear')

# build model and make predictions
bst.dum = xgboost(data=train.x, label=train.y, max.depth=2, eta=1, 
                  nround=50, objective='reg:linear', verbose=FALSE)
predict.test.y = predict(bst.dum, test.x)
predict.test.y = exp(predict.test.y) - 1

# make submission file
submission = read.csv('./result/sample_submission.csv')
#submission$SalePrice = predict.test.y
#write.table(submission, './result/xgboost.csv', row.names = FALSE, sep = ',')
MSE_xgboost = sum((log(predict.test.y+1) - true_y)^2)/nrow(test)
cat("MSE of XGboost model: ", MSE_xgboost)
#=====================================================================================
end.time = Sys.time()
time_xg = end.time - start.time
cat("XGboost regression time: ", time_xg)

# End of this R file
#