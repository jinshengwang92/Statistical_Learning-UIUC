library(moments)  # skewness
library(corrplot)  # corrplot

train = read.csv('./train.csv')
test = read.csv('./test.csv')

dim(train)  # dimension of the training data

str(train)  # structure of the training data

data.type = sapply(train[, -c(1, 81)], class)  # table the data type
table(data.type)
?c
dim(test)  # dimension of the test data

str(test)  # structure of the test data

# apply function: use the given function (is.na) for each column (use 2 for column, and use 1 for row) 
# in the matrix train[, -c(1, 81)].
numNA = colSums(apply(train[, -c(1, 81)], 2, is.na))
number_of_missing = numNA[which(numNA != 0)]  # number of NA's
data_type = sapply(train[,names(which(numNA != 0))], class)  # type of data
cbind(number_of_missing, data_type)

drops = c("Alley", "PoolQC", "Fence", "MiscFeature")
train = train[ , !(names(train) %in% drops)]
test = test[ , !(names(test) %in% drops)]

#For the rest of the categorical variables, we can complete the missing values by treating them as a new level.
data.type = sapply(train[, -c(1, ncol(train))], class)  # as we've removed 4 variables
cat_var = names(train)[which(c(NA, data.type, NA) == 'factor')]  # categorical variables
numeric_var =  names(train)[which(c(NA, data.type, NA) == 'integer')]  # continuous variables
for (j in cat_var){
  train[, j] = addNA(train[, j])  # addNA treat the NA's as a new level called '<NA>'
  test[, j] = addNA(test[, j])
}

tempVar = c('LotFrontage', 'MasVnrArea', 'GarageYrBlt')
for (j in tempVar){
  na.id = is.na(train[, j])  # binary indicator: NA (1) or not (0)
  tempMedian = median(train[, j], na.rm = TRUE)  # find the median
  train[which(na.id), j] = tempMedian
  train[, paste(j, 'NAInd', sep = '_')] = as.numeric(na.id)  # create a new column
}

for (j in numeric_var){
  na.id = is.na(test[, j])
  if (!any(na.id)){
    next
  }
  test[which(na.id), j] = median(train[, j])
}


# Exploratory Data Analysis
#The followings are the bar-plot of the categorical variables:
par(mfrow = c(2, 2))  # set the layout of the plots
for(j in 1:4){
  barplot(table(train[, cat_var[j]]), main = cat_var[j])
}

#The last bar in each plot is the number of NA levels.
#Histogram and density plots of numerical variables:
par(mfrow = c(2, 2))
for(j in 1:4){
  d = density(train[, numeric_var[j]])
  hist(train[, numeric_var[j]], main = numeric_var[j], xlab = '', prob = TRUE, ylim = c(0, max(d$y) * 1.1))
  lines(d)
}

# We can see that some of the numerical variables have extreme values, and are skewed to the right.
#Similarly, we plot the histogram and density estimation of the response variable, “SalePrice”. The data is also skewed to the right.
d = density(train[, 'SalePrice'])
hist(train[, 'SalePrice'], main = 'SalePrice', xlab = '', prob = TRUE, ylim = c(0, max(d$y) * 1.1))
lines(d)


# Before we move on, we take the log-transformation for the response and the numerical featuers, which is one common approach used 
# in price/counts variables, and can alleviate the skewness.
# transform SalePrice target to log form
train$SalePrice <- log(train$SalePrice + 1)

# for numeric feature with excessive skewness, perform log transformation
# determine skew for each numeric feature
skewed_feats = sapply(train[, numeric_var], skewness)
# only transform features that exceed a threshold = 0.75 for skewness
skewed_feats = numeric_var[which(skewed_feats > 0.75)]
for(j in skewed_feats) {
  train[, j] = log(train[, j] + 1)
  test[, j] = log(test[, j] + 1)
}

# Next, we explore the correlation of the response with the numerical features.
correlations = cor(train[, c(numeric_var, 'SalePrice')])  # correlation matrix
# for those relatively large correlations (> 0.3)
row_indic = apply(correlations, 1, function(x) sum(abs(x) > 0.3) > 1)
correlations = correlations[row_indic, row_indic]
corrplot(correlations, method = "square")

# record those with very large correlation (> 0.6)
highCor = which(abs(correlations[, ncol(correlations)]) > 0.3)
highCor = highCor[-length(highCor)]
names(highCor)

# Finally, we plot the variable versus SalePrice one by one:
par(mfrow = c(2,3))
for (j in 1:length(highCor)){
  plot(train[, names(highCor)[j]], train[, 'SalePrice'], xlab = names(highCor)[j], ylab = 'SalePrice')
  tempModel = lm(SalePrice ~ ., data = train[, c(names(highCor)[j], 'SalePrice')])
  abline(tempModel, col = 'blue')
  legend('topleft', legend = paste('cor = ', round(correlations[highCor[j], ncol(correlations)], 3)))
}

#  A Simple Linear Regression Model
# It seems that the five variales, OverallQual, X1stFlrSF, GrLivArea, GarageCars, GarageArea, has high 
# correlation with SalePrice. So we build a simple multiple regression with these variables:
mlr = lm(SalePrice ~ ., data = train[, c(names(highCor), 'SalePrice')])

# Then do prediction. Remember to convert the prediction to the original scale.
yHat = predict(mlr, newdata = test)
yHat = exp(yHat) - 1

#Finally, write the prediction to the submission file. You need to download the ‘sample_submission.csv’ file in order to use the following code.
submission = read.csv('./sample_submission.csv')
submission$SalePrice = yHat
write.table(submission, 'simple_model_temp.csv', row.names = FALSE, sep = ',')


