#########################
# Linear Regression for Boston Housing data
#########################

library(MASS)
data(Boston)
?Boston
dim(Boston)
names(Boston)

myData=Boston
names(myData)[14] = "Y"
iLog = c( 1,3,5,6,8,9,10,14);
myData[,iLog] = log( myData[,iLog] );
myData[,2] = myData[,2] / 10;
myData[,7] = myData[,7]^2.5 / 10^4
myData[,11] = exp( 0.4 * myData[,11] ) / 1000;
myData[,12] = myData[,12] / 100;
myData[,13] = sqrt( myData[,13] );

## run a quick summary of each column of myData
summary(myData)
#Produce a pair-wise scatter plot. Caution: a big figure.
pairs(myData, pch='.')
?pairs

#fit a linear model
lmfit = lm(Y ~., data=myData)

names(lmfit)  # What have been returned by "lm"?
lmfit$coef    # 12 regression cofficients including the intercept

summary(lmfit)
summary(lmfit)$coef  # coefficients and the corresponding p-values

# Understand the LS coefficient
summary(lm(Y~ age, myData))
round(cor(myData), dig=2)

#Rank Deficienc
## Add a "fake" column
myData$junk = myData$crim + myData$zn
tmp.lm = lm(Y~ ., myData)
summary(tmp.lm)

## The fitted values (for the first 3 obs) are the same. 
tmp.lm$fitted[1:3]
lmfit$fitted[1:3]

## remove the "junk" col
myData = myData[,-15]


# training vs test errors
## Go back to the Boston Housing Data
## Divide the data into training and test

## n: sample size
## p: predictors
## (p+1): response (in this particular example)
n=dim(myData)[1]
p = dim(myData)[2] - 1

ntrain = round(n*0.6)
train.id = sample(1:n, ntrain)

train.RSS = rep(0, p)
test.RSS = rep(0, p)
for(i in 1:p){
  myfit = lm(Y ~., myData[train.id, c(1:i, (p+1))])
  train.Y = myData[train.id, (p+1)]
  train.Y.pred = myfit$fitted
  train.RSS[i] = mean((train.Y - train.Y.pred)^2)
  
  test.Y = myData[-train.id, (p+1)]
  test.Y.pred = predict(myfit, newdata=myData[-train.id, ])
  test.RSS[i] = mean((test.Y - test.Y.pred)^2)
}

## type="n": don't plot; just set the plotting region
plot(c(1,p), range(train.RSS, test.RSS), type="n", xlab="# of variables", ylab="Squared Error")
points(train.RSS, col="blue", pch=1)
lines(train.RSS, col="blue", pch=1)
points(test.RSS, col="red", pch=2)
lines(test.RSS, col="red", pch=2)
# the train error line should always be monotonically decreasing while it may not be the case 
# for the test error line as shown in the figure
diff(train.RSS) ## always negative
diff(test.RSS)  ## not always negative

# two stage LS
newY = lm(Y ~., myData[, c(1:10, 14)])$res

newX2 = myData[,11:13]
for(i in 1:3)
  newX2[,i] = lm(newX2[,i] ~ as.matrix(myData[, 1:10]))$res
## lm can take dataframe as input, but can also take vector and
## matrix as input. 

## retrieve the coefficients for the last three predictors
two.stage.fit = lm(newY ~ as.matrix(newX2))
two.stage.fit$coef
myfit$coef
cbind(two.stage.fit$coef[-1], myfit$coef[12:14])

## Residual is the same as the one from the original model
## Check the sum of squares of the diff of the two vectors
sum((two.stage.fit$res - lmfit$res)^2)

round(sum((two.stage.fit$res - lmfit$res)^2), dig=10)




