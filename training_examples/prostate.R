setwd("~/Documents/Classes_taken/STAT542/Rcode/")
library("ElemStatLearn")
data("prostate")
?prostate
names(prostate)  
## "lpsa" the 2nd last var is the response variable
## "train" is the indicator for training data
prostate$train
table(prostate$train)  # 67 training vs 30 testing

## Fit a linear regression model to predict lpsa
traindata = prostate[prostate$train==TRUE,]
testdata  = prostate[prostate$train==FALSE,]

## Remove the "train" indicator column
traindata = within(traindata, rm(train))
testdata  = within(testdata, rm(train))

myfit = lm(lpsa ~ . , data=traindata)
myfit  ## display the estimated LS coefficients
summary(myfit)  ## more output

mypredict = predict(myfit, newdata=testdata)

## mean squared error on the training and test sets. 
sum((traindata$lpsa - myfit$fitted)^2)/nrow(traindata) 
sum((testdata$lpsa - mypredict)^2)
sum((testdata$lpsa - mypredict)^2)/nrow(testdata)

library('leaps')
IC = regsubsets(lpsa ~ ., traindata, method = "exhaustive")
sumIC = summary(IC)
sumIC$bic
sumIC

n=nrow(traindata)
n
msize = apply(sumIC$which, 1, sum)
AIC = n * log(sumIC$rss/n) + 2 * msize
BIC = n * log(sumIC$rss/n) + log(n) * msize
AIC; BIC

AICfit = lm(lpsa ~ lcavol+lweight+age+lbph+svi+lcp+pgg45 , data=traindata)
AICpredict = predict(AICfit, newdata=testdata)
AIC_error = sum((testdata$lpsa - AICpredict)^2)
AIC_error

BICfit = lm(lpsa ~ lcavol+lweight , data=traindata)
BICpredict = predict(BICfit, newdata=testdata)
BIC_error = sum((testdata$lpsa - BICpredict)^2)
BIC_error

library(glmnet)
mylasso = glmnet(as.matrix(traindata[, c(1:8)]), as.vector(traindata$lpsa), alpha=1,lambda = 0.01)
#plot(mylasso)
#cv.out = cv.glmnet(as.matrix(traindata[, c(1:8)]), as.vector(traindata$lpsa), alpha=1,lambda = 0.5)
#plot(cv.out)
pout = predict(mylasso, as.matrix(testdata[, c(1:8)]))
pout
out = sum((testdata$lpsa - pout)^2)
out
