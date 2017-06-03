setwd("~/Documents/Classes_taken/STAT542/Rcode/IowaHousing/")
train=read.csv("train.csv")
test =read.csv('test.csv')
dim(train)
names(train)
numNA = colSums(apply(train[, -c(1, 81)], 2, is.na))
numNA
number_of_missing = numNA[which(numNA != 0)]
number_of_missing
data_type = sapply(train[,names(which(numNA != 0))], class)  # type of data
data_type
cbind(number_of_missing, data_type)


summary(train$SalePrice)
sum(train$SalePrice<35000)
sum(train$SalePrice<36000)
median(train$SalePrice)
sum(train$SalePrice > 500000)
sum(train$GarageCars > 3)

keep.vars = c("OverallQual", "X1stFlrSF", "GrLivArea", "GarageCars", "GarageArea", "SalePrice")
mytrain = train[, colnames(train) %in% keep.vars]
dim(mytrain)
mytrain[1:2,]

sum(is.na(mytrain))  ## no missing value check for na

summary(mytrain)
table(mytrain$GarageCars)
table(mytrain$OverallQual)

mytrain$SalePrice = log(mytrain$SalePrice+1)
myfit=lm(SalePrice ~., data=mytrain)
summary(myfit)

mytrain2 = mytrain; 
mytrain2$X1stFlrSF = log(mytrain2$X1stFlrSF+1)
mytrain2$GrLivArea = log(mytrain2$GrLivArea+1)
mytrain2$GarageArea = log(mytrain2$GarageArea+1)
myfit2 = lm(SalePrice ~., data=mytrain2)
summary(myfit2)

apply(mytrain, 2, median)

newhouse = apply(mytrain, 2, median); 
newhouse[c(2,3,5)]=log(newhouse[c(2,3,5)] +1)
newhouse = as.data.frame(t(newhouse))
newhouse
yhat = predict(myfit2, newdata=newhouse)
exp(yhat)-1                  

