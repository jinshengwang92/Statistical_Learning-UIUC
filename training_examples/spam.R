
##############################################################################
#
# This code is used to for classification of Email/Spam given the training data
#                     For Stat 542
#                     read on Feb 2nd 2017
#                     UIUC
#                     Jinsheng Wang
#
##############################################################################

setwd("~/Documents/Classes_taken/STAT542/Rcode/")
library("ElemStatLearn")
?spam
dim(spam)
names(spam)

spam.name=read.table("spam_name.txt");
for(i in 1:(ncol(spam)-1)) names(spam)[i]=as.character(spam.name[i,]);
names(spam)[ncol(spam)]="Y"
spam$Y = ifelse(spam$Y=="spam", 1, 0)

head(spam) # display the first 5 obs/rows.
spam[1:2,] # display the first 2 obs/rows

#attach(spam)
table(Y)  # counts for Y=1(spam) and 0 (email)

spam.sum = matrix(0, 57, 6)
colnames(spam.sum) = c("spam.mean", "email.mean", "spam.sd", "email.sd", "spam.med", "email.med")
rownames(spam.sum) = colnames(spam)[1:57]

for(i in 1:57){
  spam.sum[i,1] = mean(spam[Y==1,i])
  spam.sum[i,2] = mean(spam[Y==0,i])
  
  spam.sum[i,3] = sd(spam[Y==1,i])
  spam.sum[i,4] = sd(spam[Y==0,i])
  
  spam.sum[i,5] = median(spam[Y==1,i])
  spam.sum[i,6] = median(spam[Y==0,i])  
}

spam.sum  ## too many digits after the decimal point
round(spam.sum, dig=2)

spam.sum[1,]
library(ggplot2)
tmp = as.data.frame(spam[, c(1,58)])
ggplot(tmp, aes(x=Wmake,fill=as.factor(Y))) + 
  geom_density(alpha=0.7)

## Extract a subset from the training data to form a test set.
## If you want to make the sampling step reproducible, 
## use set.seed function. 

set.seed(10)  
n=nrow(spam)
test.id=rep(0,n);
table(test.id)
test.id[sample(1:n,500)]=1;  # here set the test size as 500
table(test.id)

## I would ignore the p-values/t-stat/F-stat since
## the response Y apparently doesn't follow a normal dist

mylm=lm(Y~., data=spam[test.id !=1,]);
summary(mylm)

pred.train=ifelse(mylm$fitted>0.5,1,0)
sum(pred.train == Y[test.id==0])
train.err = 1.0- sum(pred.train == Y[test.id==0])/(n-500)
train.err

pred.test=predict(mylm, spam[test.id==1,]);
#pred.test
pred.test=ifelse(pred.test>0.5,1,0)
sum(pred.test == Y[test.id==1])
test.err = 1- sum(pred.test == Y[test.id==1])/500
test.err

