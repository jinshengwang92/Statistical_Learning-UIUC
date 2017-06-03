library(MASS)
data(Boston)
names(Boston)

Boston$chas  # categorical predictor
tmp = lm(medv ~ chas + age + tax + chas:age, data=Boston)
summary(tmp)
b=tmp$coef

Boston$newchas = ifelse(Boston$chas==0, 1, 0)
newtmp = lm(medv ~ newchas + age + newchas:age+ tax, data=Boston)
summary(newtmp)
newb = newtmp$coef

cbind(b, newb)
