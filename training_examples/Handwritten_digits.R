setwd("~/Documents/Classes_taken/STAT542/Rcode/")
library(RColorBrewer)
library(class)  
install.packages("RColorBrewer")

?zip.train
data(zip.train)
dim(zip.train)

## The first column indicate the digit
table(zip.train[,1])

n=nrow(zip.train)
par(mfrow=c(3,4), pty='s', mar=c(1,1,1,1), xaxt='n', yaxt='n')
for(i in 1:12){
  tmp=zip.train[sample(1:n, 1), -1]; 
  tmp = tmp/max(tmp)
  tmp=matrix(tmp, 16, 16)
  image(tmp[, 16:1])
}

##Color ramp def.
colors = c('white','black')
cus_col = colorRampPalette(colors=colors)

par(mfrow=c(3,4), pty='s', mar=c(1,1,1,1), xaxt='n', yaxt='n')
for(i in 1:12){
  tmp=zip.train[sample(1:n, 1), -1]; 
  tmp = tmp/max(tmp)*255
  tmp=matrix(tmp, 16, 16)
  image(tmp[, 16:1], col=cus_col(256))
}

par(mfrow=c(3,4), pty='s', mar=c(1,1,1,1), xaxt='n', yaxt='n')
for(i in 0:9){
  tmp=zip.train[zip.train[,1]==i, -1]; 
  
  ## calculate the average image and rescale the value in each
  ## pixel to be between 0 to 255
  tmp=apply(tmp, 2, mean)
  tmp = tmp/max(tmp)*255
  
  tmp=matrix(tmp, 16, 16)
  image(tmp[, 16:1], col=cus_col(256))
}

?knn   ## check the help file for command "knn"

## For knn, no need to spend extra time on trianing.
## Training is done on the fly when a test point is given
## The command below may take some time to run
pred=knn(zip.train[,-1], zip.test[,-1], zip.train[,1])  

## display the classification result in a 2x2 table
table(zip.test[,1], pred)  

## Compute the 0/1 test error
mytable = table(zip.test[,1], pred)
1-sum(diag(mytable))/nrow(zip.test)