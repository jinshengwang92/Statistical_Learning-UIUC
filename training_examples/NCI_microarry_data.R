########################
#    NCI Microarray Data
########################

setwd("~/Documents/Classes_taken/STAT542/Rcode/")
library("ElemStatLearn")

data(nci)
dim(nci)
?nci

## Create a custom color palette from green to red
cus_col = colorRampPalette(colors=c("green", "black", "red"))
?colorRampPalette
## Show just the first 500 genes
?image
image(nci[1:500,], col=cus_col(124))
