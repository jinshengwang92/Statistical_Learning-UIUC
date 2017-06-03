# clean and combine 2016 data
setwd("~/Documents/Classes_taken/STAT542/Rcode/LendingClub/")
raw.test  = read.csv('train.csv', header = TRUE)[1:10,]
keep.names = names(raw.test)
Q1 = read.csv('LoanStats_2016Q1.csv', header = TRUE)[,keep.names]
Q2 = read.csv('LoanStats_2016Q2.csv', header = TRUE)[,keep.names]
Q3 = read.csv('LoanStats_2016Q3.csv', header = TRUE)[,keep.names]
Q4 = read.csv('LoanStats_2016Q4.csv', header = TRUE)[,keep.names]

total = rbind(rbind(rbind(Q1,Q2,make.row.names=FALSE),Q3,make.row.names=FALSE),Q4,make.row.names=FALSE)
#total1 = rbind(rbind(rbind(Q1,Q2),Q3),Q4)
write.csv(total,'./2016AllData_v2.csv',row.names = FALSE)
