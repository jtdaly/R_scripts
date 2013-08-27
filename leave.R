# This will calculate total leave available based 
# on inputs from a leave.csv file I maintain.
#J.Daly created 2013-03-08
library(ggplot2)
leave<-as.data.frame(read.csv(file="/home/jdaly/Rfiles/leave.csv",TRUE))
weeks<-length(leave$ending.date)
ggplot(data=leave,aes(ending.date,leave.taken))+geom_point()
cat(weeks*4-sum(leave$leave.taken),"hours as of",as.character(leave[weeks,1]))
leave$cumulative<-seq(1:weeks)

leave$cumulative[1]<-4
for (i in 2:weeks) 
  leave$cumulative[i]<-(-leave$leave.taken[i]+4+leave$cumulative[i-1])

ggplot(data=leave,aes(x=seq(1:weeks),y=cumulative))+geom_line()