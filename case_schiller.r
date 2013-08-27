## read in data
dat <- read.csv("CSHomePrice_History.csv")
#Now that the data is loaded, lets start by simply plotting the time series of the Indices.

## save dataset dimensions
n <- dim(dat)[1]
m <- dim(dat)[2]
 
## plot time series
col <- seq(1, m-1, 1)
matplot(dat[,2:m], type="l", xaxt="n", main="Case-Shiller Indices", ylab="Index Value", lty=1, col=col)
xticks <- seq(1, n, 12)
xlabels <- dat$YEAR[xticks]
axis(1, at = xticks, las = 2, cex.axis = 0.6, labels = xlabels)
legend("topleft", names(dat)[2:m], lty=1, cex=0.6, col=col)

## plot NY, CHI, DC
col <- seq(1, 3, 1)
matplot(cbind(dat$NYXR, dat$CHXR, dat$WDXR), type="l", xaxt="n", 
              main="Case-Shiller Indices", ylab="Index Value", lty=1, col=col)
xticks <- seq(1, n, 12)
xlabels <- dat$YEAR[xticks]
axis(1, at = xticks, las = 2, cex.axis = 0.6, labels = xlabels)
legend("topleft", c("New York", "Chicago", "Washington D.C."), lty=1, cex=0.6, col=col)

## calculate the monthly returns
r <- log(dat[2:n, 2:m] / dat[1:(n-1), 2:m])

## pairs plot of monthly returns
pairs(cbind(r$NYXR, r$CHXR, r$WDXR), main="Monthly Returns", 
          labels=c("New York", "Chicago", "Washington D.C."))
## boxplot
boxplot(r, xaxt="n", main="Monthly Returns", ylab="Monthly Return", col="light blue") 
abline(h=0)
xticks <- seq(1, m-1, 1)
xlabels <- names(r)
axis(1, at = xticks, las = 2, cex.axis = 0.6, labels = xlabels)

## qqnorm plots
par(mfrow=c(3,4))
for(i in 1:12){
  qqnorm(r[,i], main=names(r)[i])
  qqline(r[,i], col="red")
}
windows()
par(mfrow=c(3,4))
for(i in 13:(m-1)){
  qqnorm(r[,i], main=names(r)[i])
  qqline(r[,i], col="red")
}

###
mdf=melt(dat,id.vars="YEAR")
mdf$Date=as.Date(paste("01-",mdf$YEAR,sep=""),"%d-%b-%y")
names(mdf)=c("MonthYear","City","IndexValue","Date")
ggplot(data=mdf,aes(x=Date,y=IndexValue)) + geom_line(aes(color=City),size=1.25) +
scale_x_date("Year", minor="years") + scale_y_continuous("Case Schiller Index")
ggsave("all.cit.png")

###
sm=subset(mdf,City %in% c('NY.New.York','FL.Miami','CA.Los Angeles','MI.Detroit',
'TX.Dallas','IL.Chicago','DC.Washington'))
sm$City=droplevels(sm$City)
ggplot(data=sm,aes(x=Date,y=IndexValue)) + geom_line(aes(color=City),size=1.5) +
scale_x_date("Year", minor="years") + scale_y_continuous("Case Schiller Index")
ggsave("sel.cit.png")



