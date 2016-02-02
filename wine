wine <- read.csv("wine.csv")

### Draw the time series
plot(wine$Sales, xlab="month", ylab="Sales", type="l", col=2, lwd=2, main="Time series of monthly Sales of wine in Australia", xaxt="n")
axis(1, at=(0:14)*12, labels=1996:2010)

### Draw the ACF analysis
acf(wine$Sales, lag.max=100)

### Change the scale to log
plot(LogSales<- log(wine$Sales), xlab="month", ylab="Log of Sales", type="l", col=2, lwd=2, main="Time series of Log of Sales of wine in Australia", xaxt="n")
axis(1, at=(0:14)*12, labels=1996:2010)

### Define the first regression using time, periodicity and autoregressive term
n<-nrow(wine)
t <- 2:n
YX<-data.frame(LogY<-LogSales[2:n],
LogYPast<-LogSales[1:n-1])

summary(reg1<-lm(LogY~t + sin(2*pi*t/12) + cos(2*pi*t/12) + LogYPast), data=YX)

### plot predictions
plot(LogSales, xlab="year",
     ylab="log monthly sales", type="l", col=4, lty=2,
     xaxt="n", lwd=2, main="Regression of Sales using periodicity and trend")
axis(1, at=(0:14)*12, labels=1996:2010)
lines(t, reg1$fitted, col=2, lwd=2)
legend("bottomleft", legend=c("data", "fitted"), lty=c(2,1), col=c(4,2))

### Plot the residuals to see how the model can be improved
par(mfrow=c(1,2))
plot(reg1$resid, xlab="year", ylab="residual", type="l",
col=4, main="residuals in time",  xaxt="n", lwd=2)
axis(1, at=(0:14)*12, labels=1996:2010)
acf(reg1$resid, lwd=2)

boxplot(reg1$resid ~ wine$Month[t], xlab="month", ylab="residuals", col=7)

### Define monthly dummy variables and add them to the regression
YX$jan <- wine$Month[t]=="January"
YX$dec <- wine$Month[t]=="December"
YX$jul <- wine$Month[t]=="July"

summary(reg2<-lm(LogY~t + sin(2*pi*t/12) + cos(2*pi*t/12) + LogYPast + YX$jan + YX$dec + YX$jul))

plot(LogSales, xlab="year",
     ylab="log monthly sales", type="l", col=4, lty=2,
     xaxt="n", lwd=2, main="Regression of Sales using periodicity, trend and dummy variable")
axis(1, at=(0:14)*12, labels=1996:2010)
lines(t, reg2$fitted, col=2, lwd=2)
legend("bottomleft", legend=c("data", "fitted"), lty=c(2,1), col=c(4,2))

### Add the 12th month in the regression
t2 <- 13:n
m<-n-12
YX2<-data.frame(LogY2<-LogSales[13:n],
LogYPast12<-LogSales[1:m])

YX2$jan <- wine$Month[t2]=="January"
YX2$dec <- wine$Month[t2]=="December"
YX2$jul <- wine$Month[t2]=="July"

summary(reg3<-lm(LogY2~t2 + sin(2*pi*t2/12) + cos(2*pi*t2/12) + LogYPast12 + jan + dec + jul, data=YX2))

plot(LogSales, xlab="year",
      ylab="log monthly sales", type="l", col=4, lty=2,
      xaxt="n", lwd=2, main="Regression of Sales using periodicity, trend and dummy variable")
axis(1, at=(0:14)*12, labels=1996:2010)
lines(t2, reg3$fitted, col=2, lwd=2)
legend("bottomleft", legend=c("data", "fitted"), lty=c(2,1), col=c(4,2))

### Get rid of the July term which doesn't look significant
summary(reg4<-lm(LogY2~t2 + sin(2*pi*t2/12) + cos(2*pi*t2/12) + LogYPast12 + jan+dec, data=YX2))

### Regression 5: only take into account the value of 12 months before
summary(reg5<-lm(LogY2~LogYPast12, data=YX2))

### Calculate all BIC
print(AICforRegs <- c(AICregression1=extractAIC(reg1,k=log(n))[2], AICregression2=extractAIC(reg2, k=log(n))[2],AICregression3=extractAIC(reg3, k=log(n))[2], AICregression4=extractAIC(reg4, k=log(n))[2], AICregression5=extractAIC(reg5, k=log(n))[2]))

print (AICprob<-exp(-.5*(AICforRegs-min(AICforRegs))))
