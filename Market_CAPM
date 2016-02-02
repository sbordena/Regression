###### Applied regression analysis - Final exam
###### Samuel Bordenave

setwd("/Users/samuelbordenave/Documents/Google Drive/CHICAGO BOOTH/### Regression - Stats/Final exam")

data<-read.csv("market.csv")
SBUX<-data$RET.SBUX-data$RF
GOOG<-data$RET.GOOG-data$RF

### Plot the variables to see which ones seem to be correlated
par(mfrow=c(2,4))
plot(data$MKTminusRF, SBUX,col=2,pch=20, main="Return of starbucks against market return")
plot(data$SMB, SBUX,col=2,pch=20, main="Return of starbucks against SMB")
plot(data$HML, SBUX,col=2,pch=20, main="Return of starbucks against HML")
plot(data$MOM, SBUX,col=2,pch=20, main="Return of starbucks against momentum")

plot(data$MKTminusRF, GOOG,col=2,pch=20, main="Return of Google against market return")
plot(data$SMB, GOOG,col=2,pch=20, main="Return of Google against SMB")
plot(data$HML, GOOG,col=2,pch=20, main="Return of Google against HML")
plot(data$MOM, GOOG,col=2,pch=20, main="Return of Google against momentum")

### Define the regressions
summary(CAPM.GOOG<-lm(GOOG~data$MKTminusRF))
summary(CAPM.SBUX<-lm(SBUX~data$MKTminusRF))

summary(ThreeFact.GOOG<-lm(GOOG~data$MKTminusRF+data$SMB+data$HML))
summary(ThreeFact.SBUX<-lm(SBUX~data$MKTminusRF+data$SMB+data$HML))

summary(ThreeFactMom.GOOG<-lm(GOOG~data$MKTminusRF+data$SMB+data$HML+data$MOM))
summary(ThreeFactMom.SBUX<-lm(SBUX~data$MKTminusRF+data$SMB+data$HML+data$MOM))

### Comparison of all models using BIC
n<-nrow(data)

# For Google
print(BICGoogle <- c(CAPM=extractAIC(CAPM.GOOG,k=log(n))[2], ThreeFactor=extractAIC(ThreeFact.GOOG, k=log(n))[2],ThreeFactorPlusMomentum=extractAIC(ThreeFactMom.GOOG, k=log(n))[2]))

print (eBICGoogle<-exp(-.5*(BICGoogle-min(BICGoogle))))

# For Starbucks
print(BICStarbucks <- c(CAPM=extractAIC(CAPM.SBUX,k=log(n))[2], ThreeFactor=extractAIC(ThreeFact.SBUX, k=log(n))[2],ThreeFactorPlusMomentum=extractAIC(ThreeFactMom.SBUX, k=log(n))[2]))

print (eBICStarbucks<-exp(-.5*(BICStarbucks-min(BICStarbucks))))
