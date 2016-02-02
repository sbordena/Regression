### Draw the scatterplots
churndata<-read.csv("churn.csv")

pairs(churndata[,7:9], pch=20, col=churndata$CHURN+1, main="Plots of day minutes, number of day calls and cost of day calls")
pairs(churndata[,c(7,10,13)], pch=20, col=churndata$CHURN+1, main="Plots of length of day, evening and night calls")
boxplot(churndata$IMIN ~ churndata$INTPLAN, xlab="International plan?", ylab="Number of minutes on intl calls", col=7)

### model fitting
churndata2<-churndata[,c(1,2,4,5,6,7,8,10,11,13,14,16,17,19,20)]
train <- 1:2500

null <- glm(CHURN~., family=binomial, data=churndata2[train,])
full <- glm(CHURN~.^2, family=binomial, data=churndata2[train,])
reg <- step(null, scope=formula(full), direction="forward", k=log(length(train)))

### Modify obtained reg to delete non significant variables

churndata3<-churndata[,c(2,4,5,6,7,8,10,11,13,14,16,17,19,20)]
churndata$texas<-churndata[,1]=="STATETX"
churndata$SouthCarolina<-churndata[,1]=="STATESC"

reg2 <- glm(CHURN~texas+SouthCarolina+INTPLAN+VMPLAN+NMIN+CUSCALL+DMIN*CUSCALL+INTPLAN*IMIN+ DMIN*EMIN+ VMPLAN*DMIN+ INTPLAN*DMIN+ EMIN*CUSCALL+ DMIN*NMIN+ INTPLAN*ICALL+ VMPLAN*EMIN+ INTPLAN*CUSCALL- NVMAIL- DMIN- DCALL- EMIN- NCALL- IMIN- ICALL, family=binomial, data=churndata3[train,])

### Calculate BIC and model probabilities
n<-nrow(churndata)
print(AICchurn <- c(AICnull=extractAIC(null,k=log(n))[2], AICreg=extractAIC(reg2,k=log(n))[2], AICfull=extractAIC(full, k=log(n))[2]))

print (AICprob<-exp(-.5*(AICchurn-min(AICchurn))))

### Calculate mean squared error on the left-out rows
predreg <- predict(reg2, newdata=churndata[-train,], type="response") 
predfull <- predict(full, newdata=churndata[-train,], type="response") 
errorreg <- churndata[-train,20]-(predreg >= .5)
errorfull <- churndata[-train,20]-(predfull >= .5)


mean(abs(errorreg))
mean(abs(errorfull))
