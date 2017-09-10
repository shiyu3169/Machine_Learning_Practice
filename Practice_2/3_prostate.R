# Setting up
###################################################################
###################################################################

# Set up work directory
#------------------------------------------------------------------
setwd('/Users/ovitek/Dropbox/Olga/Teaching/CS6140/Spring17/LectureNotes/3-linearRegression/R')

# Read data
#------------------------------------------------------------------
X <- read.table('prostate.txt', header=TRUE, row.names=1, sep='\t')
head(X)
table(X$train)

# Select the training and the validation sets
#------------------------------------------------------------------
Xtrain <- X[X$train,-10]
Xvalid <- X[!X$train,-10]


# Exploration and modeling
###################################################################
###################################################################
 
# Data exploration
#------------------------------------------------------------------
summary(Xtrain)
pairs(Xtrain)
 
# Simple regression
#------------------------------------------------------------------
par(mfrow=c(2,2))
plot(lpsa ~ age, data=Xtrain, pch=16, col='red', ylim=c(0,5), main='lspa ~ age') 
out <- lm(lpsa ~ age, data=Xtrain)
out 
summary(out)
abline(lm(lpsa ~ age, data=Xtrain), col='grey')


# Additive multiple regression
#------------------------------------------------------------------
boxplot(lpsa ~ svi, data=Xtrain, col='orange', ylim=c(0,5))
out1 <- lm(lpsa ~ age+svi, data=Xtrain)
summary(out1)

plot(lpsa ~ age, data=Xtrain, pch=16, col='orange', ylim=c(0,5), main='lspa ~ age+svi')
lines(40:80, predict(out1, newdata=data.frame(age=40:80, svi=1)), lty=1)
lines(40:80, predict(out1, newdata=data.frame(age=40:80, svi=0)), lty=2)

# Multiple regression with interaction
#------------------------------------------------------------------
out2 <- lm(lpsa ~ age*svi, data=Xtrain)
summary(out2)

plot(lpsa ~ age, data=Xtrain, pch=16, col='green', ylim=c(0,5), main='lspa ~ age+svi+age*svi')
lines(40:80, predict(out2, newdata=data.frame(age=40:80, svi=1)), lty=1)
lines(40:80, predict(out2, newdata=data.frame(age=40:80, svi=0)), lty=2)


# Multiple regression with all predictors
#------------------------------------------------------------------
out3 <- lm(lpsa ~ ., data=Xtrain)
summary(out3)
names(out3)


# Checking of assumptions
###################################################################
###################################################################

# Residual plot
#------------------------------------------------------------------
plot(out2$fitted.values, out2$residuals)
abline(h=0)
plot(out2$residuals)

qqnorm(out2$residuals)
qqline(out2$residuals)


# Prediction on the validation set
###################################################################
###################################################################
par(mfrow=c(2,2))
# Prediction: compare models
#------------------------------------------------------------------
plot(Xvalid$lpsa, predict(out, newdata=Xvalid), 
	pch=16, col='red', xlim=c(1,6), ylim=c(1,6), xlab='observed', ylab='predicted', 
	main='validation, lspa ~ age')
abline(a=0,b=1, col='grey')

plot(Xvalid$lpsa, predict(out1, newdata=Xvalid), 
	pch=16, col='orange', xlim=c(1,6), ylim=c(1,6), xlab='observed', ylab='predicted', 
	main='validation, lspa ~ age+svi')
abline(a=0,b=1, col='grey')

plot(Xvalid$lpsa, predict(out2, newdata=Xvalid), 
	pch=16, col='green', xlim=c(1,6), ylim=c(1,6), xlab='observed', ylab='predicted', 
	main='validation, lspa ~ age + svi + age*svi')
abline(a=0,b=1, col='grey')

plot(Xvalid$lpsa, predict(out3, newdata=Xvalid), 
	pch=16, col='blue', xlim=c(1,6), ylim=c(1,6), xlab='observed', ylab='predicted', 
	main='validation, lspa ~ all')
abline(a=0,b=1, col='grey')

# Prediction: compare training and validation set
#------------------------------------------------------------------
par(mfrow=c(2,2))
plot(Xtrain$lpsa, predict(out, newdata=Xtrain), 
	pch=16, col='red', xlim=c(1,6), ylim=c(1,6), xlab='observed', ylab='predicted', 
	main='training, lspa ~ age')
abline(a=0,b=1, col='grey')

plot(Xvalid$lpsa, predict(out, newdata=Xvalid), 
	pch=16, col='red', xlim=c(1,6), ylim=c(1,6), xlab='observed', ylab='predicted', 
	main='validation, lspa ~ age')
abline(a=0,b=1, col='grey')

plot(Xtrain$lpsa, predict(out3, newdata=Xtrain), 
	pch=16, col='blue', xlim=c(1,6), ylim=c(1,6), xlab='observed', ylab='predicted', 
	main='training, lspa ~ all')
abline(a=0,b=1, col='grey')

plot(Xvalid$lpsa, predict(out3, newdata=Xvalid), 
	pch=16, col='blue', xlim=c(1,6), ylim=c(1,6), xlab='observed', ylab='predicted', 
	main='validation, lspa ~ all')
abline(a=0,b=1, col='grey')
