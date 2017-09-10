############################ Setting up ############################
####################################################################
# setwd('D:/Dropbox/CS6140/hw3')
install.packages()
##### Question 1

### Part A
sub <- sample(nrow(data), floor(nrow(data) * 0.5))
train<-data[sub,]
valid<-data[-sub,]

### Part B

## One-variable summary
summary(train)

## Two-variable summary
pairs(train)

### Correlation coefficients ###
cor(data[,c(1,2,3,4,6,7,8,9,10)])

### Missing values ###
any(is.na(train))

### Outliers
boxplot(train)

##### Question 2

#performs all subset selection (best subset selection)
regfit.full <- regsubsets(chd~., data = train)
reg.summary <- summary(regfit.full)
reg.summary

#Perform variable selection using all subsets selection BIC criteria.
plot(reg.summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")

#Getting number of parameters
which.min(reg.summary$bic)

# choosing 2 predictor is fine, then it will be "age" and "ldl"

# Apply logistic regression
sa.heart.fit = glm(chd ~ age + ldl, family = binomial, data=train)
coef(sa.heart.fit)
# The logistic model is: 
# log(chd/(1-chd)) = −4.92215492 + 0.05386515 x age + 0.35512072 x ldl + error
# age and ldl have correlation of 0.3

##### Question 3
lda.fit = lda(chd ~ age + ldl, data=train,cv=TRUE)
lda.fit
# The LDA output indicates that ˆπ1 = 0.6927083 and ˆπ2 = 0.3072917
# Group means are the average of each predictor within each class, and are used by LDA as estimates of μ..
#The coefficients of linear discriminants output provides the linear combination of Lag1 and Lag2 that are used to form the LDA decision rule.

# prediction on the training set
lda.pred=predict(lda.fit , train)
lda.class =lda.pred$class
table(lda.class)

##### Question 6
#logistic 
# --------------Predictive ability on the training set-------------------------
summary(sa.heart.fit)
# calculate predicted probabilities on the same training set
scores <- predict(sa.heart.fit, newdata=train, type="response")

# compare predicted probabilities to labels, for varying probability cutoffs
pred <- prediction(scores, labels=train$chd )
perf <- performance(pred, "tpr", "fpr")

# plot the ROC curve
plot(perf, colorize=F, main="In-sample ROC curve")

# print out the area under the curve
unlist(attributes(performance(pred, "auc"))$y.values)


# --------------Evalate the predictive ability on the validation set------------
# make prediction on the validation dataset
scores <- predict(sa.heart.fit, newdata=valid, type="response")
pred <- prediction( scores, labels=valid$chd )
perf <- performance(pred, "tpr", "fpr")

# overlay the line for the ROC curve
plot(perf, colorize=T, add=TRUE)

# print out the area under the curve
unlist(attributes(performance(pred, "auc"))$y.values)


#LDA
# ROC on the training set
scores <- predict(lda.fit, newdata= train)$posterior[,2]
pred <- prediction( scores, labels= train$chd )
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=T, main="LDA")
# print out the area under the curve
unlist(attributes(performance(pred, "auc"))$y.values)


# ROC on the validation set
# prediction on the validation set
lda.pred=predict(lda.fit , valid)
lda.class =lda.pred$class
table(lda.class)

scores <- predict(lda.fit, newdata= valid)$posterior[,2]
pred <- prediction( scores, labels= valid$chd )
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=T, main="LDA", add=TRUE)
# print out the area under the curve
unlist(attributes(performance(pred, "auc"))$y.values)


