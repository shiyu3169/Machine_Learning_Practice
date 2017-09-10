############################ Setting up ############################
####################################################################
# Set up work directory
#------------------------------------------------------------------
#setwd('D:/Dropbox/CS6140/hw2')
setwd('C:/Users/Shiyu/Desktop')
library(leaps)
library(MASS)

############################ Part A ##############################################
######################################################################################
# Read data
#------------------------------------------------------------------
data <- read.table('prostate.txt', header=TRUE, row.names=1, sep='\t', stringsAsFactors = FALSE)

# factor the two categorical variables
# data$svi <- factor(data$svi)
# data$gleason <- factor(data$gleason)

# Select the training sets and validation sets
#------------------------------------------------------------------
train <- data[data$train, -10]
valid <- data[!data$train, -10]

############################ Part B ##########################################
##################################################################################
#### one-variable summary statistics ###
summary(train)
#it analyzed the general data of each column, including man, min, mean, etc.

### two-variable summary statistics ###
pairs(lpsa ~ lcavol + lweight + age + lbph + lcp + pgg45, data = train)
# From the chart
# By simply compare lpsa and oher variables, it tells that: 
# 	1.lspa increases with lcavol.
# 	2.Lcp and lweight also increse with lspa, but it is not quite strong. 
# 	3.A even less strong but possible positive association is with lbph. 
# Generally, This suggests that not all variabels may be needed 
# since some are correlated and may provide similar information about ;psa.

### Correlation coefficients ###
cor(data[,c(1,2,3,4,6,8,9)])
# The result
# 	1. lcavol has the highest correlation coefficients (0.7344603) suggesting a moderate positive linear association. 
# 	2. age and lbph have very low correlations (0.1695928, 0.1798094).
# 	3. lweight, lcp and pgg45 have small positive correlations (0.4333194, 0.5488132, 0.4223159).
# This suggests that the most important linear variables are lcavol with lcp, lweight and pgg45 possibly
# being important as well.

### Categorical variables ###
boxplot(lpsa ~ svi, data = train)
boxplot(lpsa ~ gleason, data = train)
# From the chart
# Both of these boxplots show lpsa varies significantly over the levels of svi and gleason.
# Therefore We need to consider including these variabels in our model.

### Missing values ###
is.na(train)
# The result shows no missing values in training set.
# After dealing with outliers, we will have some missing values.
# So if we need to calculate mean or other group vale, we need to exclude NA value

### Outliers
# To detect the outliers I use the command boxplot.stats()$out 
# which use the Tukey's method to identify the outliers ranged above and below the 1.5*IQR. 
outlier <- function(dt, var) {
    var_name <- eval(substitute(var),eval(dt))
    na1 <- sum(is.na(var_name))
    m1 <- mean(var_name, na.rm = T)
    par(mfrow=c(2, 2), oma=c(0,0,3,0))
    boxplot(var_name, main="With outliers")
    hist(var_name, main="With outliers", xlab=NA, ylab=NA)
    outlier <- boxplot.stats(var_name)$out
    mo <- mean(outlier)
    var_name <- ifelse(var_name %in% outlier, NA, var_name)
    boxplot(var_name, main="Without outliers")
    hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
    title("Outlier Check", outer=TRUE)
    na2 <- sum(is.na(var_name))
    cat("Outliers identified:", na2 - na1, "\n")
    cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "\n")
    cat("Mean of the outliers:", round(mo, 2), "\n")
    m2 <- mean(var_name, na.rm = T)
    cat("Mean without removing outliers:", round(m1, 2), "\n")
    cat("Mean if we remove outliers:", round(m2, 2), "\n")
    # dt[as.character(substitute(var))] <- invisible(var_name)
    # assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    # cat("Outliers successfully removed", "\n")
    # return(invisible(dt))
}

outlier(train, lcavol)
outlier(train, lweight)
outlier(train, age)
outlier(train, lbph)
outlier(train, lcp)
outlier(train, pgg45)
outlier(train, lpsa)

############################ Part C ##########################################
##################################################################################
# Start off with including every variable
fit <- lm(lpsa ~ ., data=train)
summary(fit)

# The model coefficients give us an estimate, standard error, t-value, and p-value for every
# variable in the model. The p-value is the probability of achieving the t-value if the null hypthesis were true
# The higher the p-value, the less we're sure that the variable is important to the model.
# The residual standard error is an estimate of the standard error of the error distribution. 
fit <-lm(lpsa ~ lcavol + lweight + svi, data = train)
plot(fit)
# 1.The first plot shows the residuals versus the fitted values. Here, we want to check that the residuals have
#   constant variance, and it appears that this assumptions holds. 
# 2.The second plot shows the normal Q-Q plot which allows us to assess the normality of the residuals. 
#   If the residuals are normal then this plot will be a straight line with a slope of 1, 
#   and it appears that is the case for this model. 
# 3.The third plot shows the scale location. 
# 4.The fourth plot allows you to spot individual points that are having a large effect on the regression results.

############################ Part D ##########################################
##################################################################################
fit2 <- regsubsets(lpsa ~ ., data = train, intercept = TRUE, method = "exhaustive")
regs <-summary(fit2)
regs
# This summary suggests lcavol + lweight

plot(regs$cp, xlab="Number of variables", ylab='Cp', type="l")
plot(regs$bic, xlab="Number of variables", ylab='Bic', type="l")
# Cp and BIC are the criterion for model selection among finite set of models, the smaller the value, the better the model.
# BIC penalizes more than Cp regarding n.

leaps.plot = regsubsets(lpsa~.,data=train, nbest=10)
plot(leaps.plot,scale="bic")
coef(fit2, which.min(regs$bic))
# (Intercept)      lcavol     lweight 
#  -1.0494396   0.6276074   0.7383751 

############################ Part E ##########################################
##################################################################################

####################################
## Ridge regression
####################################
# install.packages("MASS")
ridge <- lm.ridge(lpsa ~ .,data=train, lambda = seq(0,20,0.1))
plot(ridge)
# select parameter by minimum GCV
plot(ridge$GCV)
ridge$coef[,which.min(ridge$GCV)]

 #     lcavol     lweight         age        lbph         svi         lcp 
 # 0.60774607  0.28434026 -0.11036319  0.20050020  0.28330007 -0.16236691 
 #    gleason       pgg45 
 # 0.01204058  0.20593909 
####################################
## Lasso
####################################
# install.packages("lars")
library(lars)
lasso <- lars(as.matrix(train[,c(1:8)]),train$lpsa)
plot(lasso)

# Cross-validation
r <- cv.lars(as.matrix(train[,c(1:8)]), train$lpsa)
# bestfraction <- r$fraction[which.min(r$cv)]
bestfraction <- r$index[which.min(r$cv)]
bestfraction
# [1] 0.9090909
# Observe coefficients
coef.lasso <- predict(lasso,as.matrix(valid[,c(1:8)]),s=bestfraction,type="coefficient",mode="fraction")
coef.lasso
#      lcavol     lweight         age        lbph         svi         lcp 
# 0.464037714 0.494438064 0.000000000 0.081212327 0.433302941 0.000000000 
#     gleason       pgg45 
# 0.000000000 0.002532991 

############################ Part F ##########################################
##################################################################################

####################################
## Best subset selection
####################################
bestsubset <- lm(lpsa ~ .,data= train[,c(1,2,9)])
summary(bestsubset)
pred.bestsubset <- predict(bestsubset,valid[,c(1,2,9)])
summary((pred.bestsubset - valid$lpsa)^2)

####################################
## Ridge regression
####################################
pred.ridge = scale(valid[,c(1:8)],center = F)%*% ridge$coef[,which.min(ridge$GCV)] + ridge$ym
summary((pred.ridge - valid$lpsa)^2)

####################################
## Lasso
####################################
# Prediction
pred.lasso <- predict(lasso,as.matrix(valid[,c(1:8)]),s=bestfraction,type="fit",mode="fraction")$fit
summary((pred.lasso - valid$lpsa)^2)

############################ Part G ##########################################
##################################################################################

ridge$coef[,which.min(ridge$GCV)]

# Based on this model, we can say that:
  
# - One unit increase in lcavol will result in an expected increase of 0.61 units in lpsa, holding all other variables constant.
# - One unit increase in lweight will result in an expected increase of 0.28 units in lpsa, holding all
# other variables constant. 
# - One unit increase in age will result in an expected increase of -0.11 units in lpsa, holding all
# other variables constant.
# - One unit increase in lbph will result in an expected increase of 0.20 units in lpsa, holding all
# other variables constant.
# - One unit increase in lcp will result in an expected increase of -0.16 units in lpsa, holding all
# other variables constant.
# - Svi is a binary variable with a value of one of either one or zero.