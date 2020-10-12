install.packages("leaps")
library(leaps)

set.seed(7045607)
sampid = sample(dim(Boston)[1],400)
BostonNew = Boston[sampid,]
BostonNew


X1_raw = as.matrix(BostonNew[,2:14])
x1 = scale(X1_raw) # Standardised explanantory variables
y = BostonNew$lcrim # response variable
boston_data = data.frame(y,x1) #Matrix of response and explanatory variables
boston_data_raw = data.frame(y,X1_raw)

fit = lm(y~.,data = boston_data)
summary(fit) #Some explanatory variables are not significant when considered individually
             #Do not need all variables.

mean(fit$residuals^2) #MSE of scaled variables for least squares


fit_raw = lm(y~.,data = boston_data_raw)
mean(fit_raw$coefficients^2) #MSE of non-scaled variables of least squares

#Best subset selection; using least squares
bss = regsubsets(y~.,data = boston_data,method = "exhaustive",nvmax=13)
bss_summary = summary(bss)
bss_summary
names(bss_summary)
bss_summary$adjr2 #Adjr2 values
bss_summary$cp    #Cp statistic of each component
bss_summary$bic   #BIC values

best_adjr2 = which.max(bss_summary$adjr2) #Need highest adjR^2 value; 9 components
best_cp = which.min(bss_summary$cp)    #Smallest Cp and BIC; 8 components
best_bic = which.min(bss_summary$bic)   #7 components

best_adjr2 #Components needed for highest adjR2
best_cp    #Components needed for lowest CP
best_bic   #lowest BIC

par(mfrow=c(1,3))
plot(1:13,bss_summary$adjr2, xlab="Number of predictors",ylab="Adjusted R_squared",type="b")
points(best_adjr2,bss_summary$adjr2[best_adjr2],col="red",pch=16)
plot(1:13,bss_summary$cp, xlab="Number of predictors",ylab="Cp",type="b")
points(best_cp,bss_summary$cp[best_cp],col="red",pch=16)
plot(1:13,bss_summary$bic, xlab="Number of predictors",ylab="BIC",type="b")
points(best_bic,bss_summary$bic[best_bic],col="red",pch=16)
#Very little difference between M7 to M13 for adjr2 and cp
#choose 7 predictors
coef(bss,7)
