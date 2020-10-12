library("glmnet")

set.seed(7045607)
sampid = sample(dim(Boston)[1],400)
BostonNew = Boston[sampid,]

X1_raw = as.matrix(BostonNew[,2:14])
x1 = scale(X1_raw) # Standardised explanantory variables
y = BostonNew$lcrim # response variable
boston_data = data.frame(y,x1) #Matrix of response and explanatory variables

grid = 10^seq(5,-3,length=100) #Grid of values for the tuning parameter
ridge_fit = glmnet(x1,y,alpha=0,standardize = FALSE, lambda = grid) #alpha=0 means we are doing ridge reg
#Forcing the function to use values lambda=10^5 (lots of shrinkage) to lambda=10^(-3) (very little shrinkage).

#Ridge regression coefficients
beta1_hat = coef(ridge_fit)

#Some examples of coefficients for different levels of shrinkage
beta1_hat[,1] #Lots of shrinkage
beta1_hat[,75] #Some shrinkage
beta1_hat[,100] #Very little shrinkage. Very similar to that of least squares estimate

par(mfrow=c(1,1))
#plot to show how estimated coefficients vary with lambda
plot(ridge_fit,xvar="lambda",col=1:13,label=TRUE) #number at top are number of coeff that are non-zero.

#Using cross-validation to choose appropriate value for tuning parameter
ridge_cv_fit = cv.glmnet(x1,y,alpha=0,standardize = FALSE,lambda=grid, foldid = fold_index) #k=10 folds with the same index
plot(ridge_cv_fit) #mean MSE plotted with error bars which covers the mean +- 1 Standard error

#Extracting the value of lambda which corresponds to the minimum
(lambda_min = ridge_cv_fit$lambda.min) 

#The tuning parameter which is the minimum.
(i=which(ridge_cv_fit$lambda == ridge_cv_fit$lambda.min))

#Corresponding mean MSE
ridge_cv_fit$cvm[i]
#Regression coefficients where s argument is used for tuning parameter
coef(ridge_fit, s=lambda_min)
predict(ridge_fit, newx = as.matrix(x1), s=lambda_min)