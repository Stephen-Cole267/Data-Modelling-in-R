install.packages("pls")
library("pls")

set.seed(7045607)
sampid = sample(dim(Boston)[1],400)
BostonNew = Boston[sampid,]

X1_raw = as.matrix(BostonNew[,2:14])
x1 = scale(X1_raw) # Standardised explanantory variables
y = BostonNew$lcrim # response variable
boston_data = data.frame(y,x1) #Matrix of response and explanatory variables

fit = lm(y~.,data = boston_data)
summary(fit) #Some explanatory variables are not significant when considered individually
#Do not need all variables.

#Principal Components regression
pcr_fit = pcr(y~.,data=boston_data,scale = FALSE)
pcr_load = loadings(pcr_fit)
c = unclass(pcr_load)
c #attr - proportion of variation of the standardised explanatory variables X1 that is explained by each of the PCs.

pcr_yload = Yloadings(pcr_fit)
theta1_hat = unclass(pcr_yload)
theta1_hat #comp 4,5,7,11 are close to 0 which suggests that this is a dimension in which X1 shows variation not strongly associated 
           #with the response.
summary(pcr_fit) #Cumulative proportion of variation in X1 explained by different numbers of PCs

## cross-validation
# 10-fold cross validation
nfolds = 10
(n = nrow(boston_data_train))
(p = ncol(boston_data_train) - 1)
# Sample fold-assignment index
fold_index = sample(nfolds, n, replace=TRUE)
fold_sizes = numeric(nfolds)
for(k in 1:nfolds) fold_sizes[k] = length(which(fold_index==k))
fold_sizes
#important to keep the same!!!!

pcr_cv_fit = pcr(y~., data=boston_data, scale=FALSE, validation="CV")
MSEP(pcr_cv_fit)
plot(pcr_cv_fit, plottype = "validation", legend = "topright",val.type="MSEP") #Elbow at three components

coef(pcr_fit, intercept = TRUE, ncomp=3)
predict(pcr_fit,newdata=x1,ncomp=3)