library(pls)
#load dataset
load('Boston.RData')
## Set the seed using one of your group members login ID
set.seed(7045607)
# Randomly sample 400 rows from the 506 rows in the full data
sampid = sample(dim(Boston)[1], 400)
traindata = Boston[sampid, ]
testdata = Boston[-sampid, ]

#set the response variable
y_train = traindata$lcrim
y_test = testdata$lcrim

#create matrix with the rest of the data
X1_raw_train = traindata[,2:14]
X1_raw_train = as.matrix(X1_raw_train)
X1_train = scale(X1_raw_train)

X1_raw_test = testdata[,2:14]
X1_raw_test = as.matrix(X1_raw_test)
X1_test = scale(X1_raw_test)

#data frame with response and data
boston_data_train = data.frame(y_train, X1_train)
boston_data_test = data.frame(y_test, X1_test)

#fit the model using PLS 
plsr_fit = plsr(y_train ~ ., data = boston_data_train, scale=FALSE)
#examine the directions defined by the PLS
plsr_load = loadings(plsr_fit)
#print the directions
(C = unclass(plsr_load))

summary(plsr_fit)

#extract the coefficients of the transformed variables
plsr_yload = Yloadings(plsr_fit)
#print the coefficients
(theta1_hat = unclass(plsr_yload))

#fit model, applying 10-fold cross validations
plsr_cv_fit = plsr(y_train ~ ., data = boston_data_train, scale=FALSE, validation="CV")

#plot the cross validation scores
plot(plsr_cv_fit, plottype = "validation", legend = "topright", val.type = "MSEP")

#corresponding MSE values
MSEP(plsr_cv_fit)
crossval(plsr_fit,segments =10,segment.type = "consecutive")
?crossval
coef(plsr_fit, intercept=T, ncomp=3)
