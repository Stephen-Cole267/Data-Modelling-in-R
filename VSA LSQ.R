load('Boston.RData')

## Set the seed using one of your group members login ID
set.seed(7045607)
# Randomly sample 400 rows from the 506 rows in the full data
sampid = sample(dim(Boston)[1], 400)
traindata = Boston[sampid, ]
testdata = Boston[-sampid, ]

#look at scatterplots of all variables
pairs(traindata)

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

fit = lm(y_train ~ ., data = boston_data_train)
summary(fit) #Some explanatory variables are not significant when considered individually
#Do not need all variables.

yhat_test = predict(fit, boston_data_test)
test_error = mean((boston_data_test$y_test - yhat_test)^2)
test_error
