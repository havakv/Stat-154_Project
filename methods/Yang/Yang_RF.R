library(fields)
library(methods)
library(glmnet)
library(rpart)
library(spls)
library(rpart.plot)
library(ipred)
library(randomForest)

CVclassify = function(x, y, fold = 10, model, ntree) {
  n = length(y)
  error = numeric(fold)
  for (i in 0:(fold - 1)) {
    index = (1:n)[1:n %% fold == i]
    pred = model(x[-index, ], y[-index], x[index, ], ntree)
    error[i+1] = sum(abs(as.numeric(pred) - as.numeric(y[index]))) / length(index)
  }
  
  return (error)
}

submission = function(pred, fileName) {
  sub = data.frame(Id = 1:1888, Predictions = pred)
  write.csv(sub, file = fileName, row.names = FALSE)
}

##Reading data##
test = read.csv("test.csv", header = FALSE)
train = read.csv("train.csv", header = FALSE)

trainY = train[ ,1]
trainX = train[-1]

names(trainX) = names(test)
names(trainY) = "Y"

##Random Forest##
#1000 trees, everything else default#
set.seed(0)
rf.fit = randomForest(x = trainX, y = as.factor(trainY), ntree = 1000)
#Out of bag err rate is 81.1%
sum(rf.fit$predicted == as.factor(trainY)) / length(trainY)

rf_pred = predict(rf.fit, newdata = test, type = "response")

submission(as.numeric(rf_pred), "yang_rf.csv")

#Tuning mtry gives the number same as default
bestMtry = tuneRF(x = trainX, y = as.factor(trainY), ntree = 1000, nodesize = 1)


###PCA###
PCA <- prcomp(trainX, retx=TRUE)

par(mfrow=c(1,2))
screeplot(PCA, type = "lines", main = "screeplot")
#identify(stockPCA$x[,1], stockPCA$x[,2])
plot(PCA$x[,1], PCA$x[,2], xlab = "First Principal Component", ylab = "Second Principal Component", pch = 20, col = trainY)

trainXPCA = PCA$x[ ,1:50]
rot = PCA$rotation
rf.pca.fit = randomForest(x = trainXPCA, y = as.factor(trainY), ntree = 1000)
#Out of bag err rate is 81.1%
sum(rf.pca.fit$predicted == as.factor(trainY)) / length(trainY)


###LASSO Logistic Regression###
logit.cv = cv.glmnet(as.matrix(trainX), trainY, family = "multinomial", type.measure = "class", alpha = 1)
plot(logit.cv)

lasso.logistic.predict = predict(logit.cv, newx = as.matrix(test), s = logit.cv$lambda.min, type = "class")

submission(as.numeric(lasso.logistic.predict), "yang_Lasso_logit.csv")

lasso.logistic.1se.predict = predict(logit.cv, newx = as.matrix(test), s = logit.cv$lambda.1se, type = "class")

submission(as.numeric(lasso.logistic.1se.predict), "yang_Lasso_logit_1se.csv")



###Random Forest with the predictors picked up by LASSO logistic###
predSet = which(as.numeric(beta$"1") != 0)
for (i in 1:8) {
  predSet = union(predSet, which(as.numeric(beta$"i") != 0))
}

predSet = predSet[-1] - 1

set.seed(0)
rf.lasso = randomForest(x = trainX[ , predSet], y = as.factor(trainY), ntree = 1000)
#Out of bag err rate is 80.5%
sum(rf.lasso$predicted == as.factor(trainY)) / length(trainY)

rf_lasso_pred = predict(rf.lasso, newdata = test, type = "response")
submission(as.numeric(rf_lasso_pred), "yang_rf_lasso.csv")



###elastic net###
cvError = numeric(length(seq(0, 1, 0.2)))
i = 1
for (a in seq(0, 1, 0.2)){
  proCV = cv.glmnet(as.matrix(trainX), trainY, family = "multinomial", type.measure = "class", alpha = a)
  cvError[i] = min(proCV$cvm)
  i = i+1
}

bestNet = cv.glmnet(as.matrix(trainX), trainY, family = "multinomial", type.measure = "class", alpha = 0.4)

bestNet.predict = predict(bestNet, newx = as.matrix(test), s = bestNet$lambda.min, type = "class")

submission(as.numeric(bestNet.predict), "yang_best_net.csv")

