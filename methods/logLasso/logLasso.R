# Logistic classification with lasso penalty

library(glmnet)
source("../test.R")
#source("../optRF/optRF.R")


###LASSO Logistic Regression###
logLasso <- function(trainX, trainy, se1 = FALSE){
  obj <- cv.glmnet(as.matrix(trainX), trainy, family = "multinomial", type.measure = "class", alpha = 1)
  #plot(logit.cv)
  obj$se1 <- se1
  class(obj) <- c("logLasso", class(obj))
  return(obj)
}

predict.logLasso <- function(obj, testX){
  class(obj) <- class(obj)[2]
  if (obj$se1){
    pred <- predict(obj, newx = as.matrix(testX), s = obj$lambda.1se, 
		    type = "class")
  } else{
    pred <- predict(obj, newx = as.matrix(testX), s = obj$lambda.min, 
		    type = "class")
  }
  return(as.factor(as.vector(pred)))
}

#set.seed(0)
##system.time(rate <- test(logLasso, cv.fold = 3, parallel = TRUE))
#system.time(rate <- test(logLasso, cv.fold = 10, parallel = TRUE, nCores = 5, 
			 #se1 = TRUE))
#rate
