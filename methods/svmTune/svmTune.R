library(kernlab)
library(caret) 
library(e1071)
source("../test.R")

svmTune <- function(trainX, trainy, methodsvm, ...){
  obj <- train(trainX, trainy, method = methodsvm, ...)
  class(obj) <- c("svmTune", class(obj))
  return(obj)
}

predict.svmTune <- function(obj, testX){
  class(obj) <- class(obj)[2]
  pred <- predict(obj$finalModel, testX)
  return(pred)
}

#tuneControl = trainControl(method = "repeatedcv", number = 10, repeats = 10) #repeated 10-fold cross-validation

#t1 <- system.time({svmRadial <- test(svmTune, cv.fold = 10, parallel = F, 
		  #methodsvm = "svmRadial", trControl = tuneControl)})
#svmRadial

#t2 <- system.time({svmPoly <- test(svmTune, cv.fold = 10, parallel = F, 
		  #methodsvm = "svmPoly", trControl = tuneControl)})
#svmPoly

