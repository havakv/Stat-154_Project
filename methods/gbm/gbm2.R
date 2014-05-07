library(kernlab)
library(caret)
library(gbm)
source("../test.R")

gbm2 <- function(trainX, trainy){
  k=2
  train <- cbind(trainy, trainX)
  names(train)[1] <- "V1"
  p=floor(sqrt(512))
  obj <- gbm(
	   V1~.,
	   data=train,
	   distribution="multinomial",  
	   n.trees=2000,
	   cv.folds=5,
	   shrinkage=0.1,
	   bag.fraction=p,
	   interaction.depth = k,
	   #train.fraction=1,
	   n.cores=1)

  obj$best.iter = gbm.perf(obj, method="cv", plot.it = F)
  class(obj) <- c("gbm2", class(obj))
  return(obj)
}

predict.gbm2 <- function(obj, testX){
  #names(testX)=names(train[-1])
  class(obj) <- class(obj)[2]
  pred <- predict(obj, testX, n.trees = obj$n.trees, single.tree = F, 
			type = "response")
  pred <- matrix(pred,nrow=dim(pred)[1], ncol=dim(pred)[2])
  predictions=max.col(pred,ties.method=c("random"))
  return(predictions)
}

#system.time(rates <- test(gbm2, nr = 1, cv.fold = 10, parallel = F, nCores = 5))
#rates

