# Classifier that uses results from different classifiers
source("../test.R")
source("../optRF/optRF.R")
source("../logLasso/logLasso.R")


superclass <- function(trainX, trainy){
  rf <- randomForest(trainX, trainy)
  ll <- logLasso(trainX, trainy)
  obj <- list(rf = rf, ll = ll)
  class(obj) <- "superclass"
  return(obj)
}

predict.superclass <- function(obj, testX){
  predrf <- predict(obj$rf, testX)
  predll <- predict(obj$ll, testX)
  t2 <- which(predll == 2)
  t7 <- which(predll == 7)
  ix8 <- which(predrf == 8)
  pred <- predll
  pred[ix8] <- 8
  #pred[t2] <- predll[t2]
  #pred[t7] <- predll[t7]
  return(pred)
}


#set.seed(0)
##system.time(rate <- test(superclass, cv.fold = 3, parallel = FALSE))
#system.time(rate <- test(superclass, cv.fold = 10, parallel = TRUE, nCores = 5))
#rate


## Submissions
#source("../submission.R")
#set.seed(0)
#system.time(sub <- submission(superclass))
