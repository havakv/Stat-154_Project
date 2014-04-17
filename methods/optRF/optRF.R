# Function for finding optimal random forest.
library(randomForest)

sortPred.optRF <- function(X, y, ...){
  rfTuned <- tuneRF(X, y,
		    trace = FALSE, plot = FALSE, doBest = TRUE, ...)
  if (length(rfTuned$importance) != ncol(X))
    stop("importance does not contain all variables")
  sortPred <- sort(rfTuned$importance, decreasing = TRUE, 
		   index.return = TRUE)$ix
  return(sortPred)
}

bestPred.optRF <- function(X, y, nfold = 5, ...){
  # Find index for the best predictors in optRF
  cv <- rfcv(X, y, cv.fold = nfold, ...)
  nrPred <- cv$n.var[which(cv$error.cv == min(cv$error.cv))]
  if (length(nrPred) > 1)
    nrPred <- min(nrPred)
  sortPred <- sortPred.optRF(X, y, ...)
  bestPred <- sortPred[1:nrPred]
  return(bestPred)
}

optRF <- function(X, y, nfold = 5, ...){
  # Optimal RF, by using crossvalidation to find the best set of predictors
  bestPred <- bestPred.optRF(X, y, nfold, ...)
  rfOpt <- tuneRF(X[,bestPred], y,
		  trace = FALSE, plot = FALSE, doBest = TRUE, ...)
  rfOpt$Xnames <- names(X)
  rfOpt$bestPred <- bestPred
  class(rfOpt)  <- c("optRF", class(rfOpt))
  return(rfOpt)
}

predict.optRF <- function(obj, testX){
  if(class(testX) != "data.frame")
    testX <- data.frame(testX)
  names(testX) <- obj$Xnames
  class(obj) <- "randomForest"
  pred <- predict(obj, testX[,obj$bestPred], type = "response")
  return(pred)
}

# --------------------------------------------------
# Test methods performance 
#source("../test.R")
#set.seed(0)
#system.time(rate <- test(optRF, nr = 1, nfold = 2))
#rate

##source("../test.R")
##set.seed(0)
##test(randomForest, nr = 3)

##----------------------------------------------------
## Submissions
#source("../submission.R")
#set.seed(0)
#system.time(sub <- submission(optRF))
#set.seed(0)
#system.time(sub2 <- submission(randomForest))

