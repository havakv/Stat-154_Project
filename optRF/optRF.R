# Function for finding optimal random forest.
library(randomForest)

optRF <- function(X, y, nfold = 5, ...){
  rfTuned <- tuneRF(X, y,
		    trace = FALSE, plot = FALSE, doBest = TRUE, ...)
  
  cv <- rfcv(X, y, cv.fold = nfold, ...)
  nrPred <- cv$n.var[which(cv$error.cv == min(cv$error.cv))]
  if (length(nrPred) > 1)
    nrPred = min(nrPred)
  bestPred <- sort(rfTuned$importance, decreasing = TRUE, 
		   index.return = TRUE)$ix
  
  rfOpt <- tuneRF(X[,bestPred], y,
		  trace = FALSE, plot = FALSE, doBest = TRUE, ...)

  class(rfOpt)  <- c("rfOpt", class(rfOpt))
  rfOpt$Xnames <- names(X)
  return(rfOpt)
}

predict.optRF <- function(obj, testX){
  if(class(testX) != "data.frame")
    testX <- data.frame(testX)
  names(testX) <- obj$Xnames
  class(obj) <- "randomForest"
  pred <- predict(obj, testX, type = "response")
  return(pred)
}

# --------------------------------------------------
# Test methods performance 
source("../test.R")
set.seed(0)
system.time(rate <- test(optRF, nr = 1, nfold = 5))
rate

#source("../test.R")
#set.seed(0)
#test(randomForest, nr = 3)

#----------------------------------------------------
# Submissions
source("../submission.R")
set.seed(0)
system.time(sub <- submission(optRF))
set.seed(0)
system.time(sub2 <- submission(randomForest))

