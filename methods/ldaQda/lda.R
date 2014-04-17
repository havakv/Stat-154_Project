# Script for lda and qda classifiers
library(MASS)
source("../test.R")
source("../optRF/optRF.R")

#-------------------------------------------------
#path <- paste(findPath(), "train.csv", sep = '')
#data <- read.csv(path, header = FALSE)
#y <-  as.factor(data[,1])
#X <- data[,-1]

## LDA with CV
#set.seed(0)
#ldaCV <- lda(X, y, CV = TRUE)
#sum(ldaCV$class != y)/length(y)
#ldaFit <- lda(X, y, CV = FALSE)
#ldaPred <- predict(ldaFit, X)
#sum(ldaPred$class != y)/length(y)
#-------------------------------------------------

# Function for testing LDA with optimal predictor selection
bestPred.lda2 <- function(X, y, Split, ...){
  error.lda2 <- function(nrPred, X, y, sortPred, Split, ...){
    nrPred <- round(nrPred)
    bestPred <- sortPred[1:nrPred]
    obj <- lda(X[,bestPred], y, CV = FALSE, ...)
    obj$bestPred <- bestPred
    class(obj) <- c("lda2", class(obj))
    pred <- predict(obj, Split$testX)
    errRate <- errorRates(pred, Split$testy)$tot
    cat(errRate, "\n")
    cat(nrPred, "\n")
    return(errRate)
  }
  sortPred <- sortPred.optRF(X, y)
  opt <- optim(par = length(sortPred)/2, error.lda2, X = X, y = y, 
	       sortPred = sortPred, Split = Split, ...,
	       method = "Brent", lower = 1, upper = length(sortPred),
	       control = list(reltol = 0.1))
  return(sortPred[1:opt$par])
}

lda2 <- function(X, y, Split, ...){
  # Need a function to pick optimal predictors
  # With all predictors the results are horrible
  if (!exists("Split")) stop("Need to set \"pushSplit = TRUE\"")
  bestPred <- bestPred.lda2(X, y, Split, ...)
  cat("length(bestPred) ", length(bestPred), "\n")
  #bestPred <- NULL
  obj <- lda(X[,bestPred], y, CV = FALSE, ...)
  #obj <- lda(X[,101:170], y, CV = FALSE, ...)
  obj$bestPred <- bestPred
  class(obj) <- c("lda2", class(obj))
  return(obj)
}
predict.lda2 <- function(obj, testX){
  class(obj) <- "lda"
  pred <- predict(obj, testX[,obj$bestPred])
  #pred <- predict(obj, testX[,101:170])
  return(pred$class)
}

set.seed(0)
#test1 <- test(lda2, nr = 24, parallel = TRUE, pushSplit = TRUE, nCores = 6)
test1 <- test(lda2, nr = 2, parallel = TRUE, pushSplit = TRUE)
test1


#-------------------------------------------------
## QDA with CV
## We need < 100 predictors to be able to run qda
#qdaCV <- qda(X[,101:200], y, CV = TRUE)
#sum(is.na(qdaCV$class))
#sum(qdaCV$class != y, na.rm = TRUE)/length(y)
#
##-------------------------------------------------
#
## Function for testing QDA with optimal predictor selection
#qda2 <- function(X, y, ...){
#  # Need a function to pick optimal predictors
#  # Nr of predictors must be smaller than the smallest nr of one factor in the groups
#  # Max amount of predictors allowed in qda
#  maxNrPred <- min(as.integer(table(y)))-1
#
#  obj <- qda(X[,101:130], y, CV = FALSE, ...)
#  class(obj) <- c("qda2", class(obj))
#  return(obj)
#}
#predict.qda2 <- function(obj, testX){
#  class(obj) <- "qda"
#  pred <- predict(obj, testX[,101:130])
#  return(pred$class)
#}
#
#test(qda2, nr = 32, parallel = TRUE)
