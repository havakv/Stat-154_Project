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
bestPred.lda2 <- function(X, y, ...){
  # Getting best predictors by sorting with optRF and choosing nr of predictors.
  error.lda2 <- function(nrPred, X, y, sortPred, seed, ...){
    # We optimize over this function to find the ideal nr of predictors
    nrPred <- round(nrPred)
    bestPred <- sortPred[1:nrPred]
    set.seed(seed)
    rat <- getRates(lda2, 1, 1/3, X[,bestPred], y, ...)
    errRate <- aveRates(rat)$tot[1]
    #cat(errRate, "\n")
    #cat(nrPred, "\n")
    return(errRate)
  }
  sortPred <- sortPred.optRF(X, y)
  seed <- round(runif(1, 0, .Machine$integer.max))
  opt <- optim(par = length(sortPred)/2, error.lda2, X = X, y = y, 
	       sortPred = sortPred, seed = seed, ...,
	       method = "Brent", lower = 1, upper = length(sortPred),
	       control = list(reltol = 1))
  cat("Nr pred ", floor(opt$par), "\n")
  cat("Val ", opt$value, "\n")
  return(sortPred[1:floor(opt$par)])
}

lda2 <- function(X, y, opt = FALSE, ...){
  # Function for running LDA whith variable selection (opt = TRUE)
  if (opt) bestPred <- bestPred.lda2(X, y, ...)
  else bestPred <- 1:ncol(X)
  #cat("length(bestPred) ", length(bestPred), "\n")
  #bestPred <- NULL
  obj <- lda(X[,bestPred], y, CV = FALSE, ...)
  obj$bestPred <- bestPred
  class(obj) <- c("lda2", class(obj))
  return(obj)
}
predict.lda2 <- function(obj, testX){
  class(obj) <- "lda"
  pred <- predict(obj, testX[,obj$bestPred])
  return(pred$class)
}

set.seed(0)
#test1 <- test(lda2, nr = 24, parallel = TRUE, pushSplit = TRUE, nCores = 6)
test1 <- test(lda2, nr = 16, parallel = TRUE, opt = TRUE)
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
