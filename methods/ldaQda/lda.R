# Function for testing LDA with optimal predictor selection
library(MASS)
source("../test.R")
source("../optRF/optRF.R")

bestPred.lda2 <- function(X, y, ...){
  # Getting best predictors by sorting with optRF and choosing nr of predictors.
  error.lda2 <- function(nrPred, X, y, sortPred, seed, ...){
    # We optimize over this function to find the ideal nr of predictors
    nrPred <- round(nrPred)
    bestPred <- sortPred[1:nrPred]
    set.seed(seed)
    rat <- getRates(lda2, 1, 3, X[,bestPred], y, parallel = FALSE, 
		    opt = FALSE, ...)
    errRate <- aveRates(rat)$tot[1]
    return(errRate)
  }
  sortPred <- sortPred.optRF(X, y)
  seed <- round(runif(1, 0, .Machine$integer.max))
  opt <- optim(par = length(sortPred)/2, error.lda2, X = X, y = y, 
	       sortPred = sortPred, seed = seed, ...,
	       method = "Brent", lower = 1, upper = length(sortPred),
	       control = list(reltol = 1))
  return(sortPred[1:floor(opt$par)])
}

lda2 <- function(X, y, opt = TRUE, ...){
  # Function for running LDA whith variable selection (opt = TRUE)
  if (opt) {
    bestPred <- bestPred.lda2(X, y, ...)
  }
  else bestPred <- 1:ncol(X)
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

#set.seed(0)
##options(error = recover)
#test1 <- test(lda2, nr = 1, cv.fold = 4, parallel = TRUE)
#test1


