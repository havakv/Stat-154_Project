# Functions for testing different methods
require(parallel) # one of the core R packages
require(doParallel)
library(foreach)
library(cvTools)

findPath <- function(){
  # Finds the path from your working directory to the "methods" folder
  wd <- strsplit(getwd(), "/")[[1]]
  nr <- max(which(wd == "methods"))
  n  <- length(wd)
  path <- paste(rep("../", n-nr), collapse = '')
}

misclass <- function(pred, y, level){
  # See errorRates
  ix <- which(y == level)
  missy <- sum(pred[ix] != y[ix])/length(ix)
  ix <- which(pred == level)
  missPred <- sum(pred[ix] != y[ix])/length(ix)
  return(c(missy, missPred))
}

errorRates <- function(pred, testy){
  # Compute misclassification errors 
  # Returns list:
  # 	tot: total error rate
  # 	indiv: should is rate that should have been classified as i.
  # 	       shouldn't is rate that shouldn't have been classified as i.
  ErrRates <- matrix(rep(NA, 2*8), 8, 2)
  rownames(ErrRates) <- 1:8
  colnames(ErrRates) <- c("should", "shouldn't")
  for (i in 1:8)
    ErrRates[i,] <- misclass(pred, testy, i)
  tot <- sum(pred != testy)/length(testy)
  return(list(tot = tot, indiv = ErrRates))
}

crossVal <- function(method, cv.fold = 3, X, y, ...){
  n <- length(y)
  if (cv.fold > n)
    stop("cv.fold > length(y)")
  shuffled.order <- sample(n)
  groupSizes <- as.numeric(table(cvFolds(n, cv.fold)$which))
  ixGroup <- c(0, cumsum(groupSizes))

  Rates <- matrix(rep(NA, cv.fold*17), 17, cv.fold)
  for (i in 1:cv.fold){
    ixTest <- (1+ixGroup[i]):ixGroup[i+1]
    test   <- shuffled.order[ixTest]
    train  <- shuffled.order[-ixTest]
    trainX <- X[train,]
    trainy <- y[train]
    testX  <- X[test,]
    testy  <- y[test]
    obj <- method(trainX, trainy, ...)
    pred <- predict(obj, testX)
    rat <- errorRates(pred, testy)
    Rates[,i] <- c(rat$tot, rat$indiv[,1], rat$indiv[,2])
  }
  return(Rates)
}

crossValInPar <- function(method, cv.fold = 3, X, y, nCores = 2, ...){
  if(as.numeric(system("echo $OMP_NUM_THREADS", intern = TRUE))	!= 1)
    stop("OMP_NUM_THREADS is not 1")
  n <- length(y)
  if (cv.fold > n)
    stop("cv.fold > length(y)")
  shuffled.order <- sample(n)
  groupSizes <- as.numeric(table(cvFolds(n, cv.fold)$which))
  ixGroup <- c(0, cumsum(groupSizes))

  registerDoParallel(nCores)
  Rates <- matrix(rep(NA, cv.fold*17), 17, cv.fold)

  Rates <- foreach(i = 1:cv.fold, .combine = cbind) %dopar% {
    cat("Starting ", i, "\n", sep = '')
    ixTest <- (1+ixGroup[i]):ixGroup[i+1]
    test   <- shuffled.order[ixTest]
    train  <- shuffled.order[-ixTest]
    trainX <- X[train,]
    trainy <- y[train]
    testX  <- X[test,]
    testy  <- y[test]
    obj <- method(trainX, trainy, ...)
    pred <- predict(obj, testX)
    rat <- errorRates(pred, testy)
    rates <- c(rat$tot, rat$indiv[,1], rat$indiv[,2])
    cat("Ending ", i, "\n", sep = '')
    rates
  }
  return(Rates)
}

getRatesInPar <- function(method, nr = 1, cv.fold = 3, nCores, 
			  X, y, ...){
  # Parallel version of getRates
  # Returns 17 x nr matrix. Each row represent a rate.
  # The reason for the format is efficiency in aveRates.
  # Remember to set: export OMP_NUM_THREADS=1 on machine
  if(as.numeric(system("echo $OMP_NUM_THREADS", intern = TRUE))	!= 1)
    stop("OMP_NUM_THREADS is not 1")

  registerDoParallel(nCores)
  seeds <- round(runif(nr, 0, .Machine$integer.max))
  Rates <- matrix(rep(NA, nr*cv.fold*17), 17, nr*cv.fold)
  Rates <- foreach(i = 1:nr, .combine = cbind) %dopar% {
    cat("Starting ", i, "\n", sep = '')
    set.seed(seeds[i])
    rates <- crossVal(method, cv.fold, X, y, ...)
    cat("Ending ", i, "\n", sep = '')
    rates
  }
  return(Rates)
}

getRates <- function(method, nr = 1, cv.fold = 3, X, y, parallel = FALSE, 
		     nCores = 2, ...){
  # Returns 17 x cv.fold*nr matrix. Each row represent a rate.
  # The reason for the format is efficiency in aveRates.
  seeds <- round(runif(nr, 0, .Machine$integer.max))
  Rates <- matrix(rep(NA, nr*cv.fold*17), 17, nr*cv.fold)
  for (i in 1:nr){
    set.seed(seeds[i])
    pos <- (1+(i-1)*cv.fold):(i*cv.fold)
    if (parallel)
      Rates[,pos] <- crossValInPar(method, cv.fold, X, y, nCores, ...)
    else
      Rates[,pos] <- crossVal(method, cv.fold, X, y, ...)
  }
  return(Rates)
}

aveRates <- function(Rates){
  # Compute the average rates and sd
  # Returns list:
  # 	tot: total error rate
  # 	indiv: should is rate that should have been classified as i.
  # 	       shouldn't is rate that shouldn't have been classified as i.
  means <- rowMeans(Rates, na.rm = TRUE)
  sds <- apply(Rates, 1, sd, na.rm = TRUE)
  tot <- c(means[1], sds[1])
  names(tot) <- c("mean", "sd")
  indiv <- cbind(means[2:9], sds[2:9], means[10:17], sds[10:17])
  colnames(indiv) <- c("should", "sd", "shouldn't", "sd")
  rownames(indiv) <- 1:8
  return(list(tot = tot, indiv = indiv))
}

test <- function(method, nr = 1, cv.fold = 3, path = findPath(),
		parallel = FALSE, nCores = 2, ...){
  # Function to run test on method
  # Get data
  path <- paste(path, "train.csv", sep = '')
  data <- read.csv(path, header = FALSE)
  y <-  as.factor(data[,1])
  X <- data[,-1]
  # Run method nr times and get error rates
  Rates <- getRates(method, nr, cv.fold, X, y, parallel, nCores, ...)
  return(aveRates(Rates))
}

# E.G.
# Put this in you function file (see optRF.R)
#library(randomForest)
#set.seed(0)
#test(randomForest, nr = 1, cv.fold = 6)
#set.seed(0)
#test(randomForest, nr = 2, cv.fold = 3, parallel = TRUE)

