# Functions for testing different methods
require(parallel) # one of the core R packages
require(doParallel)
library(foreach)

findPath <- function(){
  # Finds the path from your working directory to the "methods" folder
  wd <- strsplit(getwd(), "/")[[1]]
  nr <- max(which(wd == "methods"))
  n  <- length(wd)
  path <- paste(rep("../", n-nr), collapse = '')
}

splitData <- function( X, y, testProp = 1/3){
  # Split the data in train and test set.
  # testProp is the proportion of the dataset that will be used for testing.
  # so (1 - testProp) is the proportion for training.
  n <- length(y)
  if (testProp >= 1 | testProp <= 0)
    stop("testProp not in (0,1)")
  sp <- ceiling(n*(1-testProp))
  if (sp == 1)
    stop("No data in train")
  shuffled.order <- sample(n)
  train <- shuffled.order[1:sp] #about 2/3 of the data
  test <- shuffled.order[(sp+1):n]
  trainX <- X[train,]
  trainy <- y[train]
  testX <- X[test,]
  testy <- y[test]
  return(list(testX = testX, testy = testy, trainX = trainX, trainy = trainy))
}

missclass <- function(pred, y, level){
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
    ErrRates[i,] <- missclass(pred, testy, i)
  tot <- sum(pred != testy)/length(testy)
  return(list(tot = tot, indiv = ErrRates))
}

getRatesInPar <- function(method, nr = 1, testProp = 1/3, nCores, 
			  X, y, pushSplit = FALSE, ...){
  # Parallel version of getRates
  # Returns 17 x nr matrix. Each row represent a rate.
  # The reason for the format is efficiency in aveRates.
  # Remember to set: export OMP_NUM_THREADS=1 on machine
  #require(parallel) # one of the core R packages
  #require(doParallel)
  #library(foreach)
  if(as.numeric(system("echo $OMP_NUM_THREADS", intern = TRUE))	!= 1)
    stop("OMP_NUM_THREADS is not 1")

  registerDoParallel(nCores)
  seeds <- round(runif(nr, 0, .Machine$integer.max))
  Rates <- matrix(rep(NA, nr*17), 17, nr)
  Rates <- foreach(i = 1:nr, .combine = cbind) %dopar% {
    cat("Starting ", i, "\n", sep = '')
    set.seed(seeds[i])
    Split <- splitData(X, y, testProp)
    if (pushSplit)
      obj <- method(Split$trainX, Split$trainy, Split, ...)
    else
      obj <- method(Split$trainX, Split$trainy, ...)
    pred <- predict(obj, Split$testX)
    rat <- errorRates(pred, Split$testy)
    rates <- c(rat$tot, rat$indiv[,1], rat$indiv[,2])
    cat("Ending ", i, "\n", sep = '')
    rates
  }
  return(Rates)
}

getRates <- function(method, nr = 1, testProp = 1/3, X, y, 
		     pushSplit = FALSE, ...){
  # Returns 17 x nr matrix. Each row represent a rate.
  # The reason for the format is efficiency in aveRates.
  seeds <- round(runif(nr, 0, .Machine$integer.max))
  Rates <- matrix(rep(NA, nr*17), 17, nr)
  for (i in 1:nr){
    set.seed(seeds[i])
    Split <- splitData(X, y, testProp)
    if (pushSplit)
      obj <- method(Split$trainX, Split$trainy, Split, ...)
    else
      obj <- method(Split$trainX, Split$trainy, ...)
    pred <- predict(obj, Split$testX)
    rat <- errorRates(pred, Split$testy)
    Rates[,i] <- c(rat$tot, rat$indiv[,1], rat$indiv[,2])
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

test <- function(method, nr = 1, testProp = 1/3, path = findPath(),
		parallel = FALSE, nCores = 2, pushSplit = FALSE, ...){
  # Function to run test on method
  # Get data
  path <- paste(path, "train.csv", sep = '')
  data <- read.csv(path, header = FALSE)
  y <-  as.factor(data[,1])
  X <- data[,-1]

  # Run method nr times and get error rates
  if (parallel & nr != 1)
    Rates <- getRatesInPar(method, nr, testProp, nCores, X, y, pushSplit, ...)
  else
    Rates <- getRates(method, nr, testProp, X, y, pushSplit, ...)

  return(aveRates(Rates))
}

# E.G.
# Put this in you function file (see optRF.R)
#library(randomForest)
#set.seed(0)
#test(randomForest, nr = 4)
#set.seed(0)
#test(randomForest, nr = 4, parallel = TRUE)
