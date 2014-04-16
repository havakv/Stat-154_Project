# Functions for testing different methods
# Consider parallelizing...

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

test <- function(method, nr = 1, testProp = 1/3, path = findPath(), ...){
  # Function to run test on method
  # Get data
  path <- paste(path, "train.csv", sep = '')
  data <- read.csv(path, header = FALSE)
  y <-  as.factor(data[,1])
  X <- data[,-1]

  # Run method nr times and get error rates
  Rates <- matrix(rep(NA, nr*17), 17, nr)
  for (i in 1:nr){
    Split <- splitData(X, y, testProp)
    obj <- method(Split$trainX, Split$trainy, ...)
    pred <- predict(obj, Split$testX)
    rat <- errorRates(pred, Split$testy)
    Rates[1,i] <- rat$tot
    Rates[2:9,i] <- rat$indiv[,1]
    Rates[10:17,i] <- rat$indiv[,2]
  }
  return(aveRates(Rates))
}

# E.G.
# Put this in you function file (see optRF.R)
#set.seed(0)
#library(randomForest)
#test(randomForest, nr = 5)
