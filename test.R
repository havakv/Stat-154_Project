# Script for testing functions

test <- function(method, nr = 1, testProp = 1/3){
  # Get data
  data <- read.csv("../train.csv", header = FALSE)
  y = as.factor(data[,1])
  X = data[,-1]
  
  splitData <- function(testProp = 1/3, XX = X, yy = y){
    # Split the data in train and test set.
    n <- length(yy)
    if (testProp >= 1 | testProp <= 0)
      stop("testProp not in (0,1)")
    sp <- ceiling(n*(1-testProp))
    if (sp == 1)
      stop("No data in train")
    shuffled.order <- sample(n)
    train <- shuffled.order[1:sp] #about 2/3 of the data
    test <- shuffled.order[(sp+1):n]
    trainX <<- XX[train,]
    trainy <<- yy[train]
    testX <<- XX[test,]
    testy <<- yy[test]
    invisible(NULL)
  }

  errorRate <- function(pred, y = testy)
    sum(pred != y)/length(y)

  rates <- rep(NA, nr)
  for (i in 1:nr){
    splitData(testProp = testProp)
    obj <- method(trainX, trainy)
    pred <- predict(obj, testX)
    rates[i] <- errorRate(pred, testy)
  }

  return(c(mean = mean(rates), sd = sd(rates)))
}
  
# E.G.
#set.seed(0)
#library(randomForest)
#test(randomForest, nr = 5)
