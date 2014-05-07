# Fit neural nets to our data
library(caret)
library(nnet) #neural net library
source("../test.R")

#------------------------------------------------------------------------------
#source("../optRF/optRF.R")

path <- paste(findPath(), "train.csv", sep = '')
data <- read.csv(path, header = FALSE)
y <-  as.factor(data[,1])
X <- data[,-1]

set.seed(0)
cv.fold <- 3
n <- length(y)
shuffled.order <- sample(n)
groupSizes <- as.numeric(table(cvFolds(n, cv.fold)$which))
ixGroup <- c(0, cumsum(groupSizes))

i <- 1
ixTest <- (1+ixGroup[i]):ixGroup[i+1]
test   <- shuffled.order[ixTest]
train  <- shuffled.order[-ixTest]
trainX <- X[train,]
trainy <- y[train]
testX  <- X[test,]
testy  <- y[test]
#------------------------------------------------------------------------------

nnetTuned <- function(trainX, trainy){
  tuneControl = trainControl(method = "cv", number = 10)
  obj <- train(trainX, trainy, method = "nnet", trControl = tuneControl) 
  class(obj) <- c("nnetTuned", class(obj))
  return(obj)
}

predict.nnetTuned <- function(obj, testX){
  class(obj) <- class(obj)[2]
  pred <- predict(obj$finalModel, testX)
  pred <- max.col(pred)
  return(pred)
}

hei <- nnetTuned(trainX, trainy)
pred <- predict(hei, testX)

sum(pred != testy)/length(pred)


