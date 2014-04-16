# Function to create submission files
findPath <- function(){
  # Finds the path from your working directory to the "methods" folder
  wd <- strsplit(getwd(), "/")[[1]]
  nr <- max(which(wd == "methods"))
  n  <- length(wd)
  path <- paste(rep("../", n-nr), collapse = '')
}

submission <- function(method, path = findPath(), ...){
  # Get data
  pathTrain <- paste(path, "train.csv", sep = '')
  pathTest <- paste(path, "test.csv", sep = '')
  train <- read.csv(pathTrain, header = FALSE)
  testX <- read.csv(pathTest, header = FALSE)
  trainy <- as.factor(train[,1])
  trainX <- train[,-1]
  names(testX) <- names(trainX)

  obj <- method(trainX, trainy, ...)
  pred <- predict(obj, testX)
  pred <- as.integer(pred) # We want integer outputs 

  submissions <- cbind((1:nrow(testX)), pred)
  filename <- paste(deparse(substitute(method)), "_submission.csv", sep = '')
  write.table(submissions, file = filename, sep = ",", 
	      col.names = c("Id", "Predictions"), row.names = FALSE) 
  invisible(submissions)

  ##ALWAYS CHECK YOUR OUTPUTTED SOLUTION FILE TO SEE IF IT'S IN THE FORMAT IDENTICAL TO sample_submission.csv !!!!!!
}
