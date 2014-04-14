# Script for random forest by Haavard
library(randomForest)
data <- read.csv("../train.csv", header = FALSE)
names(data)[1] = "y"
data$y <- as.factor(data$y)


errorRate <- function(rf){
  return(c(mean.err.rate = mean(rf$err.rate[,1]), sd = sd(rf$err.rate[,1])))
}

system.time(rf <- randomForest(data[-1], data$y))
#   user  system elapsed 
# 19.033   0.080  19.302 

system.time(rfTuned <- tuneRF(data[-1], data$y, 
		  trace = FALSE, plot = FALSE, doBest = TRUE))
#   user  system elapsed 
# 29.162   0.144  29.900 

errorRate(rf)
errorRate(rfTuned)

system.time(cv <- rfcv(data[-1], data$y))
#    user  system elapsed 
# 164.474   0.356 166.337 

#with(cv, plot(n.var, error.cv, log="x", type="o", lwd=2))
cv$error.cv
nrPred <- cv$n.var[which(cv$error.cv == min(cv$error.cv))]
if (length(nrPred) > 1)
  nrPred = min(nrPred)
bestPred <- sort(rfTuned$importance, decreasing = TRUE, index.return = TRUE)$ix
system.time(rfOpt <- tuneRF(data[-1][,bestPred], data$y,
		  trace = FALSE, plot = FALSE, doBest = TRUE))
#   user  system elapsed 
# 30.142   0.116  30.550 
errorRate(rfOpt)
