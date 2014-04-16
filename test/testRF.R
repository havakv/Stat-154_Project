# parallell random forest
# This does not work for classification yet!!!!!!!!!!!!!!!
# so the code will just crash.

#install.packages("devtools")
library(devtools)
source("../methods/test.R")
dev_mode(on=T)
#install_local("mkuhn-parallelrandomforest-804d353ec2f7")
library(parallelRandomForest)
system.time(test(randomForest, nr = 3, path = "../methods/"))
dev_mode(on=F)


