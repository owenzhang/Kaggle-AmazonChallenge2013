options(java.parameters="-Xmx8g")

require(irlba)
require(sqldf)
require(gbm)
require(randomForest)
require(extraTrees)
require(glmnet)



setwd("//[where train.csv and test.csv are]")
source("__final_utils.R")
source("__final_data.R")
source("__final_model.R")
