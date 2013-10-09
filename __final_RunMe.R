options(java.parameters="-Xmx8g")

require(irlba)
require(sqldf)
require(gbm)
require(randomForest)
require(extraTrees)
require(glmnet)



setwd("//home/samba/kaggle_2013_amazon")
source("__final_utils.R")
source("__final_data.R")
source("__final_model.R")
