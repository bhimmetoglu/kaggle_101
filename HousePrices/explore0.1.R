# Burak Himmetoglu
# begin: 08-31-2016
#
# Housing Prices: Simple exploration with XGBoost
#
# Libraries
require(dplyr)
require(ggplot2)

# Read train/test data for actvities
setwd("~/Works/Rworkspace/HousePrices")

# Separate outcomes from train
train <- read.csv("./data/train.csv") %>% mutate(is.train = 1)
outcomes <- log(train$SalePrice); train$SalePrice <- NULL

# Combine with test
test <- read.csv("./data/test.csv") %>% mutate(is.train = 0)
fullData <- rbind(train,test) 

# Some integers are actually factors
fullData$MSSubClass <- as.factor(fullData$MSSubClass)
fullData$OverallQual <- as.factor(fullData$OverallQual)
fullData$OverallCond <- as.factor(fullData$OverallCond)

# Get column classes
colClass <- sapply(fullData, class)

# Function for filling NA's in factors
fillNaFactor <- function(icol){
  # Number of each level in a given factor variable 
  temp <- fullData[complete.cases(fullData[,icol]),] %>%group_by_(.dots=colnames(fullData)[icol]) %>% summarize(n()) %>%
          as.data.frame()
  ind.max <- which.max(temp[,2])
  temp[ind.max,1] # Return value
}

# Function for filling NA's in integers
fillNaInteger <- function(icol){
  median(fullData[complete.cases(fullData[,icol]),icol]) # Return the median
}

# Fill NA's
for (icol in 1:ncol(fullData)){
  # Factor variables
  if (colClass[icol] == "factor"){
    l.NA <- is.na(fullData[,icol]) # NA's 

    # Replace NA's with most common level
    if (sum(l.NA) < 100){ # Less than 100 observations is NA
      fullData[l.NA,icol] <- fillNaFactor(icol)
    }
    # If there are too many NA's (i.e. 100 in this case) create a new level
    if (sum(l.NA) > 100){
      levels(fullData[,icol]) <- c(levels(fullData[,icol]), "None")
      fullData[l.NA,icol] <- "None"
    }
  }
  # Integers
  if (colClass[icol] == "integer"){
    l.NA <- is.na(fullData[,icol])
    #fullData[l.NA,icol] <- 0 # Replace NAs in integers with 0
    fullData[l.NA,icol] <- fillNaInteger(icol) # Replace with median
  }
}

# Dskew the columns
deskew <- function(x){
  if (abs(skewness(x)) > 0.5){
    x <- log(1+x)
  }
  x
}

# Deskew numeric columns
fullData <- fullData %>% mutate_at(.cols=colnames(fullData)[colClass == "integer"], funs(deskew))

# # Create a feature matrix: Let's use hashed features
# library(FeatureHashing)

# Predictor names (is.train  and Id is not a predictor) 
predictorNames <- setdiff(names(fullData),c("Id","is.train"))

# Split back to train and test
train <- filter(fullData, is.train == 1) %>% select(-is.train)
test <- filter(fullData, is.train == 0) %>% select(-is.train)

# # Create a hashed model matrix with ? features
# train_hashed <- hashed.model.matrix(~., data=train[,predictorNames], hash.size=2^16, transpose=FALSE)
# test_hashed <- hashed.model.matrix(~., data = test[,predictorNames], hash.size = 2^16, transpose = FALSE)

# Use Sparse Model Matrices
library(Matrix)
trainSparse <- sparse.model.matrix(~., data = train[,predictorNames])[,-1]
testSparse <- sparse.model.matrix(~., data = test[,predictorNames])[,-1]

# XGBoost training
library(xgboost)
library(caret)

# Grid for model training
xgb_grid <- expand.grid(eta = 2^seq(-7,-5), colsample_bytree = c(0.2,0.4),
                        max_depth = c(2,4,8), min_child_weight = c(1,2,4), gamma = c(0,0.01,0.1))

# # xgb style matrices
# dtrain <- xgb.DMatrix(train_hashed,label = outcomes)
# dtest <- xgb.DMatrix(test_hashed)

# xgb style matrices
dtrain <- xgb.DMatrix(trainSparse, label = outcomes)
dtest <- xgb.DMatrix(testSparse)

# Loop over parameters
watchlist <- list(train=dtrain)
cv.results <- data.frame(xgb_grid); cv.results$nrounds = 0; cv.results$rmse = 0
for (ind in 1:dim(xgb_grid)[1]){
  # Model parameters
  eta <- xgb_grid[ind,1]; colsample_bytree <- xgb_grid[ind,2]; max_depth <- xgb_grid[ind,3]
  min_child_weight = xgb_grid[ind,4]; gamma <- xgb_grid[ind,5]
  #
  param <- list(booster="gbtree",
                eval_metric="rmse",
                eta=eta,
                colsample_bytree = colsample_bytree,
                max_depth = max_depth,
                min_child_weight = min_child_weight,
                gamma = gamma,
                lambda = 1.0,
                subsample = 0.8)
  # 5-fold CV
  set.seed(11111)
  fit_cv <- xgb.cv(params=param,
                   data=dtrain,
                   nrounds=1000,
                   watchlist=watchlist,
                   nfold=5,
                   early.stop.round=3)
  cv.results[ind,6] <- as.integer(tail(rownames(fit_cv),1)) # Save nrounds
  cv.results[ind,7] <- tail(fit_cv$test.rmse.mean,1) # Save rmse
  cat("Trained ", ind, " of ", dim(xgb_grid)[1], "\n")
}
# Save cv_results
save(cv.results, file = "xgb_cv.RData")

# which parameters yield minimum rmse?
ind.min <- which.min(cv.results$rmse)

# Reference
# eta            colsample_bytree   max_depth    min_child_weight gamma  nrounds     rmse
# 0.015625              0.4            4                1           0     836      0.131633

# Final Model fit
param <- list(booster="gbtree",
              eval_metric="rmse",
              eta=cv.results[ind.min,1],
              colsample_bytree = cv.results[ind.min,2],
              max_depth = cv.results[ind.min,3],
              min_child_weight = cv.results[ind.min,4],
              gamma = cv.results[ind.min,5],
              lambda = 1.0,
              subsample = 0.8)

mod.xgb <- xgboost(data=dtrain, params = param,watchlist=watchlist,nrounds=cv.results[ind.min,6])

# Predict on test set
predTest <- predict(mod.xgb, newdata = dtest)
Ids <- test$Id # Id numbers

# Data for submission
submission <- data.frame(Id = Ids, SalePrice = exp(predTest))
write.csv(submission,"submission.csv",row.names = FALSE)
