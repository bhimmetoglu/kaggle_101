# Burak Himmetoglu
# begin: 08-31-2016
#
# Housing Prices: Simple exploration with XGBoost
#

# Clean and prepare data
source("stacking/cleanData.R")

# XGBoost training
library(xgboost)
library(caret)

# Grid for model training
xgb_grid <- expand.grid(eta = 2^seq(-7,-5), colsample_bytree = c(0.2,0.4),
                        max_depth = c(2,4,8,10), min_child_weight = c(1,2,4), gamma = c(0,0.01,0.1))

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
                   early_stopping_rounds = 3)
  #cv.results[ind,6] <- as.integer(tail(rownames(fit_cv),1)) # Save nrounds
  #cv.results[ind,7] <- tail(fit_cv$test.rmse.mean,1) # Save rmse
  cv.results[ind,6] <- fit_cv$best_iteration
  cv.results[ind,7] <- fit_cv$evaluation_log[fit_cv$best_iteration][[4]]
  cat("Trained ", ind, " of ", dim(xgb_grid)[1], "\n")
}
# Save cv_results
save(cv.results, file = "xgb_cv.RData")

# which parameters yield minimum rmse?
ind.min <- which.min(cv.results$rmse)

# Reference
# eta            colsample_bytree   max_depth    min_child_weight gamma  nrounds     rmse
# 0.015625              0.2            8                1           0.01     811      0.1269202

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

mod.xgb <- xgboost(data=dtrain, params = param, nrounds=cv.results[ind.min,6])

# Predict on test set
predTest <- predict(mod.xgb, newdata = dtest)
Ids <- test$Id # Id numbers

# Data for submission
submission <- data.frame(Id = Ids, SalePrice = exp(predTest))
write.csv(submission,"submission.csv",row.names = FALSE)
