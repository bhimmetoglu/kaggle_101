# Burak Himmetoglu
# begin: 11-13-2016
#
# Housing Prices: Simple exploration with XGBoost (linear)
#

# Clean and prepare data
#source("stacking/cleanData.R")
source("stacking/cleanDataDetailed.R")

# XGBoost training
library(xgboost)
library(caret)

# Grid for model training
xgb_grid <- expand.grid(eta = 2^seq(-7,-5), lambda = c(0,0.01,0.1,1), alpha = c(0,0,01,0.1,1),
                        lambda_bias = c(0,0.01,0.1,1),min_child_weight = c(1,2,4))

# xgb style matrices
dtrain <- xgb.DMatrix(trainSparse, label = outcomes)
dtest <- xgb.DMatrix(testSparse)

# Loop over parameters
watchlist <- list(train=dtrain)
cv.results <- data.frame(xgb_grid); cv.results$nrounds = 0; cv.results$rmse = 0
for (ind in 1:dim(xgb_grid)[1]){
  # Model parameters
  eta <- xgb_grid[ind,1]; lambda <- xgb_grid[ind,2]; alpha <- xgb_grid[ind,3]
  lambda_bias <- xgb_grid[ind,4]
  #
  param <- list(booster="gblinear",
                eval_metric="rmse",
                eta=eta,
                lambda = lambda,
                alpha = alpha,
                lambda_bias = lambda_bias,
                subsample = 0.8)
  # 5-fold CV
  set.seed(11111)
  fit_cv <- xgb.cv(params=param,
                   data=dtrain,
                   nrounds=1000,
                   watchlist=watchlist,
                   nfold=5,
                   early_stopping_rounds = 3)
  
  cv.results[ind,5] <- fit_cv$best_iteration
  cv.results[ind,6] <- fit_cv$evaluation_log[fit_cv$best_iteration][[4]]
  cat("Trained ", ind, " of ", dim(xgb_grid)[1], "\n")
}
# Save cv_results
save(cv.results, file = "xgblinear_cv.RData")

# which parameters yield minimum rmse?
ind.min <- which.min(cv.results$rmse)

# Reference
# eta       lambda alpha   lambda_bias  nrounds      rmse
# 0.03125      1     0           0        1000     0.2404744

# Final Model fit
param <- list(booster="gblinear",
              eval_metric="rmse",
              eta=0.03125,
              lambda = 1.0,
              alpha = 0.0,
              lambda_bias = 0.0,
              min_child_weight = 1,
              subsample = 0.8)

mod.xgb <- xgboost(data=dtrain, params = param, nrounds=1000)

# Predict on test set
predTest <- predict(mod.xgb, newdata = dtest)
Ids <- test$Id # Id numbers

# Data for submission
submission <- data.frame(Id = Ids, SalePrice = exp(predTest))
write.csv(submission,"submission.csv",row.names = FALSE)

# Feature importances
importance <- xgb.importance(feature_names = trainSparse@Dimnames[[2]], model = mod.xgb)
head(importance,10)
