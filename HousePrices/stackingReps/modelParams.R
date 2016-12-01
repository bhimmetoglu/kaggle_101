# Burak Himmetoglu
# Date: 10-05-2016
#
# Predicting Housing Prices
# Model parameters determined from the whole training data by CV
# 

## Model parameters
# XGBoost
paramxgb <- list(booster = "gbtree",
                 eval_metric = "rmse",
                 eta = 0.015625,
                 colsample_bytree = 0.2,
                 max_depth = 4,
                 min_child_weight = 2,
                 gamma = 0.0,
                 lambda = 1.0,
                 subsample = 0.8)

# SVR
paramsvr <- list(gamma = 1e-4, cost = 100, epsilon = 0.01)

# NNET
paramnnet <- list(size = 20, decay = 0.1, maxit = 400) # 

# KNN
paramknn <- list(k = 8)
