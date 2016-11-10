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
                 max_depth = 8,
                 min_child_weight = 1,
                 gamma = 0.01,
                 lambda = 1.0,
                 subsample = 0.8)

# SVR
paramsvr <- list(gamma = 1e-5, cost = 1e+4, epsilon = 0.01)

# NNET
paramnnet <- list(size = 20, decay = 0.1)