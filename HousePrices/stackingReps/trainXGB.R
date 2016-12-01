# Burak Himmetoglu
# Date: 10-05-2016
#
# Predicting Housing Prices
# Model for XGB training
# 
# Libraries
require(xgboost)

# Function for training 
trainXGB <- function(inTrain, inPred, param, nrounds){
  
  # Separate into training and prediction sets
  Xtr <- trainSparse[inTrain, ]; Xpred <- trainSparse[inPred, ]
  
  # xgb style matrices
  dXtr <- xgb.DMatrix(Xtr, label = outcomes[inTrain])
  dXpred <- xgb.DMatrix(Xpred, label = outcomes[inPred])
  
  # Model fit
  mod.xgb <- xgboost(data=dXtr, params = param, nrounds = nrounds, verbose = 0)
  
  # Predict 
  pred <- predict(mod.xgb, newdata = dXpred)
  
  # Return predictions
  pred
}

# Function for predicting the test set 
testXGB <- function(train, test, param, nrounds){
  
  # xgb style matrices
  dXtr <- xgb.DMatrix(train, label = outcomes)
  dXts <- xgb.DMatrix(test)
  
  # Model fit
  mod.xgb <- xgboost(data=dXtr, params = param, nrounds = nrounds, verbose = 0)
  
  # Predict 
  pred <- predict(mod.xgb, newdata = dXts)
  
  # Return predictions
  pred
}
