# Burak Himmetoglu
# Date: 11-15-2016
#
# Predicting Housing Prices
# Model for KNN training
# 
# Libraries
require(FNN)

# Function for training 
trainKNN <- function(inTrain, inPred, param){
  
  # Separate into training and prediction sets
  Xtr <- trainSparse[inTrain, ]; Xpred <- trainSparse[inPred, ]
  
  # Model fit
  mod.knn <- knn.reg(train = Xtr, test = Xpred, y = outcomes[inTrain], k = param[[1]])
  
  # Predict 
  pred <- mod.knn$pred
  
  # Return predictions
  pred
}

# Function for predicting the test set 
testKNN <- function(train, test, param){
  
  # Model fit
  mod.knn <- knn.reg(train = train, test = test, y = outcomes, k = param[[1]])
  
  # Predict 
  pred <- mod.knn$pred
  
  # Return predictions
  pred
}
