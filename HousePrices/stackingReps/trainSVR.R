# Burak Himmetoglu
# Date: 10-05-2016
#
# Predicting Housing Prices
# Model for SVR training
# 
# Libraries
require(e1071)

# Function for training
trainSVR <- function(inTrain, inPred, param){

  # Separate into training and prediction sets
  Xtr <- as.matrix(trainSparse)[inTrain, ]; Xpred <- as.matrix(trainSparse)[inPred, ]
  
  # Model fit
  #lscaleCols <- trainSparse@Dimnames[[2]] %in% numericCols # These numeric columns need to be scaled
  # Scaling already done
  gamma = param[[1]]; cost = param[[2]]; epsilon = param[[3]]
  mod.svr <- svm(x = Xtr, y = outcomes[inTrain], type = "eps-regression", kernel = "radial",
                 cost = cost, gamma = gamma, epsilon = epsilon, scale = FALSE)
  
  # Predict 
  pred <- predict(mod.svr, newdata = Xpred)
  
  # Return predictions
  pred
  
}

# Function for predicting the test set
testSVR <- function(train, test, param){
  
  # Separate into training and prediction sets
  Xtr <- as.matrix(train); Xts <- as.matrix(test)
  
  # Model fit
  #lscaleCols <- trainSparse@Dimnames[[2]] %in% numericCols # These numeric columns need to be scaled
  # Scaling already done
  gamma = param[[1]]; cost = param[[2]]; epsilon = param[[3]]
  mod.svr <- svm(x = Xtr, y = outcomes, type = "eps-regression", kernel = "radial",
                 cost = cost, gamma = gamma, epsilon = epsilon, scale = FALSE)
  
  # Predict 
  pred <- predict(mod.svr, newdata = Xts)
  
  # Return predictions
  pred
  
}