# Burak Himmetoglu
# Date: 10-05-2016
#
# Predicting Housing Prices
# Model for NNET training
# 
# Libraries
require(nnet)

# Function for training 
trainNNET <- function(inTrain, inPred, param){
  
  # Separate into training and prediction sets
  Xtr <- trainSparse[inTrain, ]; Xpred <- trainSparse[inPred, ]
  
  # Model fit
  input_layer_size <- ncol(trainSparse)
  hidden_layer_size_max <- param[[1]]
  Max_NWts <- (input_layer_size+1) * hidden_layer_size_max + (hidden_layer_size_max+1)
  scl <- max(outcomes)
  mod.nnet <- nnet(x = as.matrix(Xtr), y = outcomes[inTrain]/scl, decay = param[[2]], size = param[[1]],
                   maxit = 400, MaxNWts = Max_NWts, linout = TRUE)
  
  # Predict 
  pred <- predict(mod.nnet, newdata = Xpred)*scl
  
  # Return predictions
  pred
}

# Function for predicting the test set
testNNET <- function(train, test, param){
  
  # Separate into training and prediction sets
  Xtr <- as.matrix(train); Xts <- as.matrix(test)
  
  # Model fit
  input_layer_size <- ncol(train)
  hidden_layer_size_max <- param[[1]]
  Max_NWts <- (input_layer_size+1) * hidden_layer_size_max + (hidden_layer_size_max+1)
  scl <- max(outcomes)
  mod.nnet <- nnet(x = Xtr, y = outcomes/scl, decay = param[[2]], size = param[[1]],
                   maxit = 400, MaxNWts = Max_NWts, linout = TRUE)
  
  # Predict 
  pred <- predict(mod.nnet, newdata = Xts)*scl
  
  # Return predictions
  pred
}
