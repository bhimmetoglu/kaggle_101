# Burak Himmetoglu
# Date: 10-13-2016
#
# Predicting Housing Prices
# Main script

## Main parameters
# Number of models and folds
nModels <- 3
nfolds <- 5

# Is this for verifying the stacking method?
lStackVefiry = FALSE

# Clean and prepare data
source("stacking/cleanData.R")

# Fold creation 
source("stacking/createStackingFolds.R")

# Bring the functions for training models
source("stacking/trainXGB.R")
source("stacking/trainSVR.R")
source("stacking/trainNNET.R")
source("stacking/modelParams.R")

# Predict on the HoldOut fold, if stacking is verified
if (lStackVefiry == TRUE){
  # Create Folds
  listFolds <- createStacks(nfolds, lStackVefiry); foldsTrain <- listFolds[[1]]; foldsHoldOut <- listFolds[[2]]
  
  # Create the OOS samples
  source("stacking/createOOS.R")
  
  # Bring in the level2 training function
  source("stacking/trainLevel2.R")
  
  # Predictions
  inTrain <- unlist(foldsTrain); inHoldOut <- unlist(foldsHoldOut)
  predXGB <- trainXGB(inTrain, inHoldOut, paramxgb, nrounds = 811)
  predSVR <- trainSVR(inTrain, inHoldOut, paramsvr)
  predNNET <- trainNNET(inTrain, inHoldOut, paramnnet)
  
  # The HoldOut data frame to be used in Level2
  yHoldOut <- outcomes[inHoldOut]
  trainHoldOut <- data.frame(Id = Id.train[inHoldOut], predXGB = predXGB, predSVR = predSVR, predNNET = predNNET,
                         y = yHoldOut)
  
  # Now using the level2 model, predict on HoldOut data
  predStack <- trainLevel2(trainOOS, trainHoldOut, cvfolds = 5)
  
  # Calculate RMSE for each model and stacked model
  rmse1 <- sqrt( mean( (yHoldOut - predXGB)^2 ) )
  rmse2 <- sqrt( mean( (yHoldOut - predSVR)^2 ) )
  rmse3 <- sqrt( mean( (yHoldOut - predNNET)^2 ) )
  rmseStack <- sqrt( mean( (yHoldOut - predStack)^2 ) )
}

# 
if (lStackVefiry == FALSE){
  # Create folds
  foldsTrain <- createStacks(nfolds, lStackVefiry)
  
  # Create the OOS samples
  source("stacking/createOOS.R")
  
  # Bring in the level2 training function
  source("stacking/trainLevel2.R")
  
  # Predictions
  predXGB <- testXGB(trainSparse, testSparse, paramxgb, nrounds = 811)
  predSVR <- testSVR(trainSparse, testSparse, paramsvr)
  predNNET <- testNNET(trainSparse, testSparse, paramnnet)
  
  # Now using the level2 model, predict on HoldOut data
  testOOS <- data.frame(Id = Id.test, predXGB = predXGB, predSVR = predSVR, predNNET = predNNET)
  predStack <- trainLevel2(trainOOS, testOOS, cvfolds = 5, lFinalFit = TRUE)
  
  # Data for submission
  submission <- data.frame(Id = Id.test, SalePrice = exp(predStack))
  colnames(submission) <- c("Id", "SalePrice")
  write.csv(submission,"submissionStacked.csv",row.names = FALSE)
}