# Burak Himmetoglu
# Date: 10-13-2016
#
# Predicting Housing Prices
# Main script

## Main parameters
# Number of models and folds
nModels <- 3 # xgb, svr, nnet, #knn
nfolds <- 5

# Is this for verifying the stacking method?
lStackVefiry = TRUE # FALSE

# Clean and prepare data
#source("stacking/cleanData.R")
#select <- dplyr::select
source("stacking/cleanDataDetailed.R")

# Fold creation 
source("stacking/createStackingFolds.R")

# Bring the functions for training models
source("stacking/trainXGB.R")
source("stacking/trainSVR.R")
source("stacking/trainNNET.R")
#source("stacking/trainKNN.R")
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
  predXGB <- trainXGB(inTrain, inHoldOut, paramxgb, nrounds = 836)
  predSVR <- trainSVR(inTrain, inHoldOut, paramsvr)
  predNNET <- trainNNET(inTrain, inHoldOut, paramnnet)
  #predKNN <- trainKNN(inTrain, inHoldOut, paramknn)
  
  # The HoldOut data frame to be used in Level2
  yHoldOut <- outcomes[inHoldOut]
  trainHoldOut <- data.frame(Id = Id.train[inHoldOut], predXGB = predXGB, predSVR = predSVR, predNNET = predNNET,
                         y = yHoldOut)
  
  # Now using the level2 model, predict on HoldOut data
  predStack <- trainLevel2(trainOOS, trainHoldOut, cvfolds = nfolds)
  
  # Calculate RMSE for each model and stacked model
  rmse1 <- sqrt( mean( (yHoldOut - predXGB)^2 ) )
  rmse2 <- sqrt( mean( (yHoldOut - predSVR)^2 ) )
  rmse3 <- sqrt( mean( (yHoldOut - predNNET)^2 ) )
  #rmse4 <- sqrt( mean( (yHoldOut - predKNN)^2 ) )
  rmseStack <- sqrt( mean( (yHoldOut - predStack)^2 ) )
  
  # Print
  cat("--- The rmse values are:: \n")
  c(rmse1,rmse2,rmse3,rmseStack)
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
  predXGB <- testXGB(trainSparse, testSparse, paramxgb, nrounds = 836)
  predSVR <- testSVR(trainSparse, testSparse, paramsvr)
  predNNET <- testNNET(trainSparse, testSparse, paramnnet)
  #predKNN <- testKNN(trainSparse, testSparse, paramknn)
  
  # Now using the level2 model, predict on HoldOut data
  testOOS <- data.frame(Id = Id.test, predXGB = predXGB, predSVR = predSVR, predNNET = predNNET)#, predKNN = predKNN)
  predStack <- trainLevel2(trainOOS, testOOS, cvfolds = nfolds, lFinalFit = TRUE)
  
  # Data for submission
  submission <- data.frame(Id = Id.test, SalePrice = exp(predStack))
  colnames(submission) <- c("Id", "SalePrice")
  write.csv(submission,"submissionStacked.csv",row.names = FALSE)
}
