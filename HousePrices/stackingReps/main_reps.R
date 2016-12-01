# Burak Himmetoglu
# Date: 11-16-2016
#
# Predicting Housing Prices
# Main script

## Main parameters
# Number of models and folds with nreps
nModels <- 3  # xgb, svr, nnet
nfolds <- 10  # 10 folds of stacking
nreps <- 10    # 5 repetitions of the 10 fold stacks

# Is this for verifying the stacking method?
lStackVefiry = TRUE

# Clean and prepare data
#source("stacking/cleanData.R")
#select <- dplyr::select
source("stackingReps/cleanDataDetailed.R")

# Fold creation 
source("stackingReps/createStackingFolds.R")

# Bring the functions for training models
source("stackingReps/trainXGB.R")
source("stackingReps/trainSVR.R")
source("stackingReps/trainNNET.R")
source("stackingReps/modelParams.R")

# Predict on the HoldOut fold, if stacking is verified
if (lStackVefiry == TRUE){
  # Create Folds
  listFolds <- createRepeatedStacks(nfolds, nreps, lStackVefiry); foldsTrain <- listFolds[[1]]; foldsHoldOut <- listFolds[[2]]
  
  # Create the OOS samples
  source("stackingReps/createOOS_reps.R")
  
  # Bring in the level2 training function
  source("stackingReps/trainLevel2.R")
  
  # Stack models (i.e. predict on HoldOut)
  reps.data <- matrix(0, nrow = nreps, ncol = nModels+1) # Initiate
  trainHOList <- vector("list", nreps)
  for (irep in 1:nreps){
    inHoldOut <- -(unlist(foldsHoldOut[irep]))
    inTrain <- -inHoldOut

    # Predictions for each model
    predXGB <- trainXGB(inTrain, inHoldOut, paramxgb, nrounds = 914)
    predSVR <- trainSVR(inTrain, inHoldOut, paramsvr)
    predNNET <- trainNNET(inTrain, inHoldOut, paramnnet)
    
    # The HoldOut data frame to be used in Level2
    yHoldOut <- outcomes[inHoldOut]
    trainHoldOut <- data.frame(Id = Id.train[inHoldOut], predXGB = predXGB, predSVR = predSVR, predNNET = predNNET,
                               y = yHoldOut)
    
    # Now using the level2 model, predict on HoldOut data
    predStack <- trainLevel2(trainOOSList[[irep]], trainHoldOut, cvfolds = nfolds)
    
    # Save on the HoldOut data
    trainHoldOut <- trainHoldOut %>% mutate(yStack = as.numeric(predStack))
    trainHOList[[irep]] <- trainHoldOut
    
    # Calculate RMSE for each model and the stacked model
    rmse1 <- sqrt( mean( (yHoldOut - predXGB)^2 ) )
    rmse2 <- sqrt( mean( (yHoldOut - predSVR)^2 ) )
    rmse3 <- sqrt( mean( (yHoldOut - predNNET)^2 ) )
    rmseStack <- sqrt( mean( (yHoldOut - as.numeric(predStack))^2 ) )
    
    # Print
    cat("--- Repetition : ", irep, " of ", nreps, "\n")
    reps.data[irep, 1] <- rmse1; 
    reps.data[irep, 2] <- rmse2; 
    reps.data[irep, 3] <- rmse3; 
    reps.data[irep, 4] <- rmseStack
  }
  # Convert reps.data into a data.frame
  reps.data <- as.data.frame(reps.data); names(reps.data) <- c("rmse1", "rmse2", "rmse3", "rmseStack")
  
  # Save on file
  save(reps.data, file = "repsData.RData")
  save(trainHOList, file = "trainHO.Rdata")
}

lStackVefiry <- FALSE
best.stack <- which.min(reps.data$rmseStack) # The best stack

if (lStackVefiry == FALSE){
  # Create folds
  foldsTrain <- createRepeatedStacks(nfolds, nreps, lStackVefiry)
  
  # Create the OOS samples
  source("stackingReps/createOOS_reps.R")
  
  # Bring in the level2 training function
  source("stackingReps/trainLevel2.R")
  
  # Predictions
  predXGB <- testXGB(trainSparse, testSparse, paramxgb, nrounds = 914)
  predSVR <- testSVR(trainSparse, testSparse, paramsvr)
  predNNET <- testNNET(trainSparse, testSparse, paramnnet)

  # Now using the level2 model, predict on HoldOut data
  testOOS <- data.frame(Id = Id.test, predXGB = predXGB, predSVR = predSVR, predNNET = predNNET)
  
  # Predict for each repetition
  predStack <- matrix(0, nrow = dim(testOOS)[1], ncol = nreps)
  for (irep in 1:nreps){
    predStack[, irep] <- trainLevel2(trainOOSList[[irep]], testOOS, cvfolds = nfolds, lFinalFit = TRUE)
  }

  # Average over all repeated stacks or use the best stack
  predStackAvg <- apply(predStack, 1, mean)
  # predStackBest <- predStack[, best.stack]
  
  # Data for submission
  submission <- data.frame(Id = Id.test, SalePrice = exp(predStackAvg))
  colnames(submission) <- c("Id", "SalePrice")
  write.csv(submission,"submissionStackedReps.csv",row.names = FALSE)
  
#   submission2 <- data.frame(Id = Id.test, SalePrice = exp(predStackBest))
#   colnames(submission2) <- c("Id", "SalePrice")
#   write.csv(submission2,"submissionStackedReps2.csv",row.names = FALSE)
}