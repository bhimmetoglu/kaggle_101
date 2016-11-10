# Burak Himmetoglu
# Date: 10-05-2016
#
# Predicting Housing Prices
# Create OOS predictions for and level training
# 

# Is this for verifying the stacking method?
if (lStackVefiry == TRUE){
  nActualFolds <- nfolds - 1
} else{
  nActualFolds <- nfolds
}

# Loop over folds to generate out-of-sample (OOS) predictions
nrowOos <- length(unlist(foldsTrain))
Xoos <- matrix(0, nrow = nrowOos, ncol = nModels+1) # initiate 
Yoos <- data.frame(Id = 0, y = 0)

# Loop over folds to create out-of-sample predictions
ind.start <- 1; ind.end <- 1
for (ifold in 1:nActualFolds){
  inTrain <- unlist(foldsTrain[-ifold]) # Indices of data used for training
  inPred <- unlist(foldsTrain[ifold]) # Indicaes of data used for predicting (oos)
  
  # XGBoost 
  predMod1 <- trainXGB(inTrain, inPred, paramxgb, nrounds = 811)
  
  # SVR 
  predMod2 <- trainSVR(inTrain, inPred, paramsvr)
  
  # NNET
  predMod3 <- trainNNET(inTrain, inPred, paramnnet)
  
  # Save out-of-sample (OOS) predictions 
  ind.end <- (ind.start - 1) + length(inPred)
  Xoos[ind.start:ind.end, 1] <- Id.train[inPred] # Save Ids
  Xoos[ind.start:ind.end, 2] <- predMod1 # Save predictions from XGB
  Xoos[ind.start:ind.end, 3] <- predMod2 # Save predictions from SVR
  Xoos[ind.start:ind.end, 4] <- predMod3 # Save predictions from NNET
  
  # Report on screen
  cat("Trained fold ", ifold, " of ", nActualFolds, " for OOS sample \n")
  
  # Save corresponding actual outcomes: Yoos
  Yoos[ind.start:ind.end, 1] <- Id.train[inPred] # Save Ids
  Yoos[ind.start:ind.end, 2] <- outcomes[inPred] # Save outcomes
  
  # Increase ind.start
  ind.start <- ind.start + length(inPred)
}

# Save in data frame
trainOOS <- as.data.frame(Xoos)
colnames(trainOOS) <- c("Id", "predXGB", "predSVR", "predNNET")
Yoos <- mutate(Yoos, Id = as.integer(Id))
trainOOS <- trainOOS %>% mutate(Id = as.integer(Id)) %>% left_join(Yoos, by = "Id")

# Save the OOS predictions 
save(trainOOS, file = "trainOOS.RData")
