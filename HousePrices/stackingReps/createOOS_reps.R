# Burak Himmetoglu
# Date: 11-16-2016
#
# Predicting Housing Prices
# Create OOS predictions for 2nd level training
# 

# Is this for verifying the stacking method?
if (lStackVefiry == TRUE){
  nActualFolds <- nfolds - 1
} else{
  nActualFolds <- nfolds
}

# Loop over folds to generate out-of-sample (OOS) predictions
XoosList <- vector("list", nreps); YoosList <- vector("list", nreps)
for (irep in 1:nreps){
  # Determine number of rows in Xoos
  nrowOos <- nActualFolds * nrow(trainSparse) - 
    length(unlist(foldsTrain[(1+(irep-1)*nActualFolds):(irep*nActualFolds)]))

    # Create Xoos and save in the list
  Xoos <- matrix(0, nrow = nrowOos, ncol = nModels+1) # initiate
  Yoos <- data.frame(Id = 0, y = 0)
  XoosList[[irep]] <- Xoos; YoosList[[irep]] <- Yoos
}

# Loop over folds and reps to create out-of-sample predictions
for (irep in 1:nreps){
  ind.start <- 1; ind.end <- 1
  for (ifold in 1:nActualFolds){
    ind <- ifold + nActualFolds * (irep - 1) # Index of the fold for this repetition
    inTrain <- unlist(foldsTrain[ind]) # MultiFolds return training set
    inPred <- -(unlist(foldsTrain[ind]))
    
    # XGBoost
    predMod1 <- trainXGB(inTrain, inPred, paramxgb, nrounds = 826)
    
    # SVR 
    predMod2 <- trainSVR(inTrain, inPred, paramsvr)
    
    # NNET
    predMod3 <- trainNNET(inTrain, inPred, paramnnet)
    
    # Save out-of-sample (OOS) predictions
    lenFold <- nrow(trainSparse) - length(inTrain)
    ind.end <- (ind.start - 1) + lenFold
    XoosList[[irep]][ind.start:ind.end, 1] <- Id.train[inPred] # Save Ids
    XoosList[[irep]][ind.start:ind.end, 2] <- predMod1 # Save predictions from XGB
    XoosList[[irep]][ind.start:ind.end, 3] <- predMod2 # Save predictions from SVR
    XoosList[[irep]][ind.start:ind.end, 4] <- predMod3 # Save predictions from NNET
    
    # Report on screen
    cat("Trained fold ", ifold, " of ", nActualFolds, " :: repetition ", irep, " of ", nreps, "\n")
    
    # Save corresponding actual outcomes: Yoos
    YoosList[[irep]][ind.start:ind.end, 1] <- Id.train[inPred] # Save Ids
    YoosList[[irep]][ind.start:ind.end, 2] <- outcomes[inPred] # Save outcomes
    
    # Increase ind.start
    ind.start <- ind.start + lenFold
  } # ifold
} # irep

# Save in a list of data frames
trainOOSList <- vector("list", nreps)
for (irep in 1:nreps){
  trainOOSList[[irep]] <- as.data.frame(XoosList[[irep]])
  colnames(trainOOSList[[irep]]) <- c("Id", "predXGB", "predSVR", "predNNET")
  YoosList[[irep]] <- mutate(YoosList[[irep]], Id = as.integer(Id))
  trainOOSList[[irep]] <- trainOOSList[[irep]] %>% mutate(Id = as.integer(Id)) %>% left_join(YoosList[[irep]], by = "Id")
}

# Save the OOS predictions 
if (lStackVefiry == TRUE){
  save(trainOOSList, file = "trainOOS-verify.RData")
} else{
  save(trainOOSList, file = "trainOOS.Rdata")
}
