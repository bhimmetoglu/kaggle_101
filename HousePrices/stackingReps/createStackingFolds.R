# Burak Himmetoglu
# Date: 10-05-2016
#
# Predicting Housing Prices
# Create folds for stacking models
# 
# Libraries
require(caret)

# Function for creating folds
createStacks <- function(nfolds, lStackVerify){
  set.seed(123)
  folds <- createFolds(outcomes, k = nfolds)
  
  if (lStackVerify == TRUE){
    # Separate the folds for training and holdout
    foldsTrain <- folds[1:(nfolds-1)]; foldsHoldOut <- folds[nfolds]
    # Return the list of folds
    list(foldsTrain, foldsHoldOut)  
  } else{
    folds
  }
}

createRepeatedStacks <- function(nfolds, nreps, lStackVerify){
  set.seed(123)
  folds <- createMultiFolds(y = outcomes, k = nfolds, times = nreps)
  # returnTrain = TRUE in this case
  
  if (lStackVerify == TRUE){
    # Separate the folds for training and holdout
    foldsTrain <- folds[-seq(nfolds, nfolds*nreps, nfolds)]; foldsHoldOut <- folds[seq(nfolds, nfolds*nreps, nfolds)]
    # Return the list of folds
    list(foldsTrain, foldsHoldOut)  
  } else{
    folds
  }
}
