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


