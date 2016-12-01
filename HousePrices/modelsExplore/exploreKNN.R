# Burak Himmetoglu
# begin: 11-13-2016
#
# Housing Prices: KNN
#

# Clean and prepare data
#source("stacking/cleanData.R")
#select <- dplyr::select
source("stacking/cleanDataDetailed.R")

# FNN and Caret
library(FNN)
library(caret)

# Folds
set.seed(123)
folds <- createFolds(outcomes, k = 10)

# Cross valiation search
nFolds <- length(folds)
cv.results <- data.frame(k = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), rmse = rep(0,10))
for (ik in 1:dim(cv.results)[1]){ # k
  rmse <- 0 # Set the rmse to zero
  for (iFold in 1:nFolds){ # Folds
    itrain <- unlist(folds[-iFold]); ivalid = unlist(folds[iFold])
    tr <- trainSparse[itrain, ]; vld <- trainSparse[-itrain, ]
    # Fit the model and predict on hold-out (valid)
    mod.knn <- knn.reg(train = tr, test = vld, y = outcomes[itrain], k = cv.results$k[ik])
    pred <- mod.knn$pred
    # Compute RMSE
    rmse <- c(rmse, sqrt(sum( (pred - outcomes[-itrain])^2  )/nrow(vld)) )
  }
  # Evaluate the avegare on all folds
  cat("--- k =", cv.results$k[ik], " rmse =", rmse, "\n")
  cv.results$rmse[ik] <- mean(rmse)
  cat("--- Trained :: ", ik, " of ", dim(cv.results)[1], "\n")
}

# Minimum error
ind.min <- which.min(cv.results$rmse) # k = 5, rmse = 0.1434246



