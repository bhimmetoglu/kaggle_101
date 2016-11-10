# Burak Himmetoglu
# begin: 10-06-2016
#
# Housing Prices: Simple exploration with SVM using e1071
#
# Libraries
require(e1071)

# Clean and prepare data
source("stacking/cleanData.R")

# SVM Grid search
library(caret)
set.seed(101)
svm_grid <- expand.grid(gamma = c(1e-8,1e-7,1e-6,1e-5,1e-4), cost = c(1e+4,1e+5,1e+6), epsilon = c(0.1, 0.01,0.001))
folds <- createFolds(outcomes, k = 5)

# Function for CV
svm.cv <- function(gamma, cost, epsilon){
  rmse <- c() # Initiate
  
  for (ifold in 1:length(folds)){
    itr <- unlist(folds[-ifold])
    Xtrain <- as.matrix(trainSparse)[itr,]
    Xvalid <- as.matrix(trainSparse)[-itr,]
    # Fit SVR
    lscaleCols <- trainSparse@Dimnames[[2]] %in% numericCols # These numeric columns need to be scaled
    mod.svr <- svm(x = Xtrain, y = outcomes[itr], type = "eps-regression", kernel = "radial",
                   cost = cost, gamma = gamma, epsilon = epsilon, scale = lscaleCols)
    # Predict on validation set
    predValid <- predict(mod.svr, newdata = Xvalid)
    # Compute RMSE
    errMetric <- sqrt(mean((predValid-outcomes[-itr])^2)) # Compute RMSE
    rmse <- c(rmse,errMetric) # Append
    # Write report
    cat("Model :: ", "gamma = ", gamma, "cost = ", cost, "epsilon = ", epsilon, ":: RMSE = ", errMetric, "\n")
  }
  # Return the rmse vector
  rmse
}

# Loop over the svm_grid to screen best hyperparameters
cv.results <- data.frame(svm_grid); cv.results$RMSE = 0
for (ind in 1:dim(svm_grid)[1]){
  gamma <- svm_grid[ind,1]; cost <- svm_grid[ind,2]; epsilon <- svm_grid[ind,3]
  # Run fivefold CV
  rmse <- svm.cv(gamma = gamma, cost = cost, epsilon = epsilon)
  # Save the mean to cv.results
  cv.results[ind,4] <- mean(rmse)
}

save(cv.results, file="svr_cv5.RData")

# Best model
ind.best <- which.min(cv.results$RMSE)
gamma.best <- cv.results[ind.best,1]; cost.best <- cv.results[ind.best,2]; epsilon.best <- cv.results[ind.best,3]
# Reference
#gamma  cost   epsilon      RMSE
#1e-05  10000    0.01    0.1258002

# Final Model fit
mod.svm <- svm(x = as.matrix(trainSparse), y = outcomes, type = "eps-regression", kernel = "radial",
                cost = 1e+4, gamma = 1e-5, epsilon = 0.01)

# Predict
predTest <- predict(mod.svm, newdata = as.matrix(testSparse))

# Data for submission
submission <- data.frame(Id = Ids, SalePrice = exp(predTest))
write.csv(submission,"submission-svr.csv",row.names = FALSE)
