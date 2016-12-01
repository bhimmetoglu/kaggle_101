# Burak Himmetoglu
# begin: 10-06-2016
#
# Housing Prices: Simple exploration with SVM using e1071
#
# Libraries
require(e1071)

# Clean and prepare data
#source("stacking/cleanData.R")
#select <- dplyr::select
source("stacking/cleanDataDetailed.R")

# SVM Grid search
library(caret)
set.seed(101)
svm_grid <- expand.grid(gamma = c(1e-6,1e-5,1e-4, 1e-3, 1e-2), cost = c(1,1e+2,1e+3,1e+4,1e+5), epsilon = c(0.1, 0.01,0.001))
folds <- createFolds(outcomes, k = 5)

# Function for CV
svm.cv <- function(gamma, cost, epsilon){
  rmse <- c() # Initiate
  
  for (ifold in 1:length(folds)){
    itr <- unlist(folds[-ifold])
    Xtrain <- as.matrix(trainSparse)[itr,]
    Xvalid <- as.matrix(trainSparse)[-itr,]
    # Fit SVR
    #lscaleCols <- trainSparse@Dimnames[[2]] %in% numericCols # These numeric columns need to be scaled
    # Already scaled in cleaning
    mod.svr <- svm(x = Xtrain, y = outcomes[itr], type = "eps-regression", kernel = "radial",
                   cost = cost, gamma = gamma, epsilon = epsilon, scale = FALSE)
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

save(cv.results, file="svr_cv5_new.RData")

# Best model
ind.best <- which.min(cv.results$RMSE)
gamma.best <- cv.results[ind.best,1]; cost.best <- cv.results[ind.best,2]; epsilon.best <- cv.results[ind.best,3]
# Reference (no reomval of outliers beyond GrLivArea)
#gamma  cost   epsilon      RMSE
#1e-04  100    0.01    0.1120517
#
# Same parameters after removal of outliers. RMSE becomes 0.1064619

# Final Model fit
mod.svm <- svm(x = as.matrix(trainSparse), y = outcomes, type = "eps-regression", kernel = "radial",
                cost = 100, gamma = 1e-4, epsilon = 0.01)

# Predict
predTest <- predict(mod.svm, newdata = as.matrix(testSparse))

# Data for submission
submission <- data.frame(Id = Id.test, SalePrice = exp(predTest))
write.csv(submission,"submission-svr.csv",row.names = FALSE)

# Look at training errors
predTrain <- predict(mod.svm, newdata = as.matrix(trainSparse))
train <- train %>% mutate(y_actual = outcomes) %>% mutate(y_pred = predTrain) %>% mutate(diff = abs(y_actual-y_pred))
# Worst ones
badPreds <- train %>% filter(diff > 0.3) %>% arrange(desc(diff))

gg <- ggplot(train, aes(y_actual, y_pred)) + geom_point(aes(x = y_actual, y = y_pred, color = diff)) + 
  geom_abline(slope = 1, intercept = 0)+ geom_point(data=badPreds, colour="red") + 
  scale_colour_gradient(limits=c(0, 0.35)) + ggtitle("XGBoost Predictions on Training Set"); 
gg

