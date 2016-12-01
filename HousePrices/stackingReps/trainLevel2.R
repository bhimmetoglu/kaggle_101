# Burak Himmetoglu
# Date: 10-13-2016
#
# Predicting Housing Prices
# Level 2 training

# Load libraries
library(glmnet)
library(caret)

## Functions for training level2 
trainLevel2 <- function(trainOOS, testOOS, cvfolds, lFinalFit = FALSE){
  # By cross validation, train Ridge Regression model
  Xtr <- trainOOS %>% select(-c(Id,y))

  # Values of alpha
  alpha_values = c(0, 1e-6, 1e-5, 1e-4, 1e-3, 0.01, 0.1, 1)
  cv.results <- data.frame(alpha = alpha_values, lambda = 0, rmse = 0)
  for (ial in 1:length(alpha_values)){
    mod.cv <- cv.glmnet(x = as.matrix(Xtr), y = trainOOS$y, nfolds = cvfolds, type.measure = "mse",
                        alpha = alpha_values[ial])
    # Determine lambda
    lam <- mod.cv$lambda.min; ind.lam <- which(mod.cv$lambda == lam) 
    
    # Store CV results
    cv.results[ial, ]$lambda <- mod.cv$lambda.min; cv.results[ial, ]$rmse <- sqrt( mod.cv$cvm[ind.lam] )
  }
  
  # Best model
  ind.best <- which.min(cv.results$rmse)
  alBest <- cv.results[ind.best, 1]
  lamBest <- cv.results[ind.best, 2]

  # In Final Fit, the outcomes of testOOS are unknown 
  if (lFinalFit == TRUE){ 
    Xts <- testOOS %>% select(-Id)
  } else{
    Xts <- testOOS %>% select(-c(Id,y))
  }
  # Train and predict
  mod.level2 <- glmnet(x = as.matrix(Xtr), y = trainOOS$y, lambda = lamBest, alpha = alBest) 
  pred <- predict(mod.level2, newx = as.matrix(Xts))
  pred
}

## SVR
trainLevel2_svr <- function(trainOOS, testOOS, cvfolds, lFinalFit = FALSE){
  # By cross validation, train Ridge Regression model
  Xtr <- trainOOS %>% select(-c(Id,y))

  # Grid for CV
  set.seed(101)
  svm_grid <- expand.grid(gamma = c(1e-8,1e-7,1e-6,1e-5,1e-4), cost = c(1e+4,1e+5,1e+6), epsilon = c(0.1, 0.01,0.001))
  folds <- createFolds(trainOOS$y, k = cvfolds)
    
  # Function for CV
  svm.cv <- function(gamma, cost, epsilon){
    rmse <- c() # Initiate
    # Loop over folds
    for (ifold in 1:length(folds)){
      itr <- unlist(folds[-ifold])
      Xtrain <- as.matrix(Xtr)[itr,]; Xvalid <- as.matrix(Xtr)[-itr,]
      # Fit SVR
      mod.svr <- svm(x = Xtrain, y = trainOOS$y[itr], type = "eps-regression", kernel = "radial",
                     cost = cost, gamma = gamma, epsilon = epsilon, scale = FALSE)
      
      # Predict on validation set
      predValid <- predict(mod.svr, newdata = Xvalid)
      
      # Compute RMSE
      errMetric <- sqrt(mean((predValid-trainOOS$y[-itr])^2)) # Compute RMSE
      rmse <- c(rmse,errMetric) # Append
    }
    # Return the rmse vector
    rmse
  }
  
  # Loop over the svm_grid to screen best hyperparameters
  cv.results <- data.frame(svm_grid); cv.results$RMSE = 0
  for (ind in 1:dim(svm_grid)[1]){
    gamma <- svm_grid[ind,1]; cost <- svm_grid[ind,2]; epsilon <- svm_grid[ind,3]
    # Run CV
    rmse <- svm.cv(gamma = gamma, cost = cost, epsilon = epsilon)
    # Save the mean to cv.results
    cv.results[ind,4] <- mean(rmse)
  }
  
  # Best model
  ind.best <- which.min(cv.results$RMSE)
  gamma.best <- cv.results[ind.best,1]; cost.best <- cv.results[ind.best,2]; epsilon.best <- cv.results[ind.best,3]
   
  # In Final Fit, the outcomes of testOOS are unknown 
  if (lFinalFit == TRUE){ 
    Xts <- testOOS %>% select(-Id)
  } else{
    Xts <- testOOS %>% select(-c(Id,y))
  }

  # Train and predict
  mod.svm <- svm(x = as.matrix(Xtr), y = trainOOS$y, type = "eps-regression", kernel = "radial",
                 cost = cost.best, gamma = gamma.best, epsilon = epsilon.best)
  
  # Predict and return
  pred <- predict(mod.svm, newdata = as.matrix(Xts))
  pred
}

## KNN
trainLevel2_knn <- function(trainOOS, testOOS, cvfolds, lFinalFit = FALSE){
  # By cross validation, train KNN regression
  Xtr <- trainOOS %>% select(-c(Id,y))
  
  # Values of k
  cv.results <- data.frame(k = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), rmse = rep(0,10))

  # Create folds
  folds <- createFolds(trainOOS$y, k = cvfolds)
  
  # Run cross validation
  for (ik in 1:dim(cv.results)[1]){ 
    rmse <- 0 
    for (iFold in 1:cvfolds){
      itrain <- unlist(folds[-iFold]); ivalid = unlist(folds[iFold])
      tr <- trainOOS[itrain, ]; vld <- trainOOS[-itrain, ]
      
      # Fit the model and predict on hold-out (valid)
      mod.knn <- knn.reg(train = tr, test = vld, y = trainOOS$y[itrain], k = cv.results$k[ik])
      pred <- mod.knn$pred
      # Compute RMSE
      rmse <- c(rmse, sqrt( mean( (pred - trainOOS$y[-itrain])^2 ) ) )
    }
    
    # Evaluate the avegare on all folds
    cv.results$rmse[ik] <- mean(rmse)
  }
  # Best result
  ind.best <- which.min(cv.results$rmse)
  
  # In Final Fit, the outcomes of testOOS are unknown 
  if (lFinalFit == TRUE){ 
    Xts <- testOOS %>% select(-Id)
  } else{
    Xts <- testOOS %>% select(-c(Id,y))
  }
  
  # Predict on testOOS and return
  mod.knn <- knn.reg(train = Xtr, test = Xta, y = trainOOS, k = cv.results$k[ind.best])
  pred <- mod.knn$pred
  pred
}

# ## Plot
# gg <- ggplot(trainOOS, aes(y)) + geom_point(aes(x = y, y = predXGB, color = "predXGB")) + 
#   geom_point(aes(x = y, y = predSVR, color = "predSVR")) + 
#   geom_point(aes(x = y, y = predNNET, color = "predNNET")) + 
#   geom_abline(slope = 1, intercept = 0) + labs(x = "Actual", y = "Prediction") + 
#   guides(title = "Model")
# gg
