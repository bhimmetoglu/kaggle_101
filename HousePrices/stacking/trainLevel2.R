# Burak Himmetoglu
# Date: 10-13-2016
#
# Predicting Housing Prices
# Level 2 training

# Load library
library(glmnet)

# Function for training
trainLevel2 <- function(trainOOS, testOOS, cvfolds, lFinalFit = FALSE){
  # By cross validation, train Ridge Regression model
  Xtr <- trainOOS %>% select(-c(Id,y))
  mod.cv <- cv.glmnet(x = as.matrix(Xtr), y = trainOOS$y, nfolds = cvfolds, type.measure = "mse")

  # Use best lambda to predict on testOOS
  lamBest <- mod.cv$lambda.min

  # In Final Fit, the outcomes of testOOS are unknown 
  if (lFinalFit == TRUE){ 
    Xts <- testOOS %>% select(-Id)
  } else{
    Xts <- testOOS %>% select(-c(Id,y))
  }
  # Train and predict
  mod.level2 <- glmnet(x = as.matrix(Xtr), y = trainOOS$y, lambda = lamBest) 
  pred <- predict(mod.level2, newx = as.matrix(Xts))
  pred
}

