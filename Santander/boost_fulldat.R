# Santander data (xgboost CV full data)
# vfull_dat
# Author: Burak H
#
require(dplyr)
require(caret)
require(xgboost)
require(e1071)
setwd("~/Works/Rworkspace/Santender/")

# Read data
train0 <- read.csv("train.csv")
test0 <- read.csv("test.csv")

# Get the cleaned and processed data
setwd("~/Works/Rworkspace/Santender/final/")
fun.clean <- dget("clean_process.R") 
cleaned <- fun.clean(train0,test0) 
y <- cleaned[[1]]; train0 <- cleaned[[2]]; test0 <- cleaned[[3]]
test0.id <- cleaned[[4]]
rm(cleaned)

train0$TARGET <- y # Put TARGET back

# Create training and validation sets for cross-validation
set.seed(101)
inTrain <- createDataPartition(y = train0$TARGET, p = 0.7, list = FALSE)
training <- train0[inTrain, ]; validation <- train0[-inTrain, ]

#--------------------------#
# Fit XGBOOST with Caret
#--------------------------#

# # Old grid
# xgb_grid <- expand.grid(eta = 2^seq(-6,-4), colsample_bytree = c(0.4, 0.6, 0.8), nrounds = 2^(8:9), max_depth = 5, 
#                         min_child_weight = c(0,1,2), gamma = c(0,2,4,6))

# Initial grid
# xgb_grid <- expand.grid(eta = c(0.001,0.01,0.1,1,10), 
#                         colsample_bytree = c(0.4, 0.6, 0.8, 1.0), 
#                         nrounds = 2^(8:9), 
#                         max_depth = 5,
#                         min_child_weight = c(0,1,2), 
#                         gamma = c(0.001,0.01,1,10))

# Fitting nrounds = 512, max_depth = 5, eta = 0.1, gamma = 10, colsample_bytree = 0.6, 
# min_child_weight = 1 on full training set: auc = 0.8411

# Try more targeted grid 
xgb_grid <- expand.grid(eta = 2^seq(-6,-4), colsample_bytree = c(0.4, 0.6, 0.8),
                        nrounds = c(350, 400, 450, 500, 550), max_depth = 5,
                        min_child_weight = c(0,1,2), gamma = c(2,4,6,8,10))

ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary,
                     verboseIter = TRUE)

mod.xgb <- train(x = select(training, -TARGET), y = ifelse(training$TARGET == "1", "y", "n"),
                 method = "xgbTree", trControl = ctrl, metric = "ROC", tuneGrid = xgb_grid,
                 nthread = 8)


# 5-fold CV result was:
# Fitting nrounds = 400, max_depth = 5, eta = 0.0312, gamma = 4, colsample_bytree = 0.4, min_child_weight = 1 
# auc = 0.8414
# Note: 10-fold does not make a big difference.

# Save model
#xgb.save(mod.xgb$finalModel, "mod-xgb")

# Predictions
validation$pred <- predict(mod.xgb$finalModel, newdata = as.matrix(select(validation, -TARGET)), outputmargin = FALSE)

# ROC
library(pROC)
roc.curve <- roc(validation$TARGET, validation$pred)
plot(roc.curve)

# Confusion Matrix
#cm <- confusionMatrix(ifelse(validation$pred < 0.5, 1, 0), validation$TARGET) # 0 is positive class

# ---- FINAL FIT ---- #
# Now, let us fit a boosting model on the whole train0 set and predict test0

dtrain <- xgb.DMatrix(data = as.matrix(select(train0, -TARGET)),
                      label = train0$TARGET)

params <- list(booster="gbtree", objective = "binary:logistic",
               max_depth = 5, eta = 2^(-5), colsample_bytree = 0.4,
               subsample = 1, gamma = 4, min_child_weight = 1)

mod.xgb <- xgboost(params = params, data = dtrain, 
                     nrounds = 400, nthread = 8, subsample = 1,
                     print.every.n = 10 )

#--------------------------#
# Prediction for test0
#--------------------------#
pred <- predict(mod.xgb, newdata = as.matrix(test0))
final <- data.frame(ID = test0.id, TARGET = pred)
write.csv(final,"boost_deskewed_42116.csv", row.names = FALSE)
