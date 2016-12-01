# Burak Himmetoglu
# begin: 10-06-2016
#
# Housing Prices: Simple exploration with nnet
#
# Libraries
require(nnet)

# Clean and prepare data
#source("stacking/cleanData.R")
#select <- dplyr::select
source("stacking/cleanDataDetailed.R")

# Grid search for nnet
library(caret)
set.seed(101)
nnet_grid <- expand.grid(decay = c(0,1e-2,0.1,1,10), size = c(10,15,20,25))
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
# folds <- createFolds(outcomes, k = 5)

# Parameters 
input_layer_size <- ncol(trainSparse)
hidden_layer_size_max <- max(nnet_grid[[2]])
Max_NWts <- (input_layer_size+1) * hidden_layer_size_max + (hidden_layer_size_max+1)

# Train by caret
scl <- max(outcomes)
nnet.cv <- train(x = as.matrix(trainSparse), y = outcomes/scl, method = "nnet", maxit = 400,
                 MaxNWts = Max_NWts, linout = TRUE, tuneGrid = nnet_grid, trControl = ctrl)
# 
save(nnet.cv, file = "nnet_cv_2.RData")
# The final values used for the model were size =20 and decay = 0.1 (no reomval of outliers beyond GrLivArea, same after)

# Look at training errors
mod.nnet <- nnet(x = as.matrix(trainSparse), y = outcomes/scl, maxit = 400, size = 20, decay = 0.1,
                 MaxNWts = Max_NWts, linout = TRUE)
# Predict
predTrain <- predict(mod.nnet, newdata = as.matrix(trainSparse))

train <- train %>% mutate(y_actual = outcomes) %>% mutate(y_pred = as.numeric(predTrain)*scl) %>% mutate(diff = abs(y_actual-y_pred))
gg <- ggplot(train, aes(y_actual, y_pred)) + geom_point(aes(x = y_actual, y = y_pred, color = diff)) + 
  geom_abline(slope = 1, intercept = 0); gg
# Worst ones
badPreds <- train %>% filter(diff > 0.3) %>% arrange(desc(diff))
