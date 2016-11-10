# Burak Himmetoglu
# begin: 10-06-2016
#
# Housing Prices: Simple exploration with SVM using e1071
#
# Libraries
require(nnet)

# Clean and prepare data
source("stacking/cleanData.R")

# Grid search for nnet
# SVM Grid search
library(caret)
set.seed(101)
nnet_grid <- expand.grid(decay = c(0,1e-2,0.1,10,100), size = c(10,20))
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
save(nnet.cv, file = "nnet_cv.RData")
# The final values used for the model were size = 20 and decay = 0.1
