# Santander data: nnet on full data
# Author: Burak H
#
library(dplyr); library(caret); library(nnet); library(e1071)
library(Matrix)
setwd("~/Works/Rworkspace/Santender/")
#setwd("~/Coursera/projects/santender/")

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
training <- train0[inTrain, ]; testing <- train0[-inTrain, ]

# -------------------------#
# ------ Train NNET ------ #
# ------------------------ #

# Let us determine the decay and hidden_layer_size by cross-validation using caret
input_layer_size = ncol(training)-1 # Initial layer (-1 for label column)
output_layer_size = 2 # Number of classes
max_hidden_layer_size = 15 # Maximum value of hidden layer size

# Maximum number of weights
N.weights = (input_layer_size+1)*max_hidden_layer_size + (max_hidden_layer_size+1)*output_layer_size

# # --- CAUTION: This takes days!!! --- #
# 
# # Grid 
# nnet.grid <- expand.grid(decay = c(0, 1, 2, 4), size = c(5, 10, 50))
# 
# # Train control
# ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary,
#                      verboseIter = TRUE)
# 
# # Model training
# mod.nnet <- train(x = select(training, -TARGET), y = ifelse(training$TARGET == "1", "y", "n"),
#                   method = "nnet", trControl = ctrl, metric = "ROC", tuneGrid = nnet.grid,
#                   MaxNWts = N.weights , maxit = 1000)
# 
# # After training, save the model
# save(mod.nnet, file = "nnet_final.Rdata")
# 
# # ---END CAUTION ---- #

# Since we have learned that size = 5 and decay = 2 works for the reduced data, let's fix size = c(4, 5)
# and try to tune decay in the full data

# Grid 
#nnet.grid <- expand.grid(decay = c(1, 2, 4), size = c(4,5))
nnet.grid <- expand.grid(decay = c(2,4), size = c(5, 10, 15))

# Train control
ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary,
                      verboseIter = TRUE)
 
# Model training
mod.nnet <- train(x = select(training, -TARGET), y = ifelse(training$TARGET == "1", "y", "n"),
                   method = "nnet", trControl = ctrl, metric = "ROC", tuneGrid = nnet.grid,
                   MaxNWts = N.weights , maxit = 1000)
# Size 5 decay 2 is bestTune, try smaller size too

# --------- #

# Neural network structure
input_layer_size = ncol(training)-1 # Initial layer (-1 for label column)
hidden_layer_size = 5 # hidden layer size, from bestTune
output_layer_size = 2 # Number of classes
dec = 2.0 # From best tune

# # Number of weights
# N.weights = (input_layer_size+1)*hidden_layer_size + (hidden_layer_size+1)*output_layer_size
# 
# # Need to determine hidden_layer_size and decay by CV
# mod.nnet <- nnet(TARGET ~., data = mutate(training, TARGET = factor(TARGET)), softmax = FALSE, 
#               size = hidden_layer_size, MaxNWts = N.weights , maxit = 1000, decay = 1.0)

pred.nnet <- predict(mod.nnet$finalModel, newdata = testing)
#pred.class <- as.numeric(ifelse(pred.nnet0 < 0.5, 0, 1))
#cm.nnet0 <- confusionMatrix(validation.nn$TARGET, pred.class)

# ROC
library(pROC)
ROC <- roc(response = testing$TARGET, predictor = as.numeric(pred.nnet))
plot(ROC)
