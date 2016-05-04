# Santander data (Random Forest CV full data)
# vfull_dat
# Author: Burak H
#
require(dplyr)
require(caret)
require(randomForest)
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

# For RF, we pretty much need a balanced set for training
nmin <- sum(training$TARGET == "1")
nmax <- sum(training$TARGET == "0")
nfolds <- floor(nmax/nmin) # Number of data separations 

# Folds to use
folds <- createFolds(1:nmax, k = nfolds)

# All 0's and all 1's data
temp.0 <- filter(training, TARGET == "0")
temp.1 <- filter(training, TARGET == "1")

# Balanced data used for predictions
tr.bal <- rbind(temp.1, temp.0[folds[[1]], ])

# Training with Caret
rf_grid <- expand.grid(mtry = c(10, 20, 30, 40, 50, 60, 100))

ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary,
                     verboseIter = TRUE)

mod.rf <- train(x = select(tr.bal, -TARGET), y = ifelse(tr.bal$TARGET == "1", "y", "n"),
                method = "rf", trControl = ctrl, metric = "ROC", tuneGrid = rf_grid, ntree = 1000)

# mtry = 40 in the first round of RF
# Second round of CV
# Training with Caret
rf_grid <- expand.grid(mtry = c(32, 34, 36, 38, 40, 42, 44, 46, 48))

ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary,
                     verboseIter = TRUE)
set.seed(101)
mod.rf <- train(x = select(tr.bal, -TARGET), y = ifelse(tr.bal$TARGET == "1", "y", "n"),
                method = "rf", trControl = ctrl, metric = "ROC", tuneGrid = rf_grid, ntree = 1000)

# In this one mtry = 38 is found..