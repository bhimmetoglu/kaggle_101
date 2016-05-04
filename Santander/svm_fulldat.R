# Santander data (SVM CV full data)
# vfull_dat
# Author: Burak H
#
require(dplyr)
require(caret)
require(randomForest)
require(e1071)
setwd("~/Works/Rworkspace/Santender/")
#setwd("~/Works/Rworkspace/Santatnder/")

# Read data
train0 <- read.csv("train.csv")
test0 <- read.csv("test.csv")

# Get the cleaned and processed data
setwd("~/Works/Rworkspace/Santender/final/")
#setwd("~/Works/Rworkspace/Santatnder/final/")
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

# For SVM, we pretty much need a balanced set for training
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

# Train with Caret
# First round: Fitting sigma = 0.001, C = 100 on full training set
#svm_grid <- expand.grid(sigma = c(0.0001, 0.001, 0.1, 1.0), C = c(0.001, 0.01, 0.1, 1, 5, 10, 100))

# Second round: Fitting sigma = 0.002, C = 10 on full training set
svm_grid <- expand.grid(sigma = c(0.0005, 0.001, 0.002, 0.005), C = c(1, 10, 20, 50, 100, 150))

ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary,
                     verboseIter = TRUE)

mod.svm <- train(x = select(tr.bal, -TARGET), y = ifelse(tr.bal$TARGET == "1", "y", "n"),
                method = "svmRadial", trControl = ctrl, metric = "ROC", tuneGrid = svm_grid, 
                scaled = FALSE, prob.model = TRUE)

pred <- predict(mod.svm$finalModel, newdata = select(validation,-TARGET), type="prob")[,1]

# Plot a ROC curve
library(pROC)
roc.curve <- roc(validation$TARGET, pred)
plot(roc.curve)

# # --- Simple TEST --- # 
# mod.svm0 <- svm(TARGET~., data = mutate(training, TARGET = factor(TARGET)), kernel = 'radial',
#                 gamma = 1, cost = 1, decision.values = TRUE)
