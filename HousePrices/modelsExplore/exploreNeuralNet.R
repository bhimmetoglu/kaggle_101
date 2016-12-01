# Burak Himmetoglu
# begin: 11-13-2016
#
# Housing Prices: NeuralNet
#

# Clean and prepare data
#source("stacking/cleanData.R")
select <- dplyr::select
source("stacking/cleanDataDetailed.R")

# Libraries
library(caret)
library(neuralnet)

# Split into training and validation sets
trainSparse <- cbind(trainSparse, outcomes)

# Construct the data and the formula
n <- trainSparse@Dimnames[[2]]
featNames <- n[!n %in% "outcomes"]
featNamesNew <- paste0("feat", 1:length(featNames))
train <- as.matrix(trainSparse); colnames(train) <- c(featNamesNew,"outcomes")
f <- as.formula(paste("outcomes ~", paste(featNamesNew, collapse = "+")))

# Model training
set.seed(123)
itrain <- createDataPartition(outcomes, times = 1, p = 0.7)
tr <- train[itrain[[1]], ]; vld <- train[-itrain[[1]], ]

# Train nnet
nn <- neuralnet(f,data=tr, hidden=c(50,25), linear.output=T)

# Predict on vld
# compute is like the predict method for neuralnet
pr.nn <- compute(nn,vld[,1:287]) # Do not include outcome

# Accuracy metric
RMSE.nn <- sqrt( sum((vld[,288] - pr.nn$net.result)^2)/nrow(vld) )

## Where is the regularization??

## Try cv
nFolds <- 5
set.seed(123)
folds <- createFolds(outcomes, k = nFolds)
RMSE <- c()
for (ifold in 1:nFolds){
  iTrain <- unlist(folds[-ifold]); iValid <- unlist(folds[ifold])
  tr <- train[iTrain, ]; vld <- train[-iTrain, ]
  nn <- neuralnet(f,data=tr, hidden=c(50,25), linear.output=T) # Fit
  pr.nn <- compute(nn,vld[,1:287]) # Predict
  # Accuracy metric
  RMSE.nn <- sqrt( sum((vld[,288] - pr.nn$net.result)^2)/nrow(vld) )
  RMSE <- c(RMSE, RMSE.nn)
  cat("--- Trained :: ", ifold, " of ", nFolds, "\n")
}
# Mean of RMSE results
mean(RMSE)
