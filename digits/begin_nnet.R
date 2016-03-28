# NNet case study
require(caret)
require(nnet)
require(dplyr)
setwd("~/Works/Rworkspace/digits/")


# -------------------------- #
# -----    Common      ----- #
# -------------------------- #


# Read the mnist data on digits
train0 <- read.csv("./data/train.csv")
test0 <- read.csv("./data/test.csv")

# Number of pixels in x and y axes
npxl <- sqrt(dim(train0)[2]-1)

# Display Random Digit (function)
displayDigit <- function(X){  
  # Unroll the pixel matrix (1D to 2D). Each axis has 28 pixels
  m <- matrix(unlist(X),nrow = npxl,byrow = T)  
  # Reverse the order of columns
  m <- t(apply(m, 2, rev))  
  # Display digit
  image(m,col=grey.colors(255))
} 

# Example
displayDigit(train0[21,-1])

# Save labels and then remove it from train (reintroduce after)
y <- train0$label
train0$label <- NULL

# Preprocess
nzv <- nearZeroVar(train0) 
train0 <- train0[, -nzv]
# Do the same for test
test0 <- test0[, -nzv]
train0$label <- y

# Create data partition
inTrain <- createDataPartition(y=train0$label, p=0.7, list=F)
training <- train0[inTrain, ]; CV <- train0[-inTrain, ]

# Preprocess with Caret
y <- training$label; training$label <- NULL
preObj <- preProcess(training, methods=c("scale"))
training <- predict(preObj, newdata = training)
training$label <- y

# Do the same preprocess on test0
test0 <- predict(preObj, newdata = test0)

# Do the same transformation to CV
ycv <- CV$label; CV$label <- NULL
CV <- predict(preObj, newdata = CV)
CV$label <- ycv

# -------------------------- #
# ----- Model training ----- #
# -------------------------- #

# Simple nnet
training.nn <- training
training.nn$label <- class.ind(training$label)

# Neural network structure
input_layer_size = ncol(training)-1 # Initial layer (-1 for label column)
hidden_layer_size = 100 # hidden layer size
output_layer_size = 10 # Number of classes

# Number of weights
N.weights = (input_layer_size+1)*hidden_layer_size + (hidden_layer_size+1)*output_layer_size

nnet0 <- nnet(label ~., data = training.nn, softmax = TRUE, entropy = TRUE, 
              size = hidden_layer_size, MaxNWts = N.weights , maxit = 250, decay = 0.5)

# Predictions
fn.prd0 <- function(X){
  p <- apply(X, MARGIN = 1, FUN = which.max) - 1
  p
}

#
predict.nn <- predict(nnet0, newdata = CV, method = "class")
pred.classes <- fn.prd0(predict.nn)
cm.nn <- confusionMatrix(CV$label, pred.classes)

# Preiction for the test set
predict.test <- predict(nnet0, newdata = test0, method = "class")
pred.classes.test <- fn.prd0(predict.test)
predictions <- data.frame(ImageId = 1:nrow(test0), Label = pred.classes.test)

# Write
write.csv(predictions, file = "predictions.csv", row.names = FALSE)

# ----------------------------- #
# ------- Training w/ CV ------ #
# ----------------------------- #

training.nn <- training
training.nn$label <- class.ind(training$label)

# Predictions
fn.prd0 <- function(X){
  p <- apply(X, MARGIN = 1, FUN = which.max) - 1
  p
}

# Neural network structure
input_layer_size = ncol(training)-1 # Initial layer (-1 for label column)
output_layer_size = 10 # Number of classes
hid_layer_size = c(5, 10) # To be varied (Then try 50, 100, 150)
lambda = c(0.0, 0.1, 0.5, 0.8, 1.0) # regularization parameter (decay)

# Initiate an empty list of predictions
preds.nnet <- list()

# Loop over hid_layer_size and lambda

for (i.layer in 1:length(hid_layer_size)){
  # Set number of weights
  N.weights = (input_layer_size + 1)*hid_layer_size[i.layer] + (hid_layer_size[i.layer] + 1)*output_layer_size 
  
  for (i.lambda in 1:length(lambda)){
    Ip = i.lambda + (length(lambda)) * (i.layer - 1 )
    # Train nnet
    nnet <- nnet(label ~., data = training.nn, softmax = TRUE, entropy = TRUE, 
                  size = hid_layer_size[i.layer], MaxNWts = N.weights , maxit = 250, decay = lambda[i.lambda])
  
    cat("Trained # layers = ", hid_layer_size[i.layer], " decay = ", lambda[i.lambda], "\n")
      
    # Predict
    preds.nnet[[Ip]] <- predict(nnet, newdata = CV, method = "class")
  }
}

# Let's check the predictions
prd.5 <- preds.nnet[1:length(lambda)] # 5 hidden layers
prd.10 <- preds.nnet[(length(lambda)+1):length(preds.nnet)]

# Class predictions on the CV set and error
err.5 <- NULL;
for (i.lambda in 1:length(lambda)){
  prd.clss.5 <- fn.prd0(prd.5[[i.lambda]])
  err.5 <- c(err.5, sum(prd.clss.5 != CV$label))
}
err.5 = err.5 / nrow(CV)

err.10 <- NULL;
for (i.lambda in 1:length(lambda)){
  prd.clss.10 <- fn.prd0(prd.5[[i.lambda]])
  err.5 <- c(err.5, sum(prd.clss.5 != CV$label))
}
err.5 = err.5 / nrow(CV)

# ----------------------------- #
# ----- Training w/ Caret ----- #
# ----------------------------- #
# For some reason, this is not working properly, do the CV by hand ... #


# Neural network structure
#input_layer_size = ncol(training)-1 # Initial layer (-1 for label column)
#output_layer_size = 10 # Number of classes
#hid_layer_size = c(5, 10, 50, 100, 150)

# Grid
#nnet.grid <- expand.grid(decay = c(0.5, 0.1, 0), size = hid_layer_size)

# Control
#ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Create model matrix
#Mnn <- model.matrix(~., data = select(training, -label)) # with make.names(training$label)

#training.nn <- training
#training.nn$label <- class.ind(training$label)

#mod.nnet <- train(x = Mnn, y = class.ind(training$label), method = "nnet",
#                  softmax = TRUE, entropy = TRUE, maxit = 100,
#                  trControl = ctrl, tuneGrid = nnet.grid)

# Simple nnet, no CV by Caret
#training.nn <- training
#training.nn$label <- class.ind(training$label)
#nnet0 <- nnet(label ~., data = training.nn, softmax = TRUE, size = 3, decay = 0.1)


