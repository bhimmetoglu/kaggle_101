# Burak Himmetoglu
# Date: 10-05-2016
#
# Predicting Housing Prices
# Clean and preprocess the training and testing sets
# 
# Libraries
require(dplyr)
require(ggplot2)
require(readr)
require(e1071)
require(Matrix)

# Read train/test data for actvities
#setwd("~/Works/Rworkspace/HousePrices")

# Separate outcomes from train
train <- read.csv("./data/train.csv") %>% mutate(is.train = 1)
outcomes <- log(train$SalePrice); train$SalePrice <- NULL

# Combine with test
test <- read.csv("./data/test.csv") %>% mutate(is.train = 0)
fullData <- rbind(train,test) 

# Some integers are actually factors
fullData$MSSubClass <- as.factor(fullData$MSSubClass)
fullData$OverallQual <- as.factor(fullData$OverallQual)
fullData$OverallCond <- as.factor(fullData$OverallCond)

# Get column classes
colClass <- sapply(fullData, class)

# Function for filling NA's in factors
fillNaFactor <- function(icol){
  # Number of each level in a given factor variable 
  temp <- fullData[complete.cases(fullData[,icol]),] %>%group_by_(.dots=colnames(fullData)[icol]) %>% summarize(n()) %>%
    as.data.frame()
  ind.max <- which.max(temp[,2])
  temp[ind.max,1] # Return value
}

# Function for filling NA's in integers
fillNaInteger <- function(icol){
  median(fullData[complete.cases(fullData[,icol]),icol]) # Return the median
}

# Fill NA's using the functions above
for (icol in 1:ncol(fullData)){
  # Factor variables
  if (colClass[icol] == "factor"){
    l.NA <- is.na(fullData[,icol]) # NA's 
    
    # Replace NA's with most common level
    if (sum(l.NA) < 100){ # Less than 100 observations is NA
      fullData[l.NA,icol] <- fillNaFactor(icol)
    }
    # If there are too many NA's (i.e. 100 in this case) create a new level
    if (sum(l.NA) > 100){
      levels(fullData[,icol]) <- c(levels(fullData[,icol]), "None")
      fullData[l.NA,icol] <- "None"
    }
  }
  # Integers
  if (colClass[icol] == "integer"){
    l.NA <- is.na(fullData[,icol])
    #fullData[l.NA,icol] <- 0 # Replace NAs in integers with 0
    fullData[l.NA,icol] <- fillNaInteger(icol) # Replace with median
  }
}

# Dskew the columns
deskew <- function(x){
  if (abs(skewness(x)) > 0.5){
    x <- log(1+x)
  }
  x
}

# Rescale columns
rescale <- function(x) { (x-mean(x))/sd(x) }

# Save locations of numeric columns
numericCols <- setdiff(names(colClass[colClass == "integer" | colClass == "numeric"]),c("Id","is.train"))

# Deskewand standardize numeric columns
fullData <- fullData %>% mutate_at(.cols=numericCols, funs(deskew)) %>% mutate_at(.cols=numericCols, funs(rescale))

# Split back to train and test
train <- filter(fullData, is.train == 1) %>% select(-is.train)
test <- filter(fullData, is.train == 0) %>% select(-is.train)
Id.test <- test$Id # Id numbers for test set
Id.train <- train$Id

# Predictor names (is.train  and Id is not a predictor) 
predictorNames <- setdiff(names(fullData),c("Id","is.train"))

# Use Sparse Model Matrices
trainSparse <- sparse.model.matrix(~., data = train[,predictorNames])[,-1]
testSparse <- sparse.model.matrix(~., data = test[,predictorNames])[,-1]

# Check unique elements in columns of training Matrix
lenUniq <- function(x) {length(unique(x))}
numUniq <- apply(trainSparse, 2, lenUniq)
rmCol <- which(numUniq == 1) # Remove this column
trainSparse <- trainSparse[,-12]; testSparse <- testSparse[,-12]

# Remove unnecessary variables
rm(fullData,colClass,icol,l.NA,fillNaFactor,fillNaInteger,deskew); gc()
