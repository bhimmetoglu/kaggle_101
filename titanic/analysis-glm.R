# Titanic Survival Analysis
# Function:
#     Read/clean train (clean.R)
# 
# Burak H. 
library(caret); library(dplyr)

# Go to working directory
setwd("~/Coursera/projects/titanic")

# Read the training and testing sets
train0 <- read.csv("./data/train.csv")
test0 <- read.csv("./data/test.csv")

# Get the cleaned data
fun.clean <- dget("clean.R")
cleaned <- fun.clean(train0,test0)
tr0 <- cleaned[[1]]; ts0 <- cleaned[[2]]

# Further split the tr0 data into training and testing sets by Caret
in.tr0 <- createDataPartition(tr0$Survived, p=0.7, list = FALSE)
training <- tr0[in.tr0, ] 
testing <- tr0[-in.tr0, ]

# Train several glm models to training set
modFit0 <- train(Survived ~ Sex + Age + Fare + binaryCabin + fam.size,
                method = "glm", family = "binomial", data = training)
#
modFit1 <- train(Survived ~ Sex * Age + Fare + binaryCabin + fam.size,
                method = "glm", family = "binomial", data = training)
#
modFit2 <- train(Survived ~ Sex * Age + Pclass + binaryCabin + fam.size,
                 method = "glm", family = "binomial", data = training)

# Predictions on the training set (choose ModFit1)
pred.tr <- ifelse(modFit2$finalModel$fitted.values > 0.5, 1, 0)

# Predict on the testing set
pred0 <- predict(modFit1, newdata = testing)

# Confusion Matrices
cm.test <- confusionMatrix(pred0, testing$Survived)
cm.train <- confusionMatrix(pred.tr, training$Survived)

# View the tables
cm.test$table
cm.train$table

# Now predict on the actual testing set (ts0)
pred.ts0 <- predict(modFit1, newdata = ts0)

# Select Passanger ID and Survived to write into final table
final <- data.frame(Survived = pred.ts0, PassengerId = ts0$PassengerId)
write.csv(final, file = "predictions.csv", row.names = FALSE, quote = FALSE)
