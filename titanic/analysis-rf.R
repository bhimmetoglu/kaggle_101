# Titanic Survival Analysis (Random Forest version)
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

# Train using random forests
ctrl <- trainControl(allowParallel=T, method = "repeatedcv", number = 10, repeats = 10)
modFit <- train(Survived ~ Sex + Age + Pclass + SibSp + Parch + Embarked + Fare + Title,
                method = "rf", importance = TRUE, data = training, ntree = 500,
                trControl = ctrl)

# Predictions on the training set
pred.tr <- predict(modFit, newdata = training)

# Predict on the testing set
pred0 <- predict(modFit, newdata = testing)

# Confusion Matrices
cm.test <- confusionMatrix(pred0, testing$Survived); cm.test$table
cm.train <- confusionMatrix(pred.tr, training$Survived); cm.train$table

# Now predict on the actual testing set (ts0)
pred.ts0 <- predict(modFit, newdata = ts0)

# Select Passanger ID and Survived to write into final table
final <- data.frame(Survived = pred.ts0, PassengerId = ts0$PassengerId)
write.csv(final, file = "predictions_rf.csv", row.names = FALSE, quote = FALSE)

##### Boosting ####
ctrl <- trainControl(method="boot")
mod.gbm <- train(Survived ~ Sex + Age + Pclass + SibSp + Parch + Embarked + Fare,
                method = "gbm", data = training,
                trControl = ctrl, verbose = FALSE)

# Predictions on the training set
pred.tr.gbm <- predict(mod.gbm, newdata = training)

# Predict on the testing set
pred.ts.gbm <- predict(mod.gbm, newdata = testing)

# Confusion Matrices
cm.test <- confusionMatrix(pred0, testing$Survived); cm.test$table
cm.train <- confusionMatrix(pred.tr, training$Survived); cm.train$table

# Select Passanger ID and Survived to write into final table
final <- data.frame(Survived = pred.ts0, PassengerId = ts0$PassengerId)
write.csv(final, file = "predictions_rf.csv", row.names = FALSE, quote = FALSE)
