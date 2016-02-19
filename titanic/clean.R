# Function for cleaning training data
library(dplyr); library(stringr)
clean.tr <- function (train, test){
  # Cleans and processes the training and testing sets (input are data.frames)

  ##### TRAIN ####  
  train <- mutate(train, Survived = factor(Survived))
  
  # Replace missing embark locations with the maximum 
  if (nrow(train[train$Embarked == "", ]) > 0){
    df.emb <- as.data.frame(table(train$Embarked))
    max.emb <- as.character(df.emb[which.max(df.emb[,2]),1])
    train[train$Embarked == "", ]$Embarked <- max.emb
  }
  
  # Get missing age information from Titles
  titles <- character(length = nrow(train))
  for (i in 1:nrow(train)){
    temp <- strsplit(as.character(train$Name[i]), split="\\.")[[1]][1]
    titles[i] <- str_trim(strsplit(temp, split = ",")[[1]][2], "left")
  }
  # New data frame with a column = title
  tr2 <- mutate(train, Title = as.factor(titles))
  
  # Find the median of the ages of passangers by Title
  uniq.titles <- unique(titles)
  med.ages <- data.frame(Title = uniq.titles, Med.Age = 0)
  for (ind in 1:length(uniq.titles)){
    med.ages[ind,2] <- median(tr2[tr2$Title == uniq.titles[ind], ]$Age, na.rm = TRUE)
    # Check for na's and replace with medians
    nr.na <- nrow(tr2[is.na(tr2$Age) & tr2$Title == uniq.titles[ind], ])
    if (nr.na > 0){
      tr2[is.na(tr2$Age) & tr2$Title == uniq.titles[ind], ]$Age <- med.ages[ind,2]
    }
  }
  
  # Compute the median Fares for each Pclass
  m3 <-  median(tr2[tr2$Pclass == 3, ]$Fare)
  m2 <-  median(tr2[tr2$Pclass == 2, ]$Fare)
  m1 <-  median(tr2[tr2$Pclass == 1, ]$Fare)
  
  # Save the Passanger ID's of passangers with missing Fares for each Pclass
  cl3 <- tr2[tr2$Fare == 0 & tr2$Pclass == 3,]$PassengerId
  cl2 <- tr2[tr2$Fare == 0 & tr2$Pclass == 2,]$PassengerId
  cl1 <- tr2[tr2$Fare == 0 & tr2$Pclass == 1,]$PassengerId
  
  # Put median Fares for missing ones
  for (i in seq(1,length(cl3)) ) { tr2[tr2$PassengerId == cl3[i], ]$Fare <- m3 }
  for (i in seq(1,length(cl2)) ) { tr2[tr2$PassengerId == cl2[i], ]$Fare <- m2 }
  for (i in seq(1,length(cl1)) ) { tr2[tr2$PassengerId == cl1[i], ]$Fare <- m1 }
  
  # Number of family members (SibSp & Parch)
  tr2 <- mutate(tr2, fam.size = Parch + SibSp)
  
  # Create a factor variable: 1/0 passanger (has)/(has not) cabininfo
  tr2 <- mutate(tr2, binaryCabin = 1)
  tr2[tr2$Cabin == "", ]$binaryCabin = 0
  tr2 <- mutate(tr2, binaryCabin = factor(binaryCabin))
  
  ##### TEST ####
  # In test data, passangers in rows 267 and 373 has 0 Fare and they are in Pclass 1
  # Row 153 has NA in Pclass 3
  test[267, 9] <- m1; test[373, 9] <- m1; test[153, 9] <- m3
  
  # Get all the titles in test data
  titles <- character(length = nrow(test))
  for (i in 1:nrow(test)){
    temp <- strsplit(as.character(test$Name[i]), split="\\.")[[1]][1]
    titles[i] <- str_trim(strsplit(temp, split = ",")[[1]][2], "left")
  }
  # New data frame with a column = title
  ts2 <- mutate(test, Title = as.factor(titles))

  #Replce NA's with median age (this comes from train)
  uniq.titles <- unique(titles)
  for (ind in 1:length(uniq.titles)){
    # Check for na's and replace with medians
    nr.na <- nrow(ts2[is.na(ts2$Age) & ts2$Title == uniq.titles[ind], ])
    if (nr.na > 0){
    med.age <- med.ages[med.ages$Title == uniq.titles[ind], ]$Med.Age
    ts2[is.na(ts2$Age) & ts2$Title == uniq.titles[ind], ]$Age <- med.age
    }
  }
  
  # Number of family members (SibSp & Parch)
  ts2 <- mutate(ts2, fam.size = Parch + SibSp)

  # Create a factor variable: 1/0 passanger (has)/(has not) cabininfo
  ts2 <- mutate(ts2, binaryCabin = 1)
  ts2[ts2$Cabin == "", ]$binaryCabin = 0
  ts2 <- mutate(ts2, binaryCabin = factor(binaryCabin))
      
  # Return the data.frames tr2 and ts2
  list(tr2,ts2)
}
