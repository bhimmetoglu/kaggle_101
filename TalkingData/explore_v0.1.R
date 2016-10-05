# Burak Himmetoglu
# begin: 07-25-2016
#
# Talking Data, exploration and simple predictions
# Computes conditional probabilities P(group|brand) for test and train
# The saved data can be used as features in a later modeling study
#
# Libraries
library(dplyr)
library(tidyr)

# Global define
DEBUG_FLAG = FALSE

# Load data
setwd("~/Works/Rworkspace/TalkingData/")
train <- read.table("./data/gender_age_train.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",",
                    colClasses = c("character", "character", "integer", "character"))  # Training data

train <- train %>% mutate(group = as.factor(group))

# Probability of a random device belonging to age-gender groups
prob_group <- train %>% group_by(group) %>% summarize(tot_by_group = n()) %>%
              mutate(prob_group = tot_by_group / sum(tot_by_group))

# Phone brand data
phone_brand_device_model <- read.table("./data/phone_brand_device_model.csv", stringsAsFactors = FALSE, header = TRUE,
                                       sep = ",", colClasses = c("character","character","character"))

# Get distinct values
# This does not remove duplicated device_id's
phone_brand_device_model <- phone_brand_device_model %>% distinct() 

## Most popular brands by each age/gender group ##
## We only need train and phone_brand_device_model for this ##

# Connect train and phone_brand_device_model
joint_train_brand <- left_join(train,phone_brand_device_model, by="device_id") %>% mutate(group = as.factor(group)) %>% mutate(phone_brand = as.factor(phone_brand))

# There is one duplicated device_id in joint_train_brand
dupl <- duplicated(joint_train_brand$device_id); dupl_id <- joint_train_brand[dupl,1]
dupl_train <- filter(joint_train_brand, device_id == dupl_id)

# For the duplicated id's, choose the one with the brand that has more phones in the data
dim1 <- filter(phone_brand_device_model, phone_brand == dupl_train$phone_brand[1]) %>% select(phone_brand) %>% dim()
dim2 <- filter(phone_brand_device_model, phone_brand == dupl_train$phone_brand[2]) %>% select(phone_brand) %>% dim()
iz <- ifelse(dim1[1] > dim2[1], 2, 1) # dim2[1] is larger

# Remove from train the entry corressponding to dupl_train[1,]
ind.rm <- joint_train_brand$device_id %in% dupl_id & joint_train_brand$phone_brand == dupl_train[iz,5]
ind.rm <- which(ind.rm == TRUE)
joint_train_brand <- joint_train_brand[-ind.rm, ]

# Group_by: group and phone_brand
n_train_brand <- ungroup(joint_train_brand) %>% group_by(group,phone_brand) %>% summarise(tot_by_group_brand = n())

# Total number of phones (all brands)
tot_phones <- sum(n_train_brand$tot_by_group_brand) # Should be equal to dim(train)[1]
if (DEBUG_FLAG){ if (tot_phones != dim(train)[1]) { print("Wrong dimensions!") } }

# Find probability of each brand
prob_brand <- ungroup(n_train_brand) %>% group_by(phone_brand) %>% summarise(tot_brand = sum(tot_by_group_brand)) %>% # Tot phones per brand
              mutate(prob_brand = tot_brand/tot_phones) # Prob of each brand

# Find conditional probability P(group|brand) by Bayes' theorem:
## Join with prob_group & prob_brand then compute conditional probability:
## P(group|brand) = P(brand|group)*P(group)/P(brand)
## ## P(brand|group) = tot_by_group_brand / tot_by_group
## ## P(group) = prob_group & P(brand) = prob_brand
n_train_brand <- ungroup(n_train_brand) %>% left_join(prob_group, by = "group") %>%
                     left_join(prob_brand, by = "phone_brand") %>%
                     mutate(cond_prob = (tot_by_group_brand/tot_by_group) * prob_group / prob_brand)

# Just select the relevant columns
cond_group_brand <- select(n_train_brand,c(group,phone_brand,cond_prob))

# Spread the data: group becomes columns
P_brand_grp <- spread(cond_group_brand,group,cond_prob, fill = 0.0) # fill = 0.0 better than below

if (DEBUG_FLAG){
  # The probabilities must sum to 1 
  which(round(rowSums(P_brand_grp[,-1]),4) != 1)
}

# Join with train and save for later
joint_train_prob <- joint_train_brand %>% left_join(P_brand_grp, by = "phone_brand") %>%
                    select(-c(gender,age,group, device_model,phone_brand))
save(joint_train_prob, file = "train_P.group_brand.RData")

## Given the conditional probability, we can compute for each given device_id, what probability each group has

# Read the test data
test <- read.table("./data/gender_age_test.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",",
                   colClasses = c("character")) 

# Join with brand
joint_test_brand <- left_join(test,phone_brand_device_model, by = "device_id") %>% mutate(phone_brand = as.factor(phone_brand))
joint_test_brand$device_model <- NULL # device model not included in analysis
joint_test_brand <- distinct(joint_test_brand) # Keep disticnt rows

# Check for duplicated device_id's
dupl <- duplicated(joint_test_brand$device_id); dupl_id <- joint_test_brand[dupl,1]
dupl_test <- filter(joint_test_brand, device_id %in% dupl_id)

# For the duplicated id's, choose the one with the brand that has more phones in the data
ind.rm <- c()
for (ind in 1:(dim(dupl_test)[1]/2)){
  i <- (ind-1)*2 + 1
  dim1 <- filter(phone_brand_device_model, phone_brand == dupl_test$phone_brand[i]) %>% select(phone_brand) %>% dim()
  dim2 <- filter(phone_brand_device_model, phone_brand == dupl_test$phone_brand[i+1]) %>% select(phone_brand) %>% dim()
  iz <- ifelse(dim1[1] > dim2[1], 2, 1) # Choose the brand with smaller phones to remove
  l.rm <- joint_test_brand$device_id == dupl_id[ind] & joint_test_brand$phone_brand == dupl_test[iz+(ind-1)*2,2]
  ind.rm <- c(ind.rm, which(l.rm == TRUE))
}

# Finish by removing the repeated elements
joint_test_brand <- joint_test_brand[-ind.rm, ]

# Fill the NA's with 0: 
fill_na <- function(column){
  column[is.na(column)] <- 0
  column
}
# Join with P_brand_grp to get probabilities
joint_test_prob <- left_join(joint_test_brand,P_brand_grp, by = "phone_brand") %>%
                   mutate_each(funs(fill_na)) 
joint_test_prob$phone_brand <- NULL

# There are device_id's with all zero probs, since their brands are not in training set. Put these a value from p_group
l.0 <- which(rowSums(joint_test_prob[,-1]) == 0)
for (ind in 1:length(l.0)){
  for (i in 2:13){
    joint_test_prob[l.0[ind], i] <- prob_group$prob_group[i-1]
  }
}
# Save for later
save(joint_test_prob, file = "test_P.group_brand.RData")

##  Benchmark with prob_group ##
joint_prob_bench <- matrix(0, nrow=112071, ncol=12)
for (ind in 1:12){
  joint_prob_bench[,ind] <- prob_group$prob_group[ind]
}
grp.names <- as.character(prob_group$group)
joint_prob_bench <- cbind(test, joint_prob_bench)
colnames(joint_prob_bench) <- c("device_id",grp.names)

# Write a submission file
write.csv(joint_test_prob, file='submission_phone-brand.csv', row.names=FALSE, quote = FALSE)
write.csv(joint_prob_bench, file='submission_group-bench.csv', row.names=FALSE, quote = FALSE)

