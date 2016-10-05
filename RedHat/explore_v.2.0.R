# Burak Himmetoglu
# begin: 08-04-2016
#
# RedHat: Simple data generation for analysis
#
# Debug flag
debugFlag = TRUE 

# Libraries
options(stringsAsFactors = FALSE)
require(data.table)

# Read train/test data for actvities
setwd("~/Works/Rworkspace/RedHat")
train <- fread("./data/act_train.csv")
test <- fread("./data/act_test.csv")

# Combine train and test into one data
train[,is.train := 1]; test[,c("is.train","outcome"):= list(0,NA)]
activity <- rbind(train,test)
rm(train,test); gc()

# People data (change TRUE & FALSE to 1 and 0)
people <- fread("./data/people.csv")
cols <- paste0("char_", 10:37)
people[, (cols) := lapply(.SD,as.integer) ,.SDcols = cols]

# Rename the columns to prevent confusion between activity chars and people chars
# activity
cols <- colnames(activity)[5:14]
paste_activity <- function(c) { paste0("act:", c) }
cols <- sapply(cols, paste_activity)
colnames(activity)[5:14] <- cols

# people
cols <- colnames(people)[c(2,4,6:41)]
paste_people <- function(c){ paste0("ppl:", c) }
cols <- sapply(cols, paste_people)
colnames(people)[c(2,4,6:41)] <- cols

# Clean
rm(cols,paste_people, paste_activity); gc()

##### Merge activity and people #####
# Drop date from each
activity[, date := NULL]; people[, date := NULL]

# Merge
act_ppl <- merge(activity,people,by="people_id", all.x = TRUE)
rm(activity,people); gc()

# Drop people_id, since we don't need it anymore
act_ppl[, people_id := NULL]

# Check for empty entries
if (debugFlag){
  # A function for testing empty entries
  is_empty <- function(c){ c == ""}

  # Number of empty entries in act:char_1 to act:char_10
  act.cols <- colnames(act_ppl)[3:12]
  tmp <- act_ppl[, lapply(.SD, is_empty), .SDcols = act.cols]
  n_empty.act <- tmp[, lapply(.SD, sum), .SDcols = act.cols]
  rm(tmp); gc()

  # Number of empty entries in ppl:char_1 to ppl:char_9
  ppl.cols <- colnames(act_ppl)[15:53]
  tmp <- act_ppl[, lapply(.SD, is_empty), .SDcols = ppl.cols]
  n_empty.ppl <- tmp[, lapply(.SD, sum), .SDcols = ppl.cols] # n_empty.ppl is all zeros!!
  rm(tmp); gc()
} # There are empty entries in act:char's but no empty entries in ppl:char's

# Maybe, one can replace empty levels with type_0
act.cols <- colnames(act_ppl)[3:12]
replace_empty <- function(c){ c[c == ""] = "type 0"; c }
act_ppl[, (act.cols) := lapply(.SD, replace_empty), .SDcols = act.cols]

## Now, we can create a model matrix
# Split back to train and test
X <- act_ppl[, -(c("is.train", "outcome","activity_id")), with = FALSE]
id.train <- act_ppl$is.train == 1
y.train <- act_ppl$outcome[id.train]
act_id <- act_ppl$activity_id

require(Matrix)

# Rename colums, get rid of "_" and "_" (sparse.model.matrix does not like such column names)
repUnderScore <- function(c){ c <- gsub("_","",c); c <- gsub(":","",c); c }
colnames(X) <- repUnderScore(colnames(X))

# Create the sparse.model.matrix
Xmm <- sparse.model.matrix(~.-1, X) # Warnings are OK, just conversion from char to factor
warnings()

# Test XGBoost
require(xgboost)
dtrain <- xgb.DMatrix(Xmm[id.train,],label = y.train)
param <- list(booster="gblinear",
              objective="binary:logistic",
              eval_metric="auc",
              eta=0.03)
              # max_depth = 5,
              # subsample = 0.7,
              # colsample_bytree = 0.3,
              # min_child_weight = ,
              # gamma = 1,
              # alpha = 0.0)

set.seed(1011)
watchlist <- list(train=dtrain)
mod.xgb_cv <- xgb.cv(params=param,
                 data=dtrain,
                 nrounds=500,
                 watchlist=watchlist,
                 nfold=5,
                 early.stop.round=3)

# Predict on test set
dtest = xgb.DMatrix(Xmm[!id.train,])
mod.xgb <- xgboost(params=param,
                  data=dtrain,
                  nrounds=173,
                  watchlist=watchlist,
                  early.stop.round=3)

pred.test <- predict(mod.xgb, newdata = dtest)
act_id.test <- act_id[!id.train]

# Final and submission
final <- data.frame(activity_id = act_id.test, outcome = pred.test)
write.csv(final,"xgb_trial.csv", row.names = FALSE)
