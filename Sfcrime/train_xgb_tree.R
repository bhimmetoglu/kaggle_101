# SF Crime data: Modeling with xgboost
# B. Himmetoglu

# Set number of clusters for feature generation
n_clusters <- 25 # 25: This requires a large memory..
euclidean <- FALSE #  distance features ?

# Clean and prepare data
source("clean_and_prepare.R")

# Split back into train and test
train <- full_data %>% filter(is.na(Category) == FALSE)
test <- full_data %>% filter(is.na(Category) == TRUE)

# Remove Resolution for now
# test$Resolution <- NULL
# train$Resolution <- NULL
rm(full_data); gc()

# Model matrices
library(MatrixModels)
Y <- train$Category
# Model matrix with interactions
X <- model.Matrix(~. + PdDistrict * hour +  X * hour + Y * hour + is.intersection * hour, 
                  data = train %>% select(-Category), 
                  sparse = TRUE)[,-1] 

# XGBoost
library(xgboost)

# XGboost wants levels to start with 0
class.names <- levels(Y)
levels(Y) <- 0:38
Y <- as.numeric(levels(Y))[Y]

# Split into train and validation
library(caret)
set.seed(123456789)
idx_train <- createDataPartition(train$Category, p = 0.7)[[1]]
X_tr <- X[idx_train, ]; X_vld <- X[-idx_train, ]
Y_tr <- Y[idx_train]; Y_vld <- Y[-idx_train]

# XGB style matrices
dtrain <- xgb.DMatrix(data = X_tr, label = Y_tr)
dvalid <- xgb.DMatrix(data = X_vld, label = Y_vld)
watchlist <- list(train=dtrain, test=dvalid)

### First train using standard params
# Parameters
params <- list(booster = "gbtree",
               eval_metric = "mlogloss",
               objective = "multi:softprob",
               subsample = 0.6,
               colsample_bytree = 0.6,
               eta = 0.1, 
               min_child_weight = 4,
               max_depth = 6,
               gamma = 0.0,
               seed = 123)
# Fit 
fit_tr <- xgb.train(params = params,
                    data = dtrain,
                    num_class = 39,
                    nrounds = 500,
                    watchlist = watchlist,
                    early_stopping_rounds = 10,
                    maximize = FALSE)


# Importances
importance <- xgb.importance(feature_names = X_tr@Dimnames[[2]], model = fit_tr)
head(importance, 50)
xgb.plot.importance(importance_matrix = head(importance, 25))

##### Final fit on all data
dtrain <- xgb.DMatrix(data = X, label = Y)
X_tst <- model.Matrix(~. + PdDistrict * hour +  X * hour + Y * hour + is.intersection * hour, 
                      data = test %>% select(-Category),
                      sparse = TRUE )[,-1]

dtest <- xgb.DMatrix(data = X_tst)
watchlist <- list(train = dtrain)

# Params
params <- list(booster = "gbtree",
               eval_metric = "mlogloss",
               objective = "multi:softprob",
               subsample = 0.6,
               colsample_bytree = 0.6,
               eta = 0.1, 
               min_child_weight = 4,
               max_depth = 6,
               gamma = 0.0,
               seed = 123)
# Fit 
tree <- xgb.train(params = params,
                    data = dtrain,
                    num_class = 39,
                    nrounds = 500,
                    watchlist = watchlist,
                    early_stopping_rounds = 10,
                    maximize = FALSE)

# Save the model for future use
save(tree, file = "xgb_tree_00.model")

# Predict on test set and submit
pred_tst <- predict(tree, newdata = dtest)
pred_tst_matrix <- matrix(pred_tst, nrow = nrow(X_tst), byrow = TRUE)

# Save for submission
submission <- data.frame(Id = test_ids)
submission <- cbind(submission, as.data.frame(pred_tst_matrix))
colnames(submission) <- c("Id", class.names)
write_csv(submission, path = "./sub_xgb_tree_00.csv")
