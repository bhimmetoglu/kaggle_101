# Burak Himmetoglu
# begin: 08-03-2016
#
# Talking Data, blahblah
#
# Libraries
options(stringsAsFactors=F)
require(data.table)

# Setwd
setwd("~/Works/Rworkspace/TalkingData/by_data_table/")

# Get data
train <- fread("../data/gender_age_train.csv", colClasses=c("character","character","integer","character"))
test <- fread("../data/gender_age_test.csv",colClasses=c("character"))

# Add NA's to group, age and gender for label test
test[,c("gender","age","group"):= NA] 

# Combine test & train
full_data <- rbind(train,test)
rm(test,train); gc() # Remove unnecessary data. Test can be reconstructed by is.na

# Get phone brands and models
brand_model <- fread("../data/phone_brand_device_model.csv",colClasses=c("character","character","character"))

# Unique entries in brand_model
brand_model <- unique(brand_model,by=NULL) # Unique entries

# Combine phone_brand and device_model
brand_model <- brand_model[,brand_model := paste(phone_brand,device_model,sep="-")]
brand_model <- brand_model[, brand_model := as.factor(brand_model)]
levs <- levels(brand_model[,brand_model]); n.levs <- length(levs)

# Change the levels of brand_model to numeric values
levels(brand_model$brand_model) <- 0:(n.levs-1) 

# Remove uunnecessary columns
brand_model <- brand_model[,c("phone_brand", "device_model") := NULL]

# Merge full_data and brand_model
brand_model_group <- merge(full_data, brand_model, by = "device_id", all.x = TRUE)
rm(full_data,brand_model); gc() # Remove data that is not needed

# Duplicated device_id's
dupl <- duplicated(brand_model_group$device_id); dupl_id <- brand_model_group[dupl,device_id]
dupl_data <- brand_model_group[device_id %in% dupl_id,]

# For the duplicated id's, choose the one with the brand that has more phones in the data
ind.rm <- c()
for (ind in 1:(dim(dupl_data)[1]/2)){
  i <- (ind-1)*2 + 1
  i1 <- brand_model_group[brand_model == dupl_data$brand_model[i],]
  i2 <- brand_model_group[brand_model == dupl_data$brand_model[i+1],]
  dim1 <- length(i1[,brand_model]); dim2 <- length(i2[,brand_model])
  iz <- ifelse(dim1 > dim2,2,1) # Choose the brand_model with smaller phones to remove
  l.rm <- brand_model_group$device_id == dupl_id[ind] & brand_model_group$brand_model == dupl_data[iz+(ind-1)*2,brand_model]
  ind.rm <- c(ind.rm, which(l.rm == TRUE))
}
rm(i1,i2,iz,dim1,dim2,dupl_data,dupl,dupl_id); gc() # Remove unused data

# Remove duplicated data
brand_model_group <- brand_model_group[-ind.rm,]

# Read apps data
events <- fread("../data/events.csv", colClasses=c("character","character","character","numeric","numeric"))
event_app <- fread("../data/app_events.csv",colClasses=rep("character",4))

# Labels
#app_labels <- fread("../data/app_labels.csv", colClasses = c("character","integer"))
#label_categories <- fread("../data/label_categories.csv")

# Get unique events for device_id - event_id combination, and subset with (device_id,event_id)
events <- unique(events[,.(device_id,event_id)],by=NULL)

# Join app_labels and event_app (is this useful?)
#N_app_label <- app_labels[,.N,by="app_id"] # How many of different labels does an app belong to ?

# For each event_id, list unique app_id's s separated by commas
event_apps <- event_app[,.(apps=paste(unique(app_id),collapse=",")),by="event_id"]

# Combine this with events
device_event_apps <- merge(events,event_apps,by="event_id")
rm(events,event_app,event_apps);gc()

# For each app by device_id get unique apps (Notice that now we group_by device_id not event_id!)
f_split_paste <- function(z){paste(unique(unlist(strsplit(z,","))),collapse=",")}
device_apps <- device_event_apps[,.(apps=f_split_paste(apps)),by="device_id"]

rm(device_event_apps,f_split_paste);gc()

# Spread the data (device_id, app_id in many rows)
tmp <- strsplit(device_apps$apps,",") # Temporary list for storage apps (i.e. their IDs). Contains 60822 lists

# device_id and app_is spread 
device_apps <- data.table(device_id=rep(device_apps$device_id,times=sapply(tmp,length)),app_id=unlist(tmp))
rm(tmp)

# Combine the brand_model_group data and the device_apps data
d1 <- brand_model_group[,list(device_id,brand_model)]
brand_model_group$brand_model <- NULL
d2 <- device_apps
rm(device_apps)

# Initiate factor variables
d1[,brand_model:=paste0("brand_model:",brand_model)]
d2[,app_id:=paste0("app_id:",app_id)]
names(d1) <- names(d2) <- c("device_id","feature_name") # Just rename

# Combine
dd <- rbind(d1,d2)
rm(d1,d2);gc()

# Find unique device_id's and spread the features
require(Matrix)
ii <- unique(dd$device_id)
jj <- unique(dd$feature_name)

# Locations of unique entries in dd
id_i <- match(dd$device_id,ii) # max(id_i) = length(ii)
id_j <- match(dd$feature_name,jj) # max(id_j) = length(jj)

# Collect the locations
id_ij <- cbind(id_i,id_j)

# Initiate matrix for storage (dimensions are unique entries in device_id and feature_names)
M <- Matrix(0,nrow=length(ii),ncol=length(jj),
            dimnames=list(ii,jj),sparse=T)

# At locations where elements are, put 1: Factor variables
M[id_ij] <- 1 
# Notice that dim(id_ij) > dim(M). However, indices in id_ij are repeated ( max(id_i) = length(ii) & max(id_j) = length(jj) )
# meaning that no offshoot of matrix M indices can occur

rm(ii,jj,id_i,id_j,id_ij,dd);gc()
# M has device_id's in rows and phone_brands in cols

# Some more wrangling
x <- M[rownames(M) %in% brand_model_group$device_id,] # Subset rows of M so that it matches brand_model$device_id
id <- brand_model_group$device_id   #[match(label1$device_id,rownames(x))]
y <- brand_model_group$group  #[match(label1$device_id,rownames(x))]
rm(M,brand_model_group)

# Training data
x_train <- x[!is.na(y),] # Y has NAs for test set

# Remove some features
tmp_cnt_train <- colSums(x_train)

# Subset features that are not completely zero (tmp_cnt_train>0)
x <- x[,tmp_cnt_train>0]
rm(x_train,tmp_cnt_train)

### XGBoost test ####

# Train model
require(xgboost)

# Label names
group_name <- na.omit(unique(y))

# Train and test data
idx_train <- which(!is.na(y))
idx_test <- which(is.na(y))
train_data <- x[idx_train,]
test_data <- x[idx_test,]

# Assing labels (numeric)
train_label <- match(y[idx_train],group_name)-1
test_label <- match(y[idx_test],group_name)-1

# XGBoost style matrices
dtrain <- xgb.DMatrix(train_data,label=train_label,missing=NA)
dtest <- xgb.DMatrix(test_data,label=test_label,missing=NA)

param <- list(booster="gbtree",
              objective="multi:softprob",
              num_class=12,
              eval_metric="mlogloss",
              eta=0.1,
              max_depth = 3,
              subsample = 0.7,
              colsample_bytree = 0.7)

# Train
set.seed(101)
watchlist <- list(train=dtrain)
fit_xgb <- xgb.train(params=param,
                     data=dtrain,
                     nrounds=300,
                     watchlist=watchlist)
# Predict
pred <- predict(fit_xgb,dtest) # Contains prepdictions of 1112071 observations. It is a vector of dim 12*112071
# Has to be rolled back to a matrix (12,112071)  
pred_detail <- t(matrix(pred,nrow=length(group_name)))

res_submit <- cbind(id=id[idx_test],as.data.frame(pred_detail))
colnames(res_submit) <- c("device_id",group_name)
write.csv(res_submit,file="submit_xgb_test2.csv",row.names=F,quote=F)
