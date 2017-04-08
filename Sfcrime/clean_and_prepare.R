# SF Crime data: Clean and prepare (round 1)

# Load libraries
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(lubridate)
library(Matrix)
library(MatrixModels)
library(caret)

# Load data
cat("Reading traing and test...\n")
train <- read_csv("./data/train.csv", col_types = "Tccccccnn")
test <- read_csv("./data/test.csv", col_types = "iTcccnn")

cat("Feature extraction ...\n")
# Remove Descript from train
train$Descript <- NULL
train$Resolution <- NULL

# # Resolution
# # Separate between None & Resolved
# train <- train %>% mutate(Resolution = as.factor(Resolution))
# new_resolutions <- rep("RESOLVED", 17)
# new_resolutions[12] <- "NONE"
# levels(train$Resolution) <- new_resolutions
# test$Resolution <- NA

### Create log ratio columns from address (here, divide train into two...)
cat("Creating log ratio features from Address...\n")

fill_na <- function(x, default_ratio){
  x[is.na(x)] <- default_ratio
  x
}

create_ratios_folds <- function(train, nfolds){
  # Create folds
  folds <- createFolds(train$Category, k = nfolds)
  list_out <- list()
  
  for (ifold in 1:length(folds)){
    train_in <- train[-folds[[ifold]], ]
    train_ho <- train[folds[[ifold]], ]
    
    # Group by Address & Category
    df <- train_in %>%
      mutate(Address = as.factor(Address)) %>%
      mutate(Category = as.factor(Category)) %>%
      group_by(Address, Category) %>% count() %>%
      rename(tot_address_category = n)
    
    # Group by Address
    df2 <- train_in %>%
      mutate(Address = as.factor(Address)) %>%
      mutate(Category = as.factor(Category)) %>%
      group_by(Address) %>% count() %>% rename(tot_address = n)
    
    # Join and determine the ratio of each Category per Address
    df3 <- left_join(df, df2, by = "Address")
    df3 <- df3 %>%
      mutate(freq = ifelse(tot_address_category >= 5, 1.0 + tot_address_category / tot_address, 1.0))
    
    # Now spread the log frequencies to columns
    df3$tot_address_category <- NULL
    df3$tot_address <- NULL
    df4 <- spread(df3, key = Category, value = freq, fill = 1.0)
    
    # Collapse the feature names
    names(df4) <- str_replace_all(names(df4), "( )", "")
    names(df4) <- str_replace(names(df4), "/", "_")
    names(df4) <- str_replace(names(df4), "-", "_")
    
    # Join df4 with train_out
    train_ho <- left_join(train_ho,
                           df4 %>% ungroup(Address) %>% mutate(Address = as.character(Address)),
                           by = "Address")
    
    # Impute NAs with 1.0
    train_ho <- train_ho %>% mutate_at(names(df4)[-1], fill_na, default_ratio=1.0)
  
    # Add to list
    list_out[[ifold]] <- train_ho
  }
  
  # Return train_out
  bind_rows(list_out)
}

create_ratios_test <- function(train, test){
  # Features to be created
  log_feats <- names(train)[8:46]
  
  # At each address, the median of the determined Address features
  df <- train %>% select_(.dots = c(log_feats, "Address")) %>% distinct() %>%
    group_by(Address) %>% summarise_at(log_feats, median)
  
  # Join df4 with train_out
  test_out <- left_join(test,
                        df %>% ungroup(Address),
                        by = "Address")
  
  # Impute NAs with 1.0
  test_out <- test_out %>% mutate_at(log_feats, fill_na, default_ratio=1.0)
  
  # Return test_out
  test_out
}

# Create new fetures 5 folds
train_new <- create_ratios_folds(train, 5)

# Cerate new features in test
test_new <- create_ratios_test(train_new, test)
rm(train,test); gc()

# Compute log
log_feats <- names(train_new)[8:46]
train_new <- train_new %>% mutate_at(log_feats, log)
test_new <- test_new %>% mutate_at(log_feats, log)

# Combine test and train
test_ids <- test_new$Id
test_new$Id <- NULL
test_new$Category <- NA # Fill test Category with NAs
full_data <- bind_rows(train_new, test_new) # Filter test by is.na(Category) == TRUE

# Date and time
full_data <- full_data %>%
  mutate(year = year(ymd_hms(Dates))) %>%
  mutate(month = month(ymd_hms(Dates))) %>%
  mutate(hour = hour(ymd_hms(Dates)))
full_data$Dates <- NULL

# Address feature: is it an intersection?
find_intersection <- function(x){ as.integer(str_detect(x, "/")) }
full_data <- full_data %>%
  mutate(is.intersection = find_intersection(Address))

# Address feature: is it block?
find_block <- function(x) { as.integer(str_detect(x, "Block")) }
full_data <- full_data %>% 
  mutate(is.block = find_block(Address))

# Address features: is it ST, AV, DR, WY, PZ, LN, BL, RD, zeroBlock, numberedST
find_type <- function(x, t){
  as.integer(str_detect(x, t))
}

full_data <- full_data %>% 
  mutate(is.AV = find_type(Address, "( AV)$|( AVE)$|( AV )| ( AVE )")) %>%
  mutate(is.ST = find_type(Address, "( ST)$|( ST )")) %>%
  mutate(is.DR = find_type(Address, "( DR)$| ( DR )")) %>%
  mutate(is.WY = find_type(Address, "( WY)$|( WAY)$|( WY )|( WAY )")) %>%
  mutate(is.PZ = find_type(Address, "( PZ)$|( PZ )")) %>%
  mutate(is.PL = find_type(Address, "( PL)$|( PL )")) %>%
  mutate(is.LN = find_type(Address, "( LN)$|( LN )")) %>%
  mutate(is.BL = find_type(Address, "( BL)$|( BLVD)$|( BL )|( BLVD )")) %>%
  mutate(is.RD = find_type(Address, "( RD)$|( RD )")) %>%
  mutate(is.zeroBlock = find_type(Address, "^(0 Block)")) %>%
  mutate(is.numberedST = find_type(Address, "[0-9](RD)|[0-9](TH)"))

# Remove Adress feature
full_data$Address <- NULL

# Convert Date features, PdDistrict into factor
full_data <- full_data %>% 
  mutate(Category = as.factor(Category)) %>%
  mutate(PdDistrict = as.factor(PdDistrict)) %>%
  mutate(year = as.factor(year)) %>%
  mutate(month = as.factor(month)) %>%
  mutate(DayOfWeek = as.factor(DayOfWeek))

# Medians by PdDistrict
med_table <- full_data %>% group_by(PdDistrict) %>% summarise(med_X = median(X), med_Y = median(Y))
full_data <- full_data %>%
  left_join(med_table, by = "PdDistrict")

# Replace outliers
outs <- full_data$Y > 40 # Outliers
full_data[outs, ]$X <- full_data[outs, ]$med_X
full_data[outs, ]$Y <- full_data[outs, ]$med_Y
full_data$med_X <- NULL
full_data$med_Y <- NULL

# Normalize X,Y,hour
normalize_feat <- function(x){
  (x - mean(x))/sd(x)
}

full_data <- full_data %>%
  mutate(X = normalize_feat(X)) %>%
  mutate(Y = normalize_feat(Y)) %>%
  mutate(hour = normalize_feat(hour))

# Clusters
if (n_clusters > 0){
  cat("Calculating clusters...\n")
  
  # K-means clustering to create new features
  df_K <- full_data %>% select(-c(Category))
  mat_K <- model.Matrix(~., as.data.frame(df_K))[,-1]
  
  # Normalize
  mat_K <- apply(mat_K, 2, function(x) { (x - median(x)) / max(x) })
  #mat_K <- apply(mat_K, 2, normalize_feat)
  
  # Compute clusters
  clust_n <- kmeans(mat_K, n_clusters)
  
  # Add cluster centers to data
  full_data <- full_data %>% mutate(cluster = clust_n$cluster) %>% mutate(cluster = as.factor(cluster))
  
  if (euclidean == TRUE){
    
    # Function to calculate distance to cluster centers
    dist_center <- function(row, centers){
      n_centers = dim(centers)[1]
      dc <- c()
      for (ii in 1:n_centers){
        tmp <- sqrt(sum(row - centers[ii, ])^2)
        dc <- c(dc, tmp)
      }
      dc
    }
    
    # Euclidean distance to the cluster centers
    distances <- apply(mat_K, 1, dist_center, centers = clust_n$centers)
    df.distances <- as.data.frame(t(distances))
    colnames(df.distances) <- paste0("cl", 1:n_clusters)
    df.distances <- df.distances %>% mutate_all(funs(normalize_feat))
    
    # Bind with the data
    full_data <- cbind(full_data, df.distances) 
    rm(distances)
  }
  
  # Remove unnecessary data
  rm(df_K, mat_K, clust_n); gc()
}

# Remove unused
rm(med_table, outs, train_new, test_new); gc()
cat("Done...\n")