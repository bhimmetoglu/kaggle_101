# Santander data clean/process
# Clean and Process the data
# Author: Burak H
#
require(dplyr); require(e1071)

clean_process <- function(train0,test0){
  # Save TARGET values and remove
  y <- train0$TARGET; train0$TARGET <- NULL
  
  # Find constant features and remove them from analysis
  for (i in names(train0)){
    if (length(unique(train0[[i]])) == 1 | length(unique(test0[[i]])) == 1){
      train0[[i]] <- NULL
      test0[[i]] <- NULL
    }
  }
  
  # Find duplicate columns and remove them
  dpl.cl <- data.frame(i = integer(), j = integer())

  for ( i in 2:(ncol(train0)-1) ){
    for ( j in (i+1):ncol(train0) ){
      if ( identical(train0[,i], train0[,j]) == TRUE  ){
        dpl.cl <- rbind(dpl.cl, data.frame(i=i,j=j))
      }
    }
  }
  train0 <- train0[,-dpl.cl$j]
  test0 <- test0[, -dpl.cl$j]
  
  # Save IDs and Remove them
  train.id <- train0$ID; test.id <- test0$ID
  train0$ID <- NULL; test0$ID <- NULL
  
  # Deskewing function
  dsk <- function(df.tr, df.ts, threshold = 1.00){
    #
    df <- rbind(df.tr, df.ts) # Bind the two frames 
    #
    for (j in 1:ncol(df)){
      c.min <- min(df[[j]]); #c.max <- max(df[[j]])
      t <- log(1 - c.min + df[[j]])
      if ( abs(skewness(df[[j]])) > threshold * abs(skewness(t))){
        df[[j]] <- t
      }
    }
    # Separate the data frames back into original
    isep = c(rep(1, nrow(df.tr)),rep(-1, nrow(df.ts)))
    df <- mutate(df, isep = factor(isep))
    list(filter(df, isep == "1"), filter(df, isep == "-1")) # Return train and test separately
  }
  
  retfn <- dsk(train0,test0,threshold = 0.5)
  train0 <- retfn[[1]]; test0 <- retfn[[2]]
  train0$isep <- NULL; test0$isep <- NULL
  
  # Return the processed data and the TARGET values
  list(y, train0, test0, test.id)
}

