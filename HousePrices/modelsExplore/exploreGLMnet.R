# Burak Himmetoglu
# begin: 10-06-2016
#
# Housing Prices: Simple exploration with glmnet
#
# Libraries
require(glmnet)

# Clean and prepare data
#source("stacking/cleanData.R")
#select <- dplyr::select
source("stacking/cleanDataDetailed.R")

# Train glmnet for a range of values of alpha
alpha_values <- c(0, 1e-6, 1e-5, 1e-4, 1e-3, 0.01, 0.1, 1)
cv.results <- data.frame(alpha = alpha_values, lambda = 0, rmse = 0)
for (ial in 1:length(alpha_values)){
  mod.cv <- cv.glmnet(x = as.matrix(trainSparse), y = outcomes, nfolds = 5, type.measure = "mse",
                      alpha = alpha_values[ial])
  # Determine lambda
  lam <- mod.cv$lambda.min; ind.lam <- which(mod.cv$lambda == lam)
  
  # Store CV results
  cv.results[ial, ]$lambda <- mod.cv$lambda.min; cv.results[ial, ]$rmse <- sqrt( mod.cv$cvm[ind.lam] )
}

# Best model
ind.best <- which.min(cv.results$rmse)
alBest <- cv.results[ind.best, 1]
lamBest <- cv.results[ind.best, 2]

# Final fit
mod.glmnet <- glmnet(x = as.matrix(trainSparse), y = outcomes, alpha = alBest, lambda = lamBest)
predTest <- predict(mod.glmnet, newx = as.matrix(testSparse))

# Data for submission
submission <- data.frame(Id = Id.test, SalePrice = exp(predTest))
colnames(submission) <- c("Id","SalePrice")
write.csv(submission,"submission-glmnet.csv",row.names = FALSE)
