# Burak Himmetoglu
# Date: 11-18-2016
#
# Predicting Housing Prices
# Plots
library(ggplot2)
library(tidyr)

## y_actual vs y_pred
dat1 <- gather(trainOOSList[[1]], key = model, value = y_pred, -c(Id,y)) %>% mutate(model = as.factor(model)) %>%
  mutate(error = abs(y - y_pred))
gg1 <- ggplot(dat1, aes(y, y_pred)) + geom_point(aes(x = y, y = y_pred, shape = model, color = error), size = 2) +
  scale_shape(solid = TRUE) + geom_abline(slope = 1, intercept = 0) + 
  facet_grid(model ~.) + ggtitle("Repetition 1")
gg1

# Rep 2
dat2 <- gather(trainOOSList[[2]], key = model, value = y_pred, -c(Id,y)) %>% mutate(model = as.factor(model)) %>%
  mutate(error = abs(y - y_pred))
gg2 <- ggplot(dat2, aes(y, y_pred)) + geom_point(aes(x = y, y = y_pred, shape = model, color = error), size = 2) +
  scale_shape(solid = TRUE) + geom_abline(slope = 1, intercept = 0) + 
  facet_grid(model ~.) + ggtitle("Repetition 2")
gg2

# Rep 3
dat3 <- gather(trainOOSList[[3]], key = model, value = y_pred, -c(Id,y)) %>% mutate(model = as.factor(model)) %>%
  mutate(error = abs(y - y_pred))
gg3 <- ggplot(dat3, aes(y, y_pred)) + geom_point(aes(x = y, y = y_pred, shape = model, color = error), size = 2) +
  scale_shape(solid = TRUE) + geom_abline(slope = 1, intercept = 0) + 
  facet_grid(model ~.) + ggtitle("Repetition 3")
gg3
