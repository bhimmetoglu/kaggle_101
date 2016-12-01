# Burak Himmetoglu
# Date: 11-18-2016
#
# Predicting Housing Prices
# Plots
library(ggplot2)
library(tidyr)
library(dplyr)

## y_actual vs y_pred for each model and each repetition

#### trainHOList ####
plotHO <- function(dataHO, irep){
  # Turn into a tidy data
  dat <- gather(dataHO[[irep]], key = model, value = y_pred, -c(Id, y)) %>% mutate(model = as.factor(model)) %>%
    mutate(error = abs(y - y_pred))
  # Bad predictions
  bad.pred <- dat %>% filter(error > 0.3) %>% arrange(desc(error))
  # Plot
  title = paste0("Repetition",as.character(irep),": Prediction of HoldOut Data")
  gg <- ggplot(dat, aes(y, y_pred)) + geom_point(aes(x = y, y = y_pred, shape = model, color = error), size = 2) +
    scale_shape(solid = TRUE) + geom_abline(slope = 1, intercept = 0) + geom_point(data=bad.pred, colour="red") +
    scale_colour_gradient(limits=c(0, 0.35)) + facet_wrap(~model) + 
    ggtitle(title)
  gg
}
# Some plots
gg1 <- plotHO(trainHOList,1); gg1
gg2 <- plotHO(trainHOList,2); gg2
gg3 <- plotHO(trainHOList,3); gg3
gg4 <- plotHO(trainHOList,4); gg4
gg5 <- plotHO(trainHOList,5); gg5
gg6 <- plotHO(trainHOList,6); gg6
gg7 <- plotHO(trainHOList,7); gg7
gg8 <- plotHO(trainHOList,7); gg8
gg9 <- plotHO(trainHOList,7); gg9
gg10 <- plotHO(trainHOList,10); gg10

#### trainOOSList ####
# Gather all the model predictions in one columns
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

## Stacked predictions
# predStack <- as.data.frame(predStack)
# colnames(predStack) <- c("Rep1", "Rep2", "Rep3")
# predStack <- predStack %>% mutate(avg = (Rep1+Rep2+Rep3)/3)
# datStack <- gather(predStack, key = Repetition, value = y_pred)
# ggStack <- ggplot(datStack, aes(y_pred))
