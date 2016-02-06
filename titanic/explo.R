library(dplyr); library(ggplot2)
setwd("~/Coursera/projects/titanic")
train <- read.csv("./data/train.csv")

# Rename some factor variables
train <- mutate(train, Pclass = as.factor(Pclass)) %>% mutate(Sex = as.factor(Sex)) 
train <- train %>% mutate(Survived = as.factor(Survived))
train <- mutate(train, fam.size = Parch + SibSp)
levels(train$Survived) <- c("Died", "Survived")

# Create a plot for survival by Pclass and gender (jitter plot) 
g <- ggplot(train, aes(x=Age, y=Pclass)) #+ aes(shape=factor(Survived))
g <- g + facet_grid(.~Sex) + geom_jitter(aes(color=Survived),size=2.5,height = 0.3)
#g
ggsave(filename="plot1.png", plot=g)

# Survival by Pclass, Age and Sex
surv.pas <- select(train, c(2,3,5,6)) %>% group_by(Pclass, Sex) 

g <- ggplot(surv.pas, aes(x=Age, y=Survived))
g <- g + geom_jitter(aes(color=Sex),size=2.5,height = 0.3) + facet_grid(.~Pclass)
#g
ggsave(filename="plot2.png", plot=g)

# Survival by Age and Sex
#surv.sa <- filter(train, Survived == 1) %>% group_by(Sex, Age) 
#surv.sa <- surv.sa %>% summarise(n.surv = n())

# Mosaic Plots
mosaicplot(train$Pclass ~ train$Survived, main="Passenger Survival by Class",
           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)

mosaicplot(train$Sex ~ train$Survived, main="Passenger Survival by Gender",
           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)

png(file="plot3.png",width = 480, height = 480)
mosaicplot(filter(train, Sex=="female")$Pclass ~ filter(train,Sex=="female")$Survived, 
           main="Female Passenger Survival by Pclass",
           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)
dev.off()

mosaicplot(filter(train, Sex=="male")$Pclass ~ filter(train,Sex=="male")$Survived, 
           main="Male Passenger Survival by Pclass",
           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)

## Some further work ##
# Effect of family.szie:
# Pclass, fam.size 
g <- ggplot(train, aes(x=fam.size, y=Pclass)) #+ aes(shape=factor(Survived))
g <- g + facet_grid(.~Sex) + geom_point(aes(color=Survived),size=2.5)
g
# Seems like a complex relationship