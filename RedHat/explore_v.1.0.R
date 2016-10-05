# Burak Himmetoglu
# begin: 08-04-2016
#
# RedHat: Simple exploration
# Inspired from an exploratory analysis from Kaggle
#
# Libraries
options(stringsAsFactors = FALSE)
require(dplyr)
require(tidyr)
require(ggplot2)

# Read train/test data for actvities
setwd("~/Works/Rworkspace/RedHat")
#
train <- read.csv("./data/act_train.csv")
test <- read.csv("./data/act_test.csv")

# People data
people <- read.csv("./data/people.csv")

# Total number of outcomes
train %>% group_by(outcome) %>% summarize(number = n())

# Total number of outcomes per activity category
train %>% group_by(activity_category,outcome) %>% summarise(number = n())
# Easier to see on plot
gg0 <- ggplot(train,aes(x = outcome, fill = activity_category)) + geom_bar(width = 0.6, position = "fill")
gg0

# Let us look at char_5
cat("Unique entries in char_5:", unique(train$char_5), "\n")
gg1 <- ggplot(train, aes(x = outcome, fill = char_5)) + geom_bar(width = 0.6,position = "fill")
gg1 # Majority is empty

# Another way
gg2 <- train %>% count(char_5) %>% ggplot(aes (x = reorder(char_5,n), y = n)) + geom_bar(stat = "identity", fill = "light blue") + 
  coord_flip() + ggtitle("Count of char_5")
gg2

# Char_10 seem to have many factor variables
gg3 <- train %>% count(char_10, sort = TRUE) %>% filter(n > 8000) %>%
  ggplot(aes (x = reorder(char_10,n), y = n)) + geom_bar(stat = "identity") + 
  coord_flip() + ggtitle("Count of char_10")
gg3

# Cumulative sum
count_char_10 <- train %>% count(char_10, sort = TRUE)
plot(cumsum(count_char_10$n[1:1000])/sum(count_char_10$n),
     type = "b", pch = ".", main = "Cumulative Percent by types of char_10", ylab = "cumulative percent")

# Popular types in char_10 (first 15 in count_char_10)
popular <- count_char_10$char_10[1:15]
gg4 <- train %>% filter(char_10 %in% popular) %>% ggplot(aes(x = outcome, fill = char_10)) + 
  geom_bar(width = 0.6, position = "fill") + ggtitle("Outcome by char_10")
gg4
# Another way
gg5 <- train %>% filter(char_10 %in% popular) %>% ggplot(aes(x = char_10, fill = char_10)) + geom_bar() + coord_flip() +
  facet_grid(~ outcome) + ggtitle("Outcome by char_10")
gg5

# People
gg6 <- train %>% count(people_id, sort = TRUE) %>% ggplot(aes(x = n)) + geom_histogram()
gg6 # A small number of people with a large number of activities 

people_count <- train %>% count(people_id, sort = TRUE)
c(median(people_count$n), mean(people_count$n)) # Very different mean and median also reflects the above fact

# Let is look at first 10 people with most activities
people_count_10 <- people_count$people_id[1:10]
gg7 <- train %>% filter(people_id %in% people_count_10) %>% ggplot(aes(x = outcome, fill = people_id)) + 
  geom_bar(width = 0.6, position = "fill") + ggtitle("Outcome by top10 people")
gg7
# A different way to look
gg8 <- train %>% filter(people_id %in% people_count_10) %>% ggplot(aes(x = people_id)) +
  geom_bar() + facet_grid(~ outcome) + coord_flip() + ggtitle("Outcome by top10 people")
gg8

# Proportion of positive outcomes by people_id
gg9 <- train %>% filter(people_id %in% people_count$people_id[1:10000]) %>% group_by(people_id) %>%
  summarize(count = n(), outcome_pos = sum(outcome)) %>% mutate(frac_pos = outcome_pos/count) %>% 
  ggplot(aes(x = frac_pos)) + geom_histogram() +
  ggtitle("Fraction of Positive Outcomes by person")
gg9 # Most people have entirely positive or negative outcomes regardless of their actions.

# No intersection between people_id's in train and test!!!
intersect(test$people_id, train$people_id)

##### Merging with people #####
train <- merge(train, people, by = "people_id", all.x = TRUE)
# Notice that char_1 -- char_10 of activities are different from char_1 -- char_38 of people 

# Group count
train %>% count(group_1, sort = TRUE) -> group_count

# Char_38 (Only numeric value)
gg10 <- train %>% ggplot(aes(x = char_38)) + geom_histogram() + ggtitle("Distribution of char_38")
gg10

# By outcome
gg11 <- train %>% ggplot(aes(x = char_38, fill = as.factor(outcome))) + geom_histogram() + facet_wrap(~ outcome)
gg11 # Small values of char_38 is related to negative outcomes

# Another view
gg12 <- train %>% 
  ggplot(aes(x = char_38, fill = as.factor(outcome))) +
  geom_histogram(position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Proportion of outcomes by values of char_38")
gg12
