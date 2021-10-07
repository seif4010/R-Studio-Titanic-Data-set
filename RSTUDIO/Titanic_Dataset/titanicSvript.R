# Loading data and cleaning the data part

# Loading the library
library(ggplot2)
library(dplyr)
library(GGally)
library(rpart)
library(rpart.plot)
library(randomForest)

# Loading the data 
test <- read.csv("C:/Users/DELL/Downloads/RSTUDIO/Titanic/titanic_test.csv")
train <- read.csv("C:/Users/DELL/Downloads/RSTUDIO/Titanic/titanic_train.csv")

# creating a new data set with both train and test
full <- bind_rows(test, train)
# shows the total number of values within the new data set full
# LT stands for less than sign
LT = dim(train)[1]

# checking the structure
str(full)

# checking for any missing values within the data set
colSums(is.na(full))
colSums(full == "")

# We have a lot of missing data in the Age feature (263/1309)
# Let's change the empty strings in Embarked to the first choice "C"
full$embarked[full$embarked == ""] = "c"

# check number of features that can be moved to factors
apply(full,2, function (x) length(unique(x)))

# lets pick pclass, sex, survived and embarked and move it into factors
cols <- c("pclass", "sex", "survived", "embarked")
for (i in cols) {
  full[,i] <- as.factor(full[,i])
}

# look at the structure to see the changes made
str(full)

# Analysis part
# Look at the relationship between sex and survival
ggplot(data = full[1:LT, ], aes(x=sex, fill=survived)) + geom_bar()

# survival as a function of embarked 
ggplot(data = full[1:LT, ], aes(x=embarked, fill=survived)) + geom_bar() + ylab("Frequency")

t <- table (full[1:LT, ]$embarked, full[1:LT, ]$survived)
for (i in 1 : dim(t) [1]) {
  t[i, ] <- t [i, ]/sum(t[i, ]) *100
}

t

# Survive as a function in place of pclass
ggplot(data = full[1:LT,], aes(x=pclass, fill=survived)) + geom_bar(position="fill") + ylab("Frequency")






