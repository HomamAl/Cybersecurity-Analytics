########################################################################
#
# KDD Cup 1999 Dataset: http://kdd.ics.uci.edu/databases/kddcup99/kddcup99.html
#
########################################################################

## Activity 2 - Decision Tree Classification

########################################################################

# Part 1 - Load and Pre-process Data

# load required libraries
library(C50)

# import training and testing data from csv
kdddata.train<- read.csv(file = "cslab-kddcupdata-corrected-train.csv", sep = ',', header = TRUE)
kdddata.test <- read.csv(file = "cslab-kddcupdata-corrected-test.csv", sep = ',', header = TRUE)

########################################################################

# Part 2 - Building a Decision Tree

# create a c5.0 decision tree model based on training data
model <- C50::C5.0(kdddata.train[,2:42], as.factor(kdddata.train[,1]) )  # use attack as class
# view model
summary(model)

########################################################################

# Part 3 - Testing the Model

# test model with the training set
model.predictions <- predict(model, kdddata.test[,2:42])

# display prediction results against truth
predictions <- table(model.predictions, kdddata.test$attack)
predictions <- as.data.frame(predictions)
colnames(predictions) <- c('Predicted Value', 'Actual Value', 'Number of Cases')
predictions

