########################################################################
#
# KDD Cup 1999 Dataset: http://kdd.ics.uci.edu/databases/kddcup99/kddcup99.html
#
########################################################################

## Activity 1 - K-Means Clustering

########################################################################

# Part 1 - Load and Pre-process Data

# load required libraries
library(cluster)
library(ggplot2)

# import training data from csv
kdddata.train<- read.csv(file = "cslab-kddcupdata-corrected-train.csv", sep = ',', header = TRUE)

# Pre-process data
data <- kdddata.train[,6:40]  # select only numerical features
data <- na.omit(data) # delete rows with missing values
subdata <- scale(data) #scale data
# subdata <- data

########################################################################

# Part 2 - Identifying K

# Determine number of clusters k by within groups sum of squares (mean distance between data points and cluster centroid)
wss <- (nrow(subdata)-1)*sum(apply(subdata,2,var))
for (i in 2:7) wss[i] <- sum(kmeans(subdata, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Determining K - WSS Elbow")

########################################################################

# Part 3 - Performing K-Means Clustering

# Perform K-Means Clustering
fit <- kmeans(subdata, 5) # *** Change integer to reflect your k value ***
# get cluster means
results <- aggregate(subdata,by=list(fit$cluster),FUN=mean)
results

# append cluster assignment
data <- data.frame(data, fit$cluster)
# append attack
data <- data.frame(data, kdddata.train$attack)

