# Part A  USArrests
library(purrr)
library(ggplot2) #ggplots
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

df <- USArrests  #US arrests data
df
df <- na.omit(df) # To remove missing value contained in the data

df <- scale(df) # To apply the scaling function
head(df)
df
distance <- get_dist(df, method = "euclidean", stand = FALSE)
fviz_dist(distance, gradient = list(low = "blue", mid = "white", high = "red"))
#states with large/high dissimilarities (red), white is mid, blue is low)


km2 <- pam(df, 4)#, metric = "euclidean", stand = FALSE)
km2$clusinfo
#sum(km2$clusinfo[,1]*km2$clusinfo[,2])

#To visualize our mediod-based clusters
fviz_cluster(km2, data = df, ellipse = TRUE, ellipse.type = "convex", main = "Cluster plot")

############ The elbow method for kmediods##################

set.seed(123)

# function to compute total within-cluster sum of square 
wd <- function(k) {
  sum(pam(df, k)$clusinfo[,1]*pam(df, k)$clusinfo[,2])
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
dist_values <- map_dbl(k.values, wd)

plot(k.values, dist_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total distance")


######## For the average silhouette method
totds <- function(k) {
  pam(df, k)$silinfo$avg.width * length(nd$widths[,1]) 
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
totds_values <- map_dbl(k.values, totds)

plot(k.values, totds_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Total width Silhouettes")


# Part A  Iris to test

library(purrr)
library(ggplot2) #ggplots
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization


df <- iris  #US data
df <- subset(df, select = -c(Species)) # removing the species column
df <- na.omit(df) # To remove missing value contained in the data

df <- scale(df) # To apply the scaling function
head(df)

gow <- daisy(df, metric="gower") #gower distance
fviz_dist(gow, gradient = list(low = "blue", mid = "white", high = "red"))
#states with large/high dissimilarities (red), white is mid, blue is low)


km2 <- pam(gow, 4)#, metric = "euclidean", stand = FALSE)
km2$clusinfo
#sum(km2$clusinfo[,1]*km2$clusinfo[,2])

#To visualize our mediod-based clusters
fviz_cluster(km2, data = gow)

############ The elbow method for kmediods##################

set.seed(123)

# function to compute total within-cluster sum of square 
wd <- function(k) {
  sum(pam(gow, k)$clusinfo[,1]*pam(gow, k)$clusinfo[,2])
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
dist_values <- map_dbl(k.values, wd)

plot(k.values, dist_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total distance")


######## For the average silhouette method
totds <- function(k) {
  pam(gow, k)$silinfo$avg.width * length(nd$widths[,1]) 
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
totds_values <- map_dbl(k.values, totds)

plot(k.values, totds_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Total width Silhouettes")


#Part 2

#Include as a comment in your R-script an explanation of why
#the code given in the online tutorial to calculate the Gap 
#statistic cannot be modified to account for Gower distance 
#(hint: look at the help file for clusGap().

#?clusGap
#clusGap(x, FUNcluster, K.max, B = 100, d.power = 1,
#spaceH0 = c("scaledPCA", "original"),
#verbose = interactive(), ...)

#Answer
# Unlike the Elbow and Silhouette methods, the function for determining
# the number of clusters does not contain the argument for Gower distance.
# The function uses Euclidean distances as default with d.power = 1
# or d.power= 2 for the proposal by Tibshirani et al.

