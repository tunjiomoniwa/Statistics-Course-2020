# Part A  USArrests
library(purrr)
library(ggplot2) #ggplots
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

df <- USArrests  #US data
df
df <- na.omit(df) # To remove missing value contained in the data

df <- scale(df) # To apply the scaling function
head(df)

distance <- get_dist(df, method = "euclidean", stand = FALSE)
fviz_dist(distance, gradient = list(low = "blue", mid = "white", high = "red"))
#states with large/high dissimilarities (red), white is mid, blue is low)


km2 <- pam(df, 4)#, metric = "euclidean", stand = FALSE)
km2$clusinfo
#sum(km2$clusinfo[,1]*km2$clusinfo[,2])

#To visualize our mediod-based clusters
fviz_cluster(km2, data = df)

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