library(apcluster)
library(purrr)
library(ggplot2) #ggplots
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra)

df1 <- read.table(file="mote_locs.txt", header=FALSE)
p1 <-qplot(df1$V2, df1$V3, xlab = "x - position", ylab = "y-position")

df <- subset(df1, select = -c(V1))
df
df <- scale(df) # To apply the scaling function

############### Affinity propagation
apres1a <- apcluster(negDistMat(r=2), df)
apres1a
plot(apres1a, df, main = "Similarity matrix with 6 clusters")

heatmap(apres1a)#, main = "Heat map showing how samples are grouped according to clusters")
s1 <- negDistMat(df, r=2)
apres1b <- apcluster(s1)
p3 <- heatmap(apres1b, s1)
apres1c <- apcluster(s1, details = TRUE)
plot(apres1c)

dgram <- aggExCluster(s1, apres1b)
dgram
plot(dgram)


