rm(list = ls())

setwd("C:/Users/Jie.Hu/Desktop/Segmentation/0304")
set.seed(1337)

library(dplyr)
library(cluster)
library(Rtsne)
library(ggplot2) 


df <- read.csv('seg_0304.csv', header=T, stringsAsFactors=FALSE)
df_seg <- df

# filter
#df_seg <- subset(df, QPASSWORD %in% c(1,2,3,4))
#df_seg <- df_seg[1:14]

# check missing value
sapply(df_seg, function(x) sum(is.na(x)))
#df_seg[is.na(df_seg)] <- 5
#df_seg$QACTUALKINGFAN = df_seg$QACTUALKINGFAN/20


#normalize
#normalize <- function(x) {
#  return ((x - min(x)) / (max(x) - min(x)))
#}
#df_seg[,-1] <- as.data.frame(lapply(df_seg[,-1], normalize))

# k-means==============================================================================
# use elbow method to find the optimal mumber of clusters
wcss<- vector()

for (i in 1:20)
  wcss[i] <- kmeans(df_seg[,-c(1)], i)$tot.withinss/kmeans(df_seg[,-1], i)$totss

plot(1:20, wcss, type='b', 
     main='Clusters of Clients',
     xlab='Number of Clusters',
     ylab='Percentage of Within Cluster Sum of Squares',
     pch=20, cex=2)


# apply to the data
set.seed(1234)
km3<- kmeans(df_seg[,-c(1)], 3, iter = 100, nstart = 10)
df_seg$Cluster3 = km3$cluster

km4 <- kmeans(df_seg[,-c(1)], 4, iter = 100, nstart = 10)
df_seg$Cluster4 = km4$cluster

km5 <- kmeans(df_seg[,-c(1)], 5, iter = 100, nstart = 10)
df_seg$Cluster5 = km5$cluster


table(df_seg$Cluster3)
table(df_seg$Cluster4)
table(df_seg$Cluster5)


# find centroids
#centers <- aggregate(df[, 2:40], list(df$Cluster4), mean)

df_seg$Weight <- 1
sapply(split(df_seg, df_seg$Cluster3), function(x) apply(x[, 2:23], 2, weighted.mean, x$Weight, na.rm =T)) *2200
sapply(split(df_seg, df_seg$Cluster4), function(x) apply(x[, 2:23], 2, weighted.mean, x$Weight, na.rm =T)) *2200
sapply(split(df_seg, df_seg$Cluster5), function(x) apply(x[, 2:23], 2, weighted.mean, x$Weight, na.rm =T)) *2200


write.csv(df_seg, file="seg_output.csv", row.names = FALSE)


# Euclidean distance
eu_dist <- daisy(df_seg[, 2:23],
                 metric = "euclidean")

# t-SNE
set.seed(123)
tsne_obj <- Rtsne(eu_dist, is_distance = TRUE)

# graph
tsne_data <- as.data.frame(tsne_obj$Y)
colnames(tsne_data) <- c("X", "Y")
tsne_data <- cbind(ID = df_seg$Internal.Interview.Numbers, tsne_data)
tsne_data$Cluster <- factor(df_seg$Cluster3)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color=Cluster, shape = Cluster), size = 2) +
  ggtitle("Clustering Graph") + 
  xlab("Dimension_1") + 
  ylab("Dimension_2") +
  theme_bw() 

#write.csv(tsne_data, file="seg_xy_cluster3.csv", row.names = FALSE)