#Removed all the existing objects
rm(list = ls())

# load package
library(readxl)
library(ggplot2)
library(factoextra)
library(NbClust)
library(cluster)

#Load the dataset
df <- read_excel("Data/vehicles.xlsx")
head(df)
df.lable <- df$Class
df <- df[,2:19]

plot.new()
par(mfrow = c(1, 2))
boxplot(df)
title("Box Plot before outlier remove")

func_outliers <- function(x){
  outlier_values <- boxplot(x, plot=FALSE)$out
  data_clean <- replace(x, x %in% outlier_values, NA)
  data_clean
}

# remove outliers
clean_df <- apply(df, 2, func_outliers)
clean_df <- as.data.frame(na.omit(clean_df))

boxplot(clean_df)
title("Box Plot after outlier remove")

# Min max normalization
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# normalize data
normalized_df <- as.data.frame(apply(clean_df,2, normalize))
boxplot(normalized_df)
clean_df <- apply(normalized_df, 2, func_outliers)
clean_df <- as.data.frame(na.omit(clean_df))
normalized_df <- as.data.frame(apply(clean_df,2, normalize))
boxplot(normalized_df)
title("Box Plot after Normalization")

library(NbClust)

# using NbClust determine the optimal number of clusters
set.seed(123)
clusterNo = NbClust(normalized_df, distance="euclidean", min.nc=2, max.nc = 10, method="kmeans")
table(clusterNo$Best.n[1,])
barplot(table(clusterNo$Best.n[1,]), xlab="Numer of Clusters",ylab="Number of Criteria",
        main="Number of Clusters 
Chosen by 30 Criteria") # NBClust suggest 3

library(factoextra)
library(ggplot2)
library(factoextra)

# silhouette
fviz_nbclust(normalized_df, kmeans, method = 'silhouette')+
  labs(subtitle = "Silhouette method") # silhouette suggest 2 

# Elbow Method
fviz_nbclust(normalized_df, kmeans, method = 'wss') # Elbow suggest 3

# gap
fviz_nbclust(normalized_df, kmeans, method = 'gap_stat') # gap Suggest 3

set.seed(1234)
# use k as a 3
k <- 3
kmeans_fit  = kmeans(normalized_df, centers = k, nstart = 2, iter.max = 17)
kmeans_fit 

#The between-cluster sum of squares
bss <- kmeans_fit $betweenss
bss

# The total sum of squares.
tss <- kmeans_fit $totss
tss

# Total within-cluster sum of squares
wss <- kmeans_fit $tot.withinss
wss


plot(normalized_df[c("Comp", "Rad.Ra")], main="K-means Clustering Plot", col=kmeans_fit $cluster)
points(kmeans_fit $centers[,c("Comp", "Rad.Ra")], col=1:3, pch=23, cex=3)

sil <- silhouette(kmeans_fit$cluster, dist(normalized_df))
fviz_silhouette(sil)
summary(sil)

library(tidyverse)   
library(gridExtra)  
library(ggplot2)    
library(rlang)         

pca_result <- prcomp(normalized_df, scale = FALSE)
cumulative_variance <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
cumulative_variance

plot(cumulative_variance, xlab = "Number of PCs", ylab = "Cumulative Variance Explained", 
     type = "b")
abline(h=0.92, col="red")
title("Cumalative value > 92%")

pca_result

# Eigenvector
eigenvector <- pca_result$rotation
eigenvector
pca_result$rotation <- -pca_result$rotation

pca_result$x <- - pca_result$x

eigenvalues <- pca_result$sdev^2
eigenvalues
set.seed(1233)
nb <- NbClust(pca_result$x[,1:6], method="kmeans", distance = "euclidean", 
              min.nc = 2, max.nc = 10, index = "all") # 3
# Elbow Method
fviz_nbclust(pca_result$x[,1:6], kmeans, method = 'wss') # Elbow suggest 3

# gap
fviz_nbclust(pca_result$x[,1:6], kmeans, method = 'gap_stat') # gap Suggest 3
fviz_nbclust(pca_result$x, kmeans, method = 'silhouette')+
  labs(subtitle = "Silhouette method") # silhouette suggest 2 


k <- 3
set.seed(453)
kmeans_fit  = kmeans(pca_result$x[,1:6], centers = k, nstart = 2, iter.max = 17)
kmeans_fit 
kmeans_fit$betweenss
wss <- kmeans_fit $tot.withinss
wss

library(cluster) 

sil <- silhouette(kmeans_fit$cluster, dist(pca_result$x[,1:6]))
fviz_silhouette(sil)
summary(sil)
kmeans_fit <- function(data, k) {
  kmeans(data, centers = k, nstart = 25)
}

# Calinski-Harabasz index
library(fpc)
Calinski <- calinhara(pca_result$x, kmeans_fit$cluster)
Calinski
