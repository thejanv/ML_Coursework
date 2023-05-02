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

clean_df <- apply(df, 2, func_outliers)

# remove outliers
clean_df <- as.data.frame(na.omit(clean_df))

boxplot(clean_df)
title("Box Plot after outlier remove")
# Min max normalization
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

normalized_df <- as.data.frame(apply(clean_df,2, normalize))
boxplot(normalized_df)



clean_df <- apply(normalized_df, 2, func_outliers)
clean_df <- as.data.frame(na.omit(clean_df))
normalized_df <- as.data.frame(apply(clean_df,2, normalize))
boxplot(normalized_df)
title("Box Plot after Normalization")

library(NbClust)
set.seed(123)
# using NbClust determine the optimal number of clusters
clusterNo = NbClust(normalized_df, distance="euclidean", min.nc=2, max.nc = 10, method="kmeans")

table(clusterNo$Best.n[1,])
barplot(table(clusterNo$Best.n[1,]), xlab="Numer of Clusters",ylab="Number of Criteria",
        main="Number of Clusters 
Chosen by 30 Criteria") # NBClust suggest 3
library("factoextra")
sum(is.na(normalized_df))
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

plot(normalized_df, col=kmeans_fit$cluster)
plot(normalized_df[c("Comp", "Rad.Ra")], main="K-means Clustering Plot", col=kmeans_fit $cluster)
points(kmeans_fit $centers[,c("Comp", "Rad.Ra")], col=1:3, pch=23, cex=3)

sil <- silhouette(kmeans_fit$cluster, dist(normalized_df))
fviz_silhouette(sil)
summary(sil)


# pca ----------------------------------------------------------------
library(dplyr)
pca_result <- prcomp(normalized_df, scale = FALSE)
pca_result
names(pca_result)
pca_result$center
pca_result$scale
pca_result$rotation
pca_result$rotation <- -pca_result$rotation
pca_result$rotation
pca_result$x <- - pca_result$x
head(pca_result$x)
pca_result$sdev
(VE <- pca_result$sdev^2)
PVE <- VE / sum(VE)
round(PVE, 3)
varPercent <- PVE*100
barplot(varPercent, xlab='PC', ylab='Percent Variance', names.arg=1:length(varPercent), las=1, ylim=c(0, max(varPercent)), col='gray')
abline(h=1/ncol(USArrests)*100, col='red') 
pca_wines = as.data.frame(pca_result$x[,1:6])
pca_wines
library(factoextra)
set.seed(12345)
# using NbClust determine the optimal number of clusters
clusterNo = NbClust(pca_wines, distance="euclidean", min.nc=2, max.nc = 10, method="kmeans")

table(clusterNo$Best.n[1,])
barplot(table(clusterNo$Best.n[1,]), xlab="Numer of Clusters",ylab="Number of Criteria",
        main="Number of Clusters 
Chosen by 30 Criteria") # NBClust suggest 3


fviz_nbclust(pca_wines, kmeans, method = 'wss')
fviz_nbclust(pca_wines, kmeans, method = 'silhouette')
fviz_nbclust(pca_wines, kmeans, method = 'gap_stat')
k = 2
kmeans_wines = kmeans(pca_wines, centers = k, nstart = 15)
fviz_cluster(kmeans_wines, data = pca_wines)
fit.km <- kmeans(pca_wines, 3)
fit.km
wss = fit.km$tot.withinss
bss = fit.km$betweenss
wss
bss
sil <- silhouette(fit.km$cluster, dist(pca_wines))
fviz_silhouette(sil)
summary(sil)

