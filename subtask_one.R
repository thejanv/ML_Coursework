#Removed all the existing objects
rm(list = ls())

# load package
library(readxl)
library(factoextra)
library(NbClust)
library(cluster)

#Load the dataset
df <- read_excel("Data/vehicles.xlsx")
df.x <- df[,-20]
df.x <- df.x[,-1]

boxplot(df.x)

# Missing Value Analysis
sum(is.na(df.x))
summary(is.na(df.x))

# Tukey Fence
threshold <- 1.5
q1 <- apply(df.x, 2, quantile, 0.25)
q3 <- apply(df.x, 2, quantile, 0.75)
iqr <- q3 - q1
lower_fence <- q1 - threshold * iqr
upper_fence <- q3 + threshold * iqr
outliers <- apply(df.x, 2, function(x) x[x < lower_fence | x > upper_fence])

# remove outliers
clean_df <- df
for (col in names(df)) {
  clean_df[,col][df[,col] < lower_fence[col] | df[,col] > upper_fence[col]] <- NA
}
clean_df <- na.omit(clean_df)
lable <- clean_df$Class
clean_df <- clean_df[,-20]
boxplot(clean_df)

# Min max normalization
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

normalized_df <- as.data.frame(lapply(clean_df, min_max_norm))
summary(normalized_df)


# determine the number of cluster
# install.packages("NbClust")

set.seed(10)

# using NbClust determine the optimal number of clusters
clusterNo = NbClust(normalized_df, distance="euclidean", min.nc=2,
                              max.nc=10, method="kmeans", index="all")

table(clusterNo$Best.n[1,])
barplot(table(clusterNo$Best.n[1,]), xlab="Numer of Clusters",ylab="Number of Criteria",
        main="Number of Clusters Chosen by 30 Criteria") # NBClust suggest 2



# silhouette
fviz_nbclust(normalized_df, kmeans, method = 'silhouette') # silhouette suggest 2

# Elbow Method
fviz_nbclust(normalized_df, kmeans, method = 'wss') # Elbow suggest 

# gap
fviz_nbclust(normalized_df, kmeans, method = 'gap_stat') # gap Suggest 


# use k as a 2
k <- 2
kc <- kmeans(normalized_df,k)
kc

#The between-cluster sum of squares
bss <- kc$betweenss
bss

# The total sum of squares.
tss <- kc$totss
tss

# Total within-cluster sum of squares
wss <- kc$tot.withinss

table(lable,kc$cluster)


plot(normalized_df[c("Circ", "D.Circ")], col=kc$cluster)
points(kc$centers[,c("Circ", "D.Circ")], col=1:3, pch=23, cex=3)


sil <- silhouette(kc$cluster, dist(normalized_df))
fviz_silhouette(sil)
summary(sil)
