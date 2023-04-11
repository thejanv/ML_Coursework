#Removed all the existing objects
rm(list = ls())

# install and load readxl package
# install.packages('readxl')
library(readxl)

#Setting the working directory
getwd()

#Load the dataset
df <- read_excel("./Data/vehicles.xlsx")
df.x <- df[,-20]

boxplot(df.x)

# Missing Value Analysis ###
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


min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

normalized_df <- as.data.frame(lapply(clean_df, min_max_norm))
head(normalized_df)


# determine the number of cluster
# install.packages("NbClust")
library(NbClust)

# using NbClust determine the optimal number of clusters
clusterNo.euclidean = NbClust(normalized_df, distance="euclidean", min.nc=2,
                              max.nc=10, method="kmeans", index="all")
clusterNo.manhattan = NbClust(normalized_df, distance="manhattan", min.nc=2,
                              max.nc=15, method="kmeans", index="all")
clusterNo.maximum = NbClust(normalized_df, distance="maximum", min.nc=2,
                            max.nc=15, method="kmeans", index="all")

# Elbow Method
wss <- 0
for (i in 1:15){
  wss[i] <-
    sum(
      kmeans(normalized_df, centers=i)$withinss
    )
}

plot(1:15,
     wss,
     type="b",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# NbClust 
nc <- NbClust(normalized_df,
              min.nc=2, max.nc=15,
              method="kmeans")

table(nc$Best.n[1,])
barplot(table(clusterNo.euclidean$Best.n[1,]), xlab="Numer of Clusters",ylab="Number of Criteria",
        main="Number of Clusters Chosen by 30 Criteria")

# silhouette
install.packages("factoextra")
library(factoextra)
fviz_nbclust(normalized_df, kmeans, method = 'silhouette')

# gap
fviz_nbclust(normalized_df, kmeans, method = 'gap_stat')

# Elbow Method
fviz_nbclust(normalized_df, kmeans, method = 'wss')

k <- 2
kc <- kmeans(normalized_df,k)
kc
table(lable,kc$cluster)

bss <- kc$betweenss
bss
tss <- kc$totss
tss
wss <- kc$tot.withinss
table(lable,kc$cluster)



plot(kc[c("kc$betweenss", "kc$tot.withinss")], col=kc$cluster)

plot(normalized_df[c("Circ", "D.Circ")], col=kc$cluster)
points(kc$centers[,c("Circ", "D.Circ")], col=1:3, pch=23, cex=3)


test <- fviz_nbclust(normalized_df, kmeans, method = 'silhouette')

plot(test, main="Silhouette Plot")

summary(test)


library(cluster)

# Assume your k-means result is stored in a variable called "fit"
sil <- silhouette(kc$cluster, dist(normalized_df))
fviz_silhouette(sil)





















