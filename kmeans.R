library(NbClust)
set.seed(10)
# using NbClust determine the optimal number of clusters
clusterNo = NbClust(normalized_df, distance="euclidean", min.nc=2,
                    max.nc=10, method="kmeans", index="all")

table(clusterNo$Best.n[1,])
barplot(table(clusterNo$Best.n[1,]), xlab="Numer of Clusters",ylab="Number of Criteria",
        main="Number of Clusters Chosen by 30 Criteria") # NBClust suggest 2

library(factoextra)
# silhouette
fviz_nbclust(normalized_df, kmeans, method = 'silhouette') # silhouette suggest 2

# Elbow Method
fviz_nbclust(normalized_df, kmeans, method = 'wss') # Elbow suggest 3

# gap
fviz_nbclust(normalized_df, kmeans, method = 'gap_stat') # gap Suggest 3

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


plot(normalized_df[c("Circ", "Rad.Ra")], col=kc$cluster)
points(kc$centers[,c("Circ", "Rad.Ra")], col=1:3, pch=23, cex=3)


sil <- silhouette(kc$cluster, dist(normalized_df))
fviz_silhouette(sil)
summary(sil)

