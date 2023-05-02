rm(list = ls())

# load package
library(readxl)
library(factoextra)
library(NbClust)
library(cluster)

#Load the dataset
df <- read_excel("Data/vehicles.xlsx")
clean_df <- df[,2:19]

normalized_df <- scale(clean_df)
boxplot(normalized_df)

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
set.seed(11)
k <- 20
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



library(fpc)    #remember to install first this package!!
plotcluster(normalized_df, kc$cluster)


sil <- silhouette(kc$cluster, dist(normalized_df))
fviz_silhouette(sil)
summary(sil)
