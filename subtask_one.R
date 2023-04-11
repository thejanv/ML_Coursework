#Removed all the existing objects
rm(list = ls())

# load package
# install.packages('readxl')
library(readxl)
library(factoextra)
library(NbClust)

#Load the dataset
df <- read_excel("Data/vehicles.xlsx")
df<- df[,-20]

boxplot(df)

# Missing Value Analysis ###
sum(is.na(df))
summary(is.na(df))

# Tukey Fence
threshold <- 1.5
q1 <- apply(df, 2, quantile, 0.25)
q3 <- apply(df, 2, quantile, 0.75)
iqr <- q3 - q1
lower_fence <- q1 - threshold * iqr
upper_fence <- q3 + threshold * iqr
outliers <- apply(df, 2, function(x) x[x < lower_fence | x > upper_fence])

# remove outliers
clean_df <- df
for (col in names(df)) {
  clean_df[,col][df[,col] < lower_fence[col] | df[,col] > upper_fence[col]] <- NA
}
clean_df <- na.omit(clean_df)
boxplot(clean_df)

# Normalize data
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

normalized_df <- as.data.frame(lapply(clean_df, min_max_norm))
head(normalized_df)

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
