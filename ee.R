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
df <- df[2,19]
df <- df[,-1]

library(dplyr)
library(ggplot2)

zscore_cols <- c(1:18) # replace with the indices of the columns for which you want to calculate the zscore
df[zscore_cols] <- lapply(df[zscore_cols], function(x) (x - mean(x)) / sd(x))

df_filtered <- df %>% filter(if_all(all_of(zscore_cols), ~ . >= -2 & . <= 2))
df_filtered <- df %>% filter_at(vars(zscore_cols), all_vars(. >= -2 & . <= 2))


clusterNo = NbClust(normalized_df, distance="euclidean", min.nc=2,
                    max.nc=10, method="kmeans", index="all")

table(clusterNo$Best.n[1,])
barplot(table(clusterNo$Best.n[1,]), xlab="Numer of Clusters",ylab="Number of Criteria",
        main="Number of Clusters Chosen by 30 Criteria") # NBClust suggest 2

normalized_df <- as.data.frame(scale(df))

# silhouette
fviz_nbclust(normalized_df, kmeans, method = 'silhouette') # silhouette suggest 2

# Elbow Method
fviz_nbclust(normalized_df, kmeans, method = 'wss') # Elbow suggest 

# gap
fviz_nbclust(normalized_df, kmeans, method = 'gap_stat') # gap Suggest 


fviz_cluster(kc, data = normalized_df)

