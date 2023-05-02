library(tidyverse)   
library(gridExtra)  
library(ggplot2)    
library(rlang)         

vehicle.cov <- cov(normalized_df)
vehicle.eigen <- eigen(vehicle.cov)
str(vehicle.eigen)                     

(phi <- vehicle.eigen$vectors)  
eigenvalues <- vehicle.eigen$values
eigenvalues
phi <- -phi
cumulative_prop_var <- cumsum(eigenvalues/sum(eigenvalues))
cumulative_prop_var
num_attributes <- which(cumulative_prop_var > 0.92)[1]
num_attributes
# Plot the cumulative proportion of variance explained by each principal component
plot(cumulative_prop_var, xlab = "Number of Principal Components", ylab = "Cumulative Proportion of Variance Explained", type = "b")


phi <- phi[,1:6]
row.names(phi) <- c("Comp",	"Circ",	"D.Circ",	"Rad.Ra",	"Pr.Axis.Ra",	"Max.L.Ra",
                    "Scat.Ra",	"Elong",	"Pr.Axis.Rect",	"Max.L.Rect",	"Sc.Var.Maxis",
                    "Sc.Var.maxis",	"Ra.Gyr",	"Skew.Maxis",	"Skew.maxis",	"Kurt.maxis",
                    "Kurt.Maxis",	"Holl.Ra")
colnames(phi) <- c("PC1", "PC2","PC3","PC4","PC5","PC6")
phi


library(NbClust)
set.seed(1235)
# using NbClust determine the optimal number of clusters
clusterNo = NbClust(phi, distance="euclidean", min.nc=2, max.nc = 10, method="kmeans")

table(clusterNo$Best.n[1,])
barplot(table(clusterNo$Best.n[1,]), xlab="Numer of Clusters",ylab="Number of Criteria",
        main="Number of Clusters 
Chosen by 30 Criteria") # NBClust suggest 3

sum(is.na(normalized_df))
library(factoextra)
# silhouette
fviz_nbclust(phi, kmeans, method = 'silhouette') # silhouette suggest 2 


# Elbow Method
fviz_nbclust(phi, kmeans, method = 'wss') # Elbow suggest 3

# gap
fviz_nbclust(phi, kmeans, method = 'gap_stat') # gap Suggest 3

set.seed(1234)
# use k as a 3
k <- 3
kmeans_fit  = kmeans(phi, centers = k, nstart = 10)
kmeans_fit 


