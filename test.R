#Removed all the existing objects
rm(list = ls())

# install and load readxl package
# install.packages('readxl')
library(readxl)

#Setting the working directory
getwd()

#Load the dataset
df <- read_excel("./Data/vehicles.xlsx")
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

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

normalized_df <- as.data.frame(lapply(clean_df, min_max_norm))
head(normalized_df)


# determine the number of cluster
# install.packages("NbClust")
library(NbClust)

# using NbClust determine the optimal number of clusters
clusterNo=NbClust(normalized_df,distance="euclidean", min.nc=2,max.nc=10,
                  method="kmeans",index="all")
clusterNo=NbClust(normalized_df, distance="manhattan", min.nc=2,max.nc=15,
                  method="kmeans",index="all")
clusterNo=NbClust(normalized_df, distance="maximum", min.nc=2,max.nc=15,
                  method="kmeans",index="all")

k = 2:10
WSS = sapply(k, function(k) {kmeans(normalized_df, centers=k)$tot.withinss})
plot(k, WSS, type="l", xlab= "Number of k", ylab="Within sum of squares")

nc <- NbClust(normalized_df,
              min.nc=2, max.nc=15,
              method="kmeans")

table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Numer of Clusters",ylab="Number of Criteria",
        main="Number of Clusters Chosen by 30 Criteria")
wss <- 0
for (i in 1:15){
  wss[i] <-
    sum(kmeans(normalized_df, centers=i)$withinss)
}

plot(1:15,
     wss,
     type="b",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")


