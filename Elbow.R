# Elbow Method
# type one
# Type one and two give cute little chart Third oe give a huge one 
k = 2:10
WSS = sapply(k, function(k) {
  kmeans(normalized_df, centers=k)$tot.withinss
})
plot(k, WSS, type="l", xlab= "Number of k", ylab="Within sum of squares")

# type two
wss <- 0
for (i in 1:15){
  wss[i] <-
    sum(
      kmeans(normalized_df, centers=i)$withinss
      )
}

plot(1:15, wss, type="b", xlab="Number of Clusters", 
     ylab="Within groups sum of squares")

# type three
# this also use withines
fviz_nbclust(normalized_df, kmeans, method = 'wss')