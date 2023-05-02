pca_result <- prcomp(normalized_df, scale = FALSE)
names(pca_result)

summary(pca_result)

pca_result$rotation <- -pca_result$rotation
pca_result$rotation

pca_result$x <- - pca_result$x
head(pca_result$x)
biplot(pca_result, scale = 0)

(VE <- pca_result$sdev^2)
PVE <- VE / sum(VE)
round(PVE, 2)

varPercent <- PVE*100
barplot(varPercent, xlab='PC', ylab='Percent Variance', names.arg=1:length(varPercent), las=1, ylim=c(0,
                                                                                                      max(varPercent)), col='gray')
abline(h=1/ncol(USArrests)*100, col='red')

pca_wines = as.data.frame(pca_result$x[,1:2])

library(factoextra)
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
sil <- silhouette(kmeans_wines$cluster, dist(pca_wines))
fviz_silhouette(sil)







