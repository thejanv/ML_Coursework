#Removed all the existing objects
rm(list = ls())

# load package
library(readxl)
library(factoextra)
library(NbClust)
library(cluster)

#Load the dataset
df <- read_excel("Data/vehicles.xlsx")
df.x <- df[,2:19]


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
clean_df <- clean_df[,2:19]
boxplot(clean_df)
apply(clean_df, 2, var)


# Min max normalization
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

normalized_df <- as.data.frame(apply(clean_df,2, min_max_norm))
summary(normalized_df)

