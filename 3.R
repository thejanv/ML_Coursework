# load package
library(readxl)
library(factoextra)
library(NbClust)
library(cluster)

#Load the dataset
df <- read_excel("Data/vehicles.xlsx")
df.x <- df[,-20]
data <- df.x[,-1]
boxplot(data)

outlier_values <- boxplot(data$Rad.Ra)$out
boxplot(data$Rad.Ra,
        ylab = "hwy",
        main = "Boxplot of highway miles per gallon"
)
mtext(paste("Outliers: ", paste(outlier_values, collapse = ", ")))

sum(is.na(data_clean))
test <- function(x){
  outlier_values <- boxplot(x, plot=FALSE)$out
  data_clean <- replace(x, x %in% outlier_values, NA)
  data_clean
}

data_clean <- apply(data, 2, test)

clean_df <- as.data.frame(na.omit(data_clean))
boxplot(clean_df)
boxplot(data_clean)

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

sum(is.na(clean_df))

normalized_df <- as.data.frame(lapply(clean_df, min_max_norm))
summary(normalized_df)
