#Removed all the existing objects
rm(list = ls())

library(readxl)
#Load the dataset
df <- read_excel("Data/vehicles.xlsx")
df.without_lbl <- df[2:19]

detect_outliers <- function(x) {
  box <- boxplot(x, plot = FALSE)
  outliers <- x < box$stats[1] | x > box$stats[5]
  outliers
}
outliers <- apply(df.without_lbl, 2,detect_outliers)
summary(outliers)
df_clean <- df
df_clean[outliers] <- NA
clean_df <- na.omit(df_clean)
clean_df.lbl <- clean_df
clean_df <- clean_df[,2:19]
apply(clean_df, 2, var)

# Min max normalization
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

normalized_df <- as.data.frame(apply(clean_df,2, min_max_norm))
summary(normalized_df)