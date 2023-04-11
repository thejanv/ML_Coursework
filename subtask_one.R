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
