# Generate some example data with outliers
set.seed(123)
df <- data.frame(x = rnorm(100, mean = 50, sd = 10),
                 y = rnorm(100, mean = 100, sd = 20))
df[c(10, 30, 60, 80), "x"] <- c(20, 80, 10, 90)
df[c(10, 30, 60, 80), "y"] <- c(10, 200, 50, 150)

# Define a function to detect outliers using box plots
detect_outliers <- function(x) {
  box <- boxplot(x, plot = FALSE)
  outliers <- x < box$stats[1] | x > box$stats[5]
  return(outliers)
}

# Apply the detect_outliers function to each column of the data frame using lapply
outliers <- lapply(df, detect_outliers)

# Combine the outlier data frames into a single data frame
outliers_df <- data.frame(outliers)

# Replace outlier values with NA in the data frame
df_clean <- df
df_clean[outliers_df] <- NA

# Create box plots of the original and cleaned data to visualize the outliers
par(mfrow = c(2, 2))
boxplot(df$x, main = "Box Plot of Original x")
boxplot(df$y, main = "Box Plot of Original y")
boxplot(df_clean$x, main = "Box Plot of Cleaned x")
boxplot(df_clean$y, main = "Box Plot of Cleaned y")
