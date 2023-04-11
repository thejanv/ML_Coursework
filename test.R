# install and load readxl package
# install.packages('readxl')
library(readxl)

# read data from excel file
getwd()
df <- read_excel("vehicles.xlsx")
df$Class <- NULL

boxplot(df)