

library(readxl)

df <- read_excel("Data/vehicles.xlsx")
df <- df[2:19]

result_df <- data.frame(column = character(),
                        p_value = numeric(),
                        status = character(),
                        stringsAsFactors = FALSE)


df <- clean_df
# check normality of data 
for (col in names(df)) {
  res <- shapiro.test(df[[col]])
  if(res$p.value > 0.05){
    result_df <- rbind(result_df, data.frame(column = col, p_value = res$p.value, status = "Ok"))
  }else{
    result_df <- rbind(result_df, data.frame(column = col, p_value = res$p.value, status = "Not Ok"))
  }
}
