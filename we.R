library(dplyr)             #install first the package before calling it via the library command

consumption <- read_excel("Data/uow_consumption.xlsx")
lable <- consumption$date
nor_train <- consumption[1:380,3]
nor_test <- consumption[381:470,3]

tm_series <- nor_train
#Data pre-processing
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

normalized_df <- as.data.frame(lapply(tm_series, min_max_norm))



str(normalized_df)
# see here how we can create 4 columns in this I/O matrix. Check the time-delayed variables!

time_lagged_data <- bind_cols(G_previous3 = lag(normalized_df,4),
                              G_previous2 = lag(normalized_df,3),
                              G_previous = lag(normalized_df,2),
                              G_current = lag(normalized_df,1),
                              G_pred = normalized_df)   

names(time_lagged_data)[1] <- 'G_pred'
names(time_lagged_data)[2] <- 'G_current'
names(time_lagged_data)[3] <- 'G_previous'
names(time_lagged_data)[4] <- 'G_previous2'
names(time_lagged_data)[5] <- 'G_previous3'
# see here the existence of NA values due to that shifting
time_lagged_data 
# see here the use of complete.cases function to remove those rows with NA
time_lagged_data <- time_lagged_data[complete.cases(time_lagged_data),]
# see this time-delayed I/O configuration from these rows
head (time_lagged_data)

str(time_lagged_data)
# see here the length of this I/O matrix. It is l-m (i.e. m: number of input variables)
time_lagged_data
time_lagged_data$G_previous

library(neuralnet)
consumsion_nnar<- neuralnet(G_pred~G_pred+G_previous2+G_previous+G_previous3+G_current, hidden = c(3,2), data = time_lagged_data,
                            linear.output=TRUE,threshold= 0.01)
plot(consumsion_nnar)


io_matrix1 <- create_io_matrix(data = your_data, inputs = c("eleven_h_lag1", "eleven_h_lag2", "eleven_h_lag3", "eleven_h_lag4"))



# Create input/output matrices for time-delayed input vectors
io_matrix1 <- create_io_matrix(data = normalized_df, inputs = c("G_pred_lag1", "G_pred_lag2", "G_pred_lag3", "G_pred_lag4"))
io_matrix2 <- create_io_matrix(data = normalized_df, inputs = c("eleven_h_lag1", "eleven_h_lag2", "eleven_h_lag3", "eleven_h_lag4", "eleven_h_lag7"))

# Train neural network on io_matrix1
nn_model1 <- neuralnet(output ~ ., data = io_matrix1, hidden = c(3,2), linear.output = TRUE)

# Train neural network on io_matrix2
nn_model2 <- neuralnet(output ~ ., data = io_matrix2, hidden = c(3,2), linear.output = TRUE)


