# Load required libraries
library(tidyverse)
library(forecast)
library(zoo)
library(readr)
library(forecast)
library(Metrics)
library(readxl)

# Read the Excel file
Sales <- read_excel("/Users/allenwalker999/Sales.xlsx")

# Inspect the data
str(Sales)
head(Sales)

# Trim column names
data$Sales
colnames(Sales) <- trimws(colnames(Sales))
# 1. Compute three-, four-, and five-week moving averages for the time series.

ma3 <- stats::filter(Sales$Sales, rep(1/3, 3), sides = 2)

ma4 <- stats::filter(Sales$Sales, rep(1/4, 4), sides = 2)

ma5 <- stats::filter(Sales$Sales, rep(1/5, 5), sides = 2)


Sales$MA3 <- ma3

Sales$MA4 <- ma4

Sales$MA5 <- ma5

# View results
head(Sales)

# Fit linear trend
trend_model <- lm(Sales ~ Week, data = Sales)

# Plot sales and moving averages
plot(Sales$Week, Sales$Sales, type="o", col="black",
     xlab="Week", ylab="Sales",
     main="Sales with Moving Averages")

lines(Sales$Week, Sales$MA3, col="blue", lwd=2)
lines(Sales$Week, Sales$MA4, col="red", lwd=2)
lines(Sales$Week, Sales$MA5, col="green", lwd=2)

# Add linear trend line
abline(trend_model, col="purple", lwd=3)

# Legend
legend("topleft",
       legend=c("Sales","MA3","MA4","MA5","Trend"),
       col=c("black","blue","red","green","purple"),
       lty=1, lwd=2)

# 2. Compute exponential smoothing forecasts for the time series using alpha = 0.5.

ts_sales <- ts(Sales$Sales)

ses_model <- ses(ts_sales, alpha = 0.5, h = 5)

ses_model

# 3. Compute the MSE, MAE, and MAPE for each of your forecasts.

ts_sales <- ts(Sales$Sales)

ses_model <- ses(ts_sales, alpha = 0.5)

fitted_vals <- fitted(ses_model)
actual_vals <- Sales$Sales

Saleserrors <- actual_vals - fitted_vals

MSE  <- mean(Saleserrors^2, na.rm = TRUE)
MAE  <- mean(abs(Saleserrors), na.rm = TRUE)
MAPE <- mean(abs(Saleserrors / actual_vals) * 100, na.rm = TRUE)

MSE
MAE
MAPE

# 4. Find the value for alpha the minimizes the MSE for your exponential smoothing forecast.

#  Convert sales to a time series
ts_sales <- ts(Sales$Sales)

#  Fit simple exponential smoothing without specifying alpha
#    This lets R automatically find the alpha that minimizes MSE
ses_opt <- ses(ts_sales, h = 5, initial = "simple", alpha = NULL)

#  Extract the optimal alpha
optimal_alpha <- ses_opt$model$par[1]
optimal_alpha  # This is the alpha that minimizes MSE

# 5. Comparing the MSE, MAE, and MAPE—which of all of your forecasts performs the best?

# Compute errors for moving averages
MA3_errors <- Sales$Sales - Sales$MA3
MA4_errors <- Sales$Sales - Sales$MA4
MA5_errors <- Sales$Sales - Sales$MA5

# Compute MSE, MAE, MAPE for moving averages
MA3_MSE  <- mean(MA3_errors^2, na.rm = TRUE)
MA3_MAE  <- mean(abs(MA3_errors), na.rm = TRUE)
MA3_MAPE <- mean(abs(MA3_errors / Sales$Sales) * 100, na.rm = TRUE)

MA4_MSE  <- mean(MA4_errors^2, na.rm = TRUE)
MA4_MAE  <- mean(abs(MA4_errors), na.rm = TRUE)
MA4_MAPE <- mean(abs(MA4_errors / Sales$Sales) * 100, na.rm = TRUE)

MA5_MSE  <- mean(MA5_errors^2, na.rm = TRUE)
MA5_MAE  <- mean(abs(MA5_errors), na.rm = TRUE)
MA5_MAPE <- mean(abs(MA5_errors / Sales$Sales) * 100, na.rm = TRUE)

# Combine SES and MA metrics for comparison

error_table <- tibble(
  Method = c("MA3", "MA4", "MA5", "SES"),
  MSE  = c(MA3_MSE, MA4_MSE, MA5_MSE, MSE),
  MAE  = c(MA3_MAE, MA4_MAE, MA5_MAE, MAE),
  MAPE = c(MA3_MAPE, MA4_MAPE, MA5_MAPE, MAPE)
)

error_table


# Case Problem 1: Forecasting Food and Beverage Sales for Karen
# 1. A Time Series plot. Comment on the underlying pattern in the time series.

# Read the Excel file
Vintage <- read_excel("Vintage.xlsx")

# Convert data into a time series object
# frequency = 12 because data is monthly
ts_sales <- ts(Vintage$Sales, frequency = 12)

# Create time series plot
plot(ts_sales,
     main = "Monthly Food & Beverage Sales",
     ylab = "Sales ($1,000s)",
     xlab = "Time",
     col = "blue",
     lwd = 2)

# Use numeric time for regression so trend matches data
time_numeric <- 1:length(ts_sales)

# Fit linear trend model
linear_model <- lm(ts_sales ~ time_numeric)

# Add the upward-sloping trend line
lines(time_numeric, predict(linear_model), col = "red", lwd = 2)

# 2. Using the dummy variable approach, forecast sales from January through December of the fourth year. How would you explain this model to Karen?

# Create time variable
t <- 1:nrow(Vintage)

# Create month variable (1–12 repeated for 3 years)
month <- as.factor(rep(1:12, length.out = nrow(Vintage)))

# Combine into dataframe
data <- data.frame(Sales = Vintage$Sales,  t = t, month = month)

dummymodel <- lm(Sales ~ t + month, data = data)
summary(dummymodel)

# 3. 

# January of Year 4
t_future <- 37

forecast_jan4 <- 249.10764 + 1.01736 * t_future
forecast_jan4

# Actual value
actual <- 295

# Forecast error
error <- actual - forecast_jan4
error
