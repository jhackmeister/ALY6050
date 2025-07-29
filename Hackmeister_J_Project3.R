##############################
##### ALY 6050 Project 3 #####
##############################

# Load Libraries 
library(readxl)
library(forecast)
library(tidyverse)
library(zoo)
library(car)

# Load data from assignment 
data <- read_excel("C:/Users/jhack/OneDrive/Northeastern/ALY6050/Module 3/Project Files/ALY6050_Module3Project_Data.xlsx", 
                   sheet = "6050_Module3Project_Data")

# data cleaning 
colSums(is.na(data))
data <- na.omit(data) # remove 5 empty rows

# split by company

appl <- data$`AAPL (Apple Inc) / $`
hon <- data$`HON (Honeywell Inc)  /  $`

#################################
# Part 1 Short-Term Forecasting #
#################################

# i. line plots

appl_plot <- ggplot(data, aes(x=Period, y = `AAPL (Apple Inc) / $`)) +
  geom_line(color = 'red') +
  labs(
    title = "Apple Stock Price Over Time",
    subtitle = "2019-11-08 through 2020-11-06",
    x = "Time Period",
    y = "Stock Price ($)"
  ) +
  theme_minimal()

hon_plot <- ggplot(data, aes(x=Period, y = `HON (Honeywell Inc)  /  $`)) +
  geom_line(color = 'darkgreen') +
  labs(
    title = "Honeywell Stock Price Over Time",
    subtitle = "2019-11-08 through 2020-11-06",
    x = "Time Period",
    y = "Stock Price ($)"
  ) +
  theme_minimal()

combined_plot <- ggplot(data, aes(x = Period)) +
  geom_line(aes(y = `AAPL (Apple Inc) / $`, color = "Apple"), linewidth = 1) +
  geom_line(aes(y = `HON (Honeywell Inc)  /  $`, color = "Honeywell"), linewidth = 1) +
  scale_color_manual(
    name = "Company",
    values = c("Apple" = "red", "Honeywell" = "darkgreen")
  ) +
  labs(
    title = "Apple vs Honeywell Stock Price Over Time",
    subtitle = "2019-11-08 through 2020-11-06",
    x = "Time Period",
    y = "Stock Price ($)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# ii. Exponential smoothing forecast for period 253
# Forecast = α × (most recent value) + (1-α) × (previous forecast)
# MAPE = (1/n) × Σ |Actual - Predicted| / |Actual| × 100

# Define exponential smoothing function 
exp_smoothing <- function(data, alpha) {
  model <- HoltWinters(data, alpha = alpha, beta = FALSE, gamma = FALSE)
  forecast <- forecast(model, h = 1)
# calculate MAPE
  mape <- mean(abs((data - forecast$fitted) / data), na.rm = TRUE) * 100
  return(list(forecast = forecast, mape = mape))
}

# apply to various alpha levels

appl_exsmo <- lapply(c(0.15, 0.35, 0.55, 0.75), function(alpha) exp_smoothing(appl, alpha))
hon_exsmo <- lapply(c(0.15, 0.35, 0.55, 0.75), function(alpha) exp_smoothing(hon, alpha))

sapply(appl_exsmo, function(x) x$mape)
sapply(hon_exsmo, function(x) x$mape)

# Find best alpha for AAPL
appl_mapes <- sapply(appl_exsmo, function(x) x$mape)
best_appl_alpha_index <- which.min(appl_mapes)
best_appl_forecast <- appl_exsmo[[best_appl_alpha_index]]$forecast$mean


# Find best alpha for HON
hon_mapes <- sapply(hon_exsmo, function(x) x$mape)
best_hon_alpha_index <- which.min(hon_mapes)
best_hon_forecast <- hon_exsmo[[best_hon_alpha_index]]$forecast$mean

# iii. Adjusted exponential smoothing 

# Define the adjusted exponential smoothing function 
adj_exp_smoothing <- function(data, alpha, beta) {
  model <- HoltWinters(data, alpha = alpha, beta = beta, gamma = FALSE)
  forecast <- forecast(model, h = 1)
  mape <- mean(abs((data - forecast$fitted) / data), na.rm = TRUE) * 100
  return(list(forecast = forecast, mape = mape))
}

appl_adj_exsmo <- lapply(c(0.15, 0.25, 0.45, 0.85), function(beta) adj_exp_smoothing(appl, 0.55, beta))
hon_adj_exsmo <-  lapply(c(0.15, 0.25, 0.45, 0.85), function(beta) adj_exp_smoothing(hon, 0.55, beta))

# MAPE results for appl
sapply(appl_adj_exsmo, function(x) x$mape)
sapply(hon_adj_exsmo, function(x) x$mape)

appl_adj_exsmo[[1]]$forecast$mean
hon_adj_exsmo[[1]]$forecast$mean

#################################
# Part 2 Long-Term Forecasting ##
#################################

# Define the 3-period weighted moving average function
weighted_moving_average <- function(data, weights) {
  n <- length(data)
  forecast <- rep(NA, n)  # Initialize with NA
  weights <- weights / sum(weights)  # Normalize weights to sum to 1
  
  if (length(weights) != 3) {
    stop("Weights vector must have exactly 3 elements.")
  }
  
  for (i in 3:n) {
    forecast[i] <- sum(data[(i-2):i] * weights)
  }
  return(forecast)
}

# Ensure data is a numeric vector
appl <- as.numeric(appl)
hon <- as.numeric(hon)

# Define weights
weights <- c(0.2, 0.3, 0.5) 

# Apply 3-period weighted moving average for AAPL and HON data (first 100 periods)
appl_wma <- weighted_moving_average(appl[1:100], weights)
hon_wma <- weighted_moving_average(hon[1:100], weights)

# Define training period indices for trend forecasting
train_period <- 101:252

# Linear trend forecasting for AAPL (fit using periods 101-252)
appl_train <- data.frame(Period = train_period, Value = appl[train_period])
appl_trend <- lm(Value ~ Period, data = appl_train)

# Forecast periods 101-257
forecast_period <- 101:257
newdata_appl <- data.frame(Period = forecast_period)
appl_trend_forecast <- predict(appl_trend, newdata = newdata_appl)

# Linear trend forecasting for HON (fit using periods 101-252)
hon_train <- data.frame(Period = train_period, Value = hon[train_period])
hon_trend <- lm(Value ~ Period, data = hon_train)

# Forecast periods 101-257
newdata_hon <- data.frame(Period = forecast_period)
hon_trend_forecast <- predict(hon_trend, newdata = newdata_hon)

# **Fix the Length Issue**
# Ensure forecasts have exactly 257 elements
appl_forecasts <- c(appl_wma, rep(NA, 100 - length(appl_wma)), appl_trend_forecast)
hon_forecasts <- c(hon_wma, rep(NA, 100 - length(hon_wma)), hon_trend_forecast)

# Extract forecasts for periods 253-257
appl_forecasts_253_257 <- setNames(appl_forecasts[253:257], 253:257)
hon_forecasts_253_257 <- setNames(hon_forecasts[253:257], 253:257)

# Forecasts for 101 - 257
setNames(appl_forecasts[101:257], 101:257)
setNames(hon_forecasts[101:257], 101:257)


# ii. MAPE 

# Define the linear trend forecasting function
linear_trend <- function(data, start_period) {
  n <- length(data)
  trend <- lm(data[start_period:n] ~ c(start_period:n))
  forecast <- predict(trend, newdata = data.frame(c(start_period:n)))
  return(forecast)
}

# Apply linear trend forecasting to AAPL data
appl_trend <- linear_trend(appl, 101)

# Apply linear trend forecasting to HON data
hon_trend <- linear_trend(hon, 101)

# Calculate MAPE for AAPL long-term forecast
appl_long_mape <- mean(abs((appl[101:length(appl)] - appl_trend) / appl[101:length(appl)])) * 100

# Calculate MAPE for HON long-term forecast
hon_long_mape <- mean(abs((hon[101:length(hon)] - hon_trend) / hon[101:length(hon)])) * 100

# Display MAPE results for long-term forecasts
appl_long_mape
hon_long_mape

##########################
####### Regression #######
##########################

# i. Simple Regression 

# Perform simple regression for AAPL
appl_regression <- lm(`AAPL (Apple Inc) / $` ~ Period, data = data)
appl_regression_forecast <- predict(appl_regression, newdata = data.frame(Period = 1:257))

# Perform simple regression for HON
hon_regression <- lm(`HON (Honeywell Inc)  /  $` ~ Period, data = data)
hon_regression_forecast <- predict(hon_regression, newdata = data.frame(Period = 1:257))


# Calculate MAPE for AAPL regression forecast
appl_actual <- data$`AAPL (Apple Inc) / $`[1:257]
appl_regression_mape <- mean(abs((appl_actual - appl_regression_forecast) / appl_actual), na.rm = TRUE) * 100

# Calculate MAPE for HON regression forecast
hon_actual <- data$`HON (Honeywell Inc)  /  $`[1:257]
hon_regression_mape <- mean(abs((hon_actual - hon_regression_forecast) / hon_actual), na.rm = TRUE) * 100

# Display MAPE results for regression forecasts
appl_regression_mape
hon_regression_mape

# ii. Regression analysis 

# 1. Independence 
appl_resid <- residuals(appl_regression)
appl_fitted <- fitted(appl_regression)
appl_ind <- data.frame(Fitted = appl_fitted, Residuals = appl_resid)

ggplot(appl_ind, aes(x = Fitted, y = Residuals)) +
  geom_point(color = 'red') +
  geom_hline(yintercept = 0, color = 'black') +
  labs(
    title = "Apple Residuals vs Fitted")+
  theme_minimal()

hon_resid <- residuals(hon_regression)
hon_fitted <- fitted(hon_regression)
hon_ind <- data.frame(Fitted = hon_fitted, Residuals = hon_resid)

ggplot(hon_ind, aes(x = Fitted, y = Residuals)) +
  geom_point(color = 'darkgreen') +
  geom_hline(yintercept = 0, color = 'black') +
  labs(
    title = "Hon Residuals vs Fitted")+
  theme_minimal()

# 2. Homoscedasticity using the Breusch-Pagen test
ncvTest(appl_regression)
ncvTest(hon_regression)

#3. Normal distribution 

qqPlot(appl_resid, main = "Apple Normal Q-Q Plot", col = 'red')
qqPlot(hon_resid, main = "Hon Normal Q-Q Plot", col = 'darkgreen')

hist(appl_resid)
hist(hon_resid)

#4. Statistical test
shapiro.test(appl_resid)
shapiro.test(hon_resid)
