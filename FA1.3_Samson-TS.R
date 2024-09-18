#FA1-3
# Load necessary libraries
library(readxl)
library(forecast)
library(ggplot2)

# Define the file path
file_path <- "C:/Users/User/OneDrive/Personal docs/FRESHMAN/4th yr - 1st Sem/Time Series/One-Step-Ahead Forecast Errors for FA1.xlsx"

# Read the Excel file
df <- read_excel(file_path)

# a. Calculate the sample ACF of the forecast errors
# Extract the forecast errors (e(1)) column
forecast_errors <- df$`e(1)`

# Plot the sample ACF
acf(forecast_errors, main = "Sample ACF of One-Step-Ahead Forecast Errors")

# Interpretation:
# The ACF plot shows the correlation of the forecast errors at different lags.
# If there is significant autocorrelation at any lag, it suggests that the forecast errors are not independent.
# Ideally, for a well-fitted forecasting model, the ACF should show no significant autocorrelations beyond lag 0.

# b. Construct a normal probability plot of the forecast errors
ggplot(df, aes(sample = forecast_errors)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q Plot of One-Step-Ahead Forecast Errors",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# Interpretation:
# The Q-Q plot compares the distribution of the forecast errors with a normal distribution.
# If the points follow the line closely, the errors are normally distributed.
# Deviations from the line indicate departures from normality.

#--------------------------------------------------------------------------------------

# Extract the forecast errors (e(1)) column
forecast_errors <- df$`e(1)`

# c. Check if the forecast errors are normally distributed
# Perform Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(forecast_errors)
print(shapiro_test)  # p-value < 0.05 would indicate non-normal distribution

# d. Calculate and plot the ACF of the forecast errors to check for autocorrelation
acf(forecast_errors, main = "Sample ACF of Forecast Errors")

# e. Calculate the mean error, mean squared error, and mean absolute deviation
mean_error <- mean(forecast_errors)
mean_squared_error <- mean(forecast_errors^2)
mean_absolute_deviation <- mean(abs(forecast_errors))

# Print the results
cat("Mean Error:", mean_error, "\n")
cat("Mean Squared Error:", mean_squared_error, "\n")
cat("Mean Absolute Deviation:", mean_absolute_deviation, "\n")

# f. Is it likely that the forecasting method produces unbiased forecasts?
# If the mean error is close to 0, the forecasts are likely unbiased
if (abs(mean_error) < 1e-5) {
  cat("The forecasting method is likely unbiased.\n")
} else {
  cat("The forecasting method is likely biased.\n")
}
