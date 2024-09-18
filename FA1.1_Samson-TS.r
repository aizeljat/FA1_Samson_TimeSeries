#FA1-1
#1. a, b
# Load necessary package
library(readxl)

df <- read_excel("C:\\Users\\User\\OneDrive\\Personal docs\\FRESHMAN\\4th yr - 1st Sem\\Time Series\\United Kingdom Airline Miles Flown.xlsx")

# View the first few rows to verify
head(df)

# Convert the data to a time series object
miles_ts <- ts(df$`Miles(in millions)`, start = c(1964, 1), frequency = 12)

# Plot the time series
plot(miles_ts, main = "UK Airline Miles Flown", ylab = "Miles (in millions)", xlab = "Year")

# Step 2: Find the sample autocorrelation function (ACF)
acf(miles_ts, main = "Sample ACF for UK Airline Miles Flown")

#------------------------------------------------------------------------
#2. a,b,c
library(readxl)
library(ggplot2)
library(forecast)  # For ACF calculation

# Read the Excel file
df <- read_excel("C:/Users/User/OneDrive/Personal docs/FRESHMAN/4th yr - 1st Sem/Time Series/United Kingdom Airline Miles Flown.xlsx")

# View the first few rows of the dataset
head(df)

# Convert 'Month' to Date format
df$Month <- as.Date(paste0("01-", df$Month), format = "%d-%b-%Y")

# Check for NA values in the 'Month' and 'Miles' columns
sum(is.na(df$Month))
sum(is.na(df$`Miles(in millions)`))  # Corrected

# Take the natural logarithm of the 'Miles' data
df$log_Miles <- log(df$`Miles(in millions)`)  # Corrected

# Check for NA values in the 'log_Miles' column
sum(is.na(df$log_Miles))

# Remove rows with missing values in 'log_Miles'
df_clean <- df[complete.cases(df$log_Miles), ]

# Plot the log-transformed time series
ggplot(df_clean, aes(x = Month, y = log_Miles)) +
  geom_line(color = "blue") +
  labs(title = "Log-Transformed UK Airline Miles Flown",
       x = "Month",
       y = "Log(Miles)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x labels for better readability

# Calculate and plot the Autocorrelation Function (ACF)
acf(df_clean$log_Miles, main = "ACF of Log-Transformed UK Airline Miles Flown")

#------------------------------------------------------------------------
#3. a,b,c
# Load necessary packages
library(readxl)
library(ggplot2)
library(forecast)  # For ACF calculation

# Read the Excel file
df <- read_excel("C:/Users/User/OneDrive/Personal docs/FRESHMAN/4th yr - 1st Sem/Time Series/United Kingdom Airline Miles Flown.xlsx")

# View the first few rows of the dataset to ensure it's loaded correctly
head(df)

# Take the natural logarithm of the 'Miles' data
df$log_Miles <- log(df$`Miles(in millions)`)

# Take the first difference of the log-transformed data
diff_log_Miles <- diff(df$log_Miles)

# Create a new data frame for the differenced series
df_diff <- data.frame(
  Month = df$Month[-1],  # Remove the first row to align with differencing
  diff_log_Miles = diff_log_Miles
)

# Plot the first difference of the log-transformed time series
ggplot(df_diff, aes(x = Month, y = diff_log_Miles)) +
  geom_line(color = "blue") +
  labs(title = "First Difference of Log-Transformed UK Airline Miles Flown",
       x = "Month",
       y = "First Difference of Log(Miles)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x labels for better readability

# Calculate and plot the Autocorrelation Function (ACF)
acf(df_diff$diff_log_Miles, main = "ACF of Differenced Log-Transformed Miles")

#------------------------------------------------------------------------
#4. a,b,c
# Load necessary libraries
library(readxl)
library(ggplot2)
library(forecast)

df <- read_excel("C:\\Users\\User\\OneDrive\\Personal docs\\FRESHMAN\\4th yr - 1st Sem\\Time Series\\United Kingdom Airline Miles Flown.xlsx")

# Inspect column names to find the exact name of the 'Miles' column
print(colnames(df))

miles_ts <- ts(df$`Miles(in millions)`, start = c(1964, 1), frequency = 12)

# Apply log transformation
log_miles_ts <- log(miles_ts)

# Difference the data at a seasonal lag of 12 months and apply a first difference
diff_log_miles_ts <- diff(log_miles_ts, differences = 1)
seasonally_diff_log_miles_ts <- diff(diff_log_miles_ts, lag = 12)

# Plot the differenced series
autoplot(seasonally_diff_log_miles_ts) +
  ggtitle("Seasonally and First Differenced Log-Transformed UK Airline Miles") +
  ylab("Differenced Log(Miles)") +
  xlab("Year")

# Find the sample autocorrelation function
acf(seasonally_diff_log_miles_ts, main = "ACF of Seasonally and First Differenced Log-Transformed Miles")

# Interpretation
cat("a. Effect of Differencing: Differencing has removed the trend and reduced seasonality, stabilizing the mean of the series.\n")
cat("b. Sample Autocorrelation Function: The ACF plot shows minimal autocorrelation in the differenced series, indicating the series is close to white noise.\n")
cat("c. Behavior of the Differenced Series: The differencing has made the series more stationary by removing long-term trends and seasonality.\n")

#-----------------------------------------------------------------------------

