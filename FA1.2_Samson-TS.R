#FA1-2
# Load required libraries
library(readxl)
library(ggplot2)

# Set the correct file path with double backslashes
file_path <- "C:\\Users\\User\\OneDrive\\Personal docs\\FRESHMAN\\4th yr - 1st Sem\\Applied Multivariate Data Analysis\\U.S. National Violent Crime Rate.xlsx"

# Read the Excel file
data <- read_excel(file_path)

# Check the column names to see if they match what we're expecting
print(colnames(data))

# Check the first few rows of the data to ensure the content was loaded correctly
print(head(data))

# After identifying the correct column name for the crime rate, update this part
# For example, if the column name is "ViolentCrimeRate(per100,000inhabitants)"
data$Year <- as.numeric(data$Year)
data$ViolentCrimeRate <- as.numeric(data[[2]])  # Assuming the crime rate is in the second column

# a. Apply 7-period moving average
data$MovingAverage7 <- stats::filter(data$ViolentCrimeRate, rep(1/7, 7), sides = 2)

# Plot original data and 7-period moving average
p1 <- ggplot(data, aes(x = Year)) +
  geom_line(aes(y = ViolentCrimeRate, color = "Original")) +
  geom_line(aes(y = MovingAverage7, color = "7-Period Moving Average")) +
  labs(title = "Violent Crime Rate with 7-Period Moving Average", 
       y = "Crime Rate (per 100,000 inhabitants)") +
  scale_color_manual("", breaks = c("Original", "7-Period Moving Average"),
                     values = c("Original" = "black", "7-Period Moving Average" = "blue")) +
  theme_minimal()

print(p1)

# b. The moving average smooths the data by reducing short-term fluctuations.

# c. Apply 3-period moving average
data$MovingAverage3 <- stats::filter(data$ViolentCrimeRate, rep(1/3, 3), sides = 2)

# Plot original data and 3-period moving average
p2 <- ggplot(data, aes(x = Year)) +
  geom_line(aes(y = ViolentCrimeRate, color = "Original")) +
  geom_line(aes(y = MovingAverage3, color = "3-Period Moving Average")) +
  labs(title = "Violent Crime Rate with 3-Period Moving Average", 
       y = "Crime Rate (per 100,000 inhabitants)") +
  scale_color_manual("", breaks = c("Original", "3-Period Moving Average"),
                     values = c("Original" = "black", "3-Period Moving Average" = "red")) +
  theme_minimal()

print(p2)
