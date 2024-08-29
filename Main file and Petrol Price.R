# Data Pre Process

# Librarys
library(readr)
library(e1071)
library(ggplot2)
library(igraph)
library(expm)
library(diagram)

# Loading csv file
data <- read_csv("E:/Ex-Dowload/future.csv")
data <- na.omit(data)
head(data)

# Calculate the transition matrix by counting occurrences
data$Petrol_Change <- ifelse(c(NA, diff(data$`Petrol Price`)) > 0, "Increase",
                             ifelse(c(NA, diff(data$`Petrol Price`)) < 0, "Decrease", "Same"))
data$Petrol_Change


data$Gold_Change <- ifelse(c(NA, diff(data$`Gold price(24k)`)) > 0, "Increase",
                             ifelse(c(NA, diff(data$`Gold price(24k)`)) < 0, "Decrease", "Same"))
data$Gold_Change


# Variable name
petrol_price <- data$`Petrol Price`
gold_price <- data$`Gold price(24k)`

# Correlation Matrix
correlation_matrix <- cor(data[, c("Petrol Price", "Gold price(24k)")], use = "complete.obs")
print(correlation_matrix)

# Scatter Plot
ggplot(data, aes(x = `Petrol Price`, y = `Gold price(24k)`)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of Petrol Price vs Gold price(24k)",
       x = "Petrol Price",
       y = "Gold price(24k)") +
  theme_minimal()

# Descriptive Statistics
mean_price <- mean(petrol_price, na.rm = TRUE)
median_price <- median(petrol_price, na.rm = TRUE)
mode_petrol_price <- as.numeric(names(sort(-table(petrol_price)))[1])
range_petrol_price <- range(petrol_price)
range_value <- diff(range_petrol_price)
variance_petrol_price <- var(petrol_price)
kurtosis_petrol_price <- kurtosis(petrol_price)
skewness_petrol_price <- skewness(petrol_price)
sd_price <- sd(petrol_price, na.rm = TRUE)
min_price <- min(petrol_price, na.rm = TRUE)
max_price <- max(petrol_price, na.rm = TRUE)
sum_petrol_price <- sum(petrol_price)
count_petrol_price <- length(petrol_price)

cat("Mean Price: ", mean_price, "\n")
cat("Median Price: ", median_price, "\n")
cat("Mode: ", mode_petrol_price, "\n")
cat("Range:", range_value, "\n")
cat("Sample Variance:", variance_petrol_price, "\n")
cat("Kurtosis:", kurtosis_petrol_price, "\n")
cat("Skewness:", skewness_petrol_price, "\n")
cat("Standard Deviation: ", sd_price, "\n")
cat("Minimum Price: ", min_price, "\n")
cat("Maximum Price: ", max_price, "\n")
cat("Sum:", sum_petrol_price, "\n")
cat("Count:", count_petrol_price, "\n")

# Change the Date Formate
data$Date <- as.Date(data$Date, format = "%d-%b-%y")

# Petrol Price Line plot
ggplot(data, aes(x = Date, y = `Petrol Price`)) + 
  geom_line(color = "blue") + 
  labs(x = "Date", 
       y = "Petrol Price",
       title = "Petrol Price") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Petrol Price Box plot
boxplot(petrol_price, 
        main = "Boxplot of Petrol Prices", 
        ylab = "Price", 
        col = "gray")

transitions1 <- table(data$Petrol_Change[-nrow(data)], data$Petrol_Change[-1])
transitions1 <- as.matrix(transitions1)
print(transitions1)

# Normalize the matrix to get frequencies
transition_matrix1 <- prop.table(transitions1, 1)
print(transition_matrix1)

# Plot the transition diagram
plotmat(transitions1, lwd = 2, relsize = 0.8)

# Calculate inital State Vector
total1 <- sum(transitions1)
initial_state_vector1 <- rowSums(transitions1) / total1
print(initial_state_vector1)

# Steady State Matrix
Steady_state1 <- transition_matrix1 %^% 39
Steady_state1

# Markov Chain Transition
Petrolprice <- (initial_state_vector1) %*% (Steady_state1)
Petrolprice