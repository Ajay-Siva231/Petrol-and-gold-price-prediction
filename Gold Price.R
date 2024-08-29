# Gold Price Prediction

# Descriptive Statistics
mean_price <- mean(gold_price, na.rm = TRUE)
median_price <- median(gold_price, na.rm = TRUE)
mode_gold_price <- as.numeric(names(sort(-table(petrol_price)))[1])
range_gold_price <- range(gold_price)
range_value <- diff(range_gold_price)
variance_gold_price <- var(gold_price)
kurtosis_gold_price <- kurtosis(gold_price)
skewness_gold_price <- skewness(gold_price)
sd_price <- sd(gold_price, na.rm = TRUE)
min_price <- min(gold_price, na.rm = TRUE)
max_price <- max(gold_price, na.rm = TRUE)
sum_gold_price <- sum(gold_price)
count_gold_price <- length(gold_price)

cat("Mean Price: ", mean_price, "\n")
cat("Median Price: ", mode_petrol_price, "\n")
cat("Mode: ", median_price, "\n")
cat("Range:", range_value, "\n")
cat("Sample Variance:", variance_gold_price, "\n")
cat("Kurtosis:", kurtosis_gold_price, "\n")
cat("Skewness:", skewness_gold_price, "\n")
cat("Standard Deviation: ", sd_price, "\n")
cat("Minimum Price: ", min_price, "\n")
cat("Maximum Price: ", max_price, "\n")
cat("Sum:", sum_gold_price, "\n")
cat("Count:", count_gold_price, "\n")


# Gold Price Line Plot
ggplot(data, aes(x = Date, y = `Gold price(24k)`)) + 
  geom_line(color = "blue") + 
  labs(x = "Date", 
       y = "Gold Price") +
  theme_minimal()


# Gold Price box Plot
boxplot(gold_price, 
        main = "Boxplot of Gold Prices", 
        ylab = "Price", 
        col = "gray")

transitions2 <- table(data$Gold_Change[-nrow(data)], data$Gold_Change[-1])
transitions2 <- as.matrix(transitions2)
print(transitions2)

# Normalize the matrix to get frequencies
transition_matrix2 <- prop.table(transitions2, 1)
print(transition_matrix2)

# Plot the transition diagram
plotmat(transitions2, lwd = 2, relsize = 0.8)

# Calculate inital State Vector
total2 <- sum(transitions2)
initial_state_vector2 <- rowSums(transitions2) / total2
print(initial_state_vector2)

# Steady State matrix
Steady_state2 <- transition_matrix2 %^% 9
Steady_state2

# Markov Chain Transition
goldprice <- (initial_state_vector2) %*% (Steady_state2)
goldprice