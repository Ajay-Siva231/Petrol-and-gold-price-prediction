#Gold to Petrol

transitions4 <- table(data$Gold_Change, data$Petrol_Change)
transitions4 <- as.matrix(transitions4)
print(transitions4)

# Normalize the matrix to get frequencies
transition_matrix4 <- prop.table(transitions4, 1)

# Print the transition matrix
print(transition_matrix4)

# Plot the transition digraph 
plotmat(transitions4, lwd = 2, relsize = 0.8)

# Calculate inital State Vector
total4 <- sum(transitions4)
initial_state_vector4 <- rowSums(transitions4) / total4
print(initial_state_vector4)

# Steady State Matrix 
Steady_state4 <- transition_matrix4 %^% 6
Steady_state4

# Markov Chain Transition
goldtopetrolprice <- (initial_state_vector4) %*% (Steady_state4)
goldtopetrolprice


