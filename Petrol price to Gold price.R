#Petrol to Gold

transitions3 <- table(data$Petrol_Change, data$Gold_Change)
transitions3 <- as.matrix(transitions3)
print(transitions3)

# Normalize the matrix to get frequencies
transition_matrix3 <- prop.table(transitions3, 1)

# Print the transition matrix
print(transition_matrix3)

# Plot the transition digraph
plotmat(transitions3, lwd = 2, relsize = 0.8)

# Calculate inital State Vector
total3 <- sum(transitions3)
initial_state_vector3 <- rowSums(transitions3) / total3
print(initial_state_vector3)

# Steady State Matrix
Steady_state3 <- transition_matrix3 %^% 6
Steady_state3

# Markov Chain Transition
Petroltogoldprice <- (initial_state_vector3) %*% (Steady_state3)
Petroltogoldprice
