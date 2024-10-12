# Exrecise 1

Credit <- read.table("Credit.dat") # loads the Credit dataset
attach(Credit) # so you can access its variables by name

# Compute the sample means and variances
n <- length(Rating)
x_bar <- mean(Rating)
y_bar <- mean(Balance)
s_xx <- sum((Rating - x_bar)^2)
s_xy <- sum((Rating - x_bar) * (Balance - y_bar))
s_yy <- sum((Balance - y_bar)^2)

# Compute the parameter estimates
B1_hat <- s_xy / s_xx
B0_hat <- y_bar - B1_hat * x_bar

# Compute predicted values
Balance_hat <- B0_hat + B1_hat * Rating
# Compute residuals
residuals <- Balance - Balance_hat
# Compute residual sum of squares (RSS)
rss <- sum(residuals^2)
# Compute degrees of freedom
df <- length(Balance) - 2
# Compute residual standard error (s)
s <- sqrt(rss / df)

# Define the range of Rating values
Rating_values <- seq(100, 1000, by = 100)

# Initialize variables
n_predictions <- 2000
n_Rating_values <- length(Rating_values)
#lower_bounds <- matrix(0, nrow = n_Rating_values, ncol = n_predictions)
#upper_bounds <- matrix(0, nrow = n_Rating_values, ncol = n_predictions)
lower_bounds <- rep(0, n_Rating_values)
upper_bounds <- rep(0, n_Rating_values)
# Set seed for reproducibility
set.seed(321)

# Compute predictions and prediction intervals
for (i in 1:n_Rating_values) {
  # Generate new observations for the given Rating value
  new_Rating <- rep(Rating_values[i], n_predictions)
  epsilon <- rnorm(n_predictions, mean = 0, sd = s)
  # Compute predictions
  new_Balance_hat <- B0_hat + B1_hat * new_Rating + epsilon
  
  # Compute residuals and residual standard error
  residuals <- Balance - B0_hat - B1_hat * Rating
  s_residuals <- sqrt(sum(residuals^2) / (n - 2))
  
  # Compute t-value for the given alpha level
  alpha <- 0.10
  t_value <- qt(1 - alpha / 2, df = n - 2)
  # Compute prediction intervals
  lower_bounds[i] <- quantile(new_Balance_hat, probs = alpha / 2)
  upper_bounds[i] <- quantile(new_Balance_hat, probs = 1 - alpha / 2)
}

# Create scatterplot with prediction intervals
png("Scatterplot of Rating vs Balance.png",width = 800, height = 600)
plot(Rating, Balance, xlab = "Rating", ylab = "Balance", main = "Scatterplot of Rating vs Balance" ,xlim = c(0, 1000),ylim=c(min(lower_bounds),max(upper_bounds)))
points(Rating_values, lower_bounds, col = "blue")
points(Rating_values, upper_bounds, col = "red")
lines(Rating_values, upper_bounds,  lty = "dashed" ,pch = 16, col = "red")
lines(Rating_values, lower_bounds, lty = "dashed",pch = 16, col = "blue")
# Save and close the plot
dev.off()

###########################################################################################################################

# Exrecise 2
# i

# Set seed for reproducibility
set.seed(321)

# Split the data into training and testing sets
n <- nrow(Credit)
n_train <- floor(0.8 * n)
train_indices <- sample(n, n_train)
train_data <- Credit[train_indices, ]
test_data <- Credit[-train_indices, ]

# Define the range of polynomial orders
p_values <- 1:10

# Initialize variables
n_p_values <- length(p_values)
pMSE <- rep(0, n_p_values)

# Fit polynomial regression models and compute pMSE
for (i in 1:n_p_values) {
  # Fit a polynomial regression model of order p
  p <- p_values[i]
  model <- lm(Balance ~ poly(Rating, p), data = train_data)
  
  # Predict the Balance values for the test data
  y_hat <- predict(model, newdata = test_data)
  
  # Compute the pMSE for the test data
  pMSE[i] <- mean((y_hat - test_data$Balance)^2)
}

# Plot the square root of pMSE vs. p
plot(sqrt(pMSE) ~ p_values, type = "b", xlab = "Polynomial Order", ylab = "Square Root of pMSE")

##############################################################################################################

#iii


# Set seed for reproducibility
set.seed(321)

# Define the range of polynomial orders
p_values <- 1:10

# Initialize matrix to store pMSE values
n_runs <- 200
pMSE_matrix <- matrix(0, nrow = n_runs, ncol = length(p_values))

# Perform model selection for 200 runs
for (run in 1:n_runs) {
  # Split the data into training and testing sets
  n <- nrow(Credit)
  n_train <- floor(0.8 * n)
  train_indices <- sample(n, n_train)
  train_data <- Credit[train_indices, ]
  test_data <- Credit[-train_indices, ]
  
  # Fit polynomial regression models and compute pMSE
  for (i in 1:length(p_values)) {
    # Fit a polynomial regression model of order p
    p <- p_values[i]
    model <- lm(Balance ~ poly(Rating, p), data = train_data)
    
    # Predict the Balance values for the test data
    y_hat <- predict(model, newdata = test_data)
    
    # Compute the pMSE for the test data
    pMSE_matrix[run, i] <- mean((y_hat - test_data$Balance)^2)
  }
}

# Calculate median and quantiles of sqrt(pMSE) values
sqrt_pMSE_median <- apply(sqrt(pMSE_matrix), 2, median)
sqrt_pMSE_quantiles <- apply(sqrt(pMSE_matrix), 2, quantile, probs = c(0.2, 0.8))

# Plot the median and quantiles of sqrt(pMSE) values
plot(p_values, sqrt_pMSE_median, type = "b", xlab = "Polynomial Order", ylab = "sqrt(pMSE)",
     ylim = c(min(sqrt_pMSE_quantiles), max(sqrt_pMSE_quantiles)))
lines(p_values, sqrt_pMSE_quantiles[1, ], lty = "dashed")
lines(p_values, sqrt_pMSE_quantiles[2, ], lty = "dashed")

# Add lines connecting the 0.2 and 0.8 quantiles
alpha <- 0.2
lines(p_values, apply(sqrt(pMSE_matrix), 2, quantile, alpha),
      lty = "dashed", col = "red")
alpha <- 0.8
lines(p_values, apply(sqrt(pMSE_matrix), 2, quantile, alpha),
      lty = "dashed", col = "red")





