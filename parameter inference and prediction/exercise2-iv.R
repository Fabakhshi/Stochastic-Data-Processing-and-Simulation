# Exercise 2
# iv
library(leaps)
Credit <- read.table("Credit.dat") # loads the Credit dataset
attach(Credit) # so you can access its variables by name

# Set the random seed for reproducibility
set.seed(321)

allmodels <- regsubsets(Balance ~ Income + Limit + Rating + Cards + Age + Education,data=Credit,method=c("exhaustive"),intercept=TRUE,nbest = 100, really.big=TRUE)
out <-summary(allmodels,matrix=TRUE)
Models <- out$which

# Create the design matrix for the largest model
largestmodel <- lm(Balance ~ Income + Limit + Rating + Cards + Age + Education,x=TRUE)
X <- largestmodel$x # design matrix of the model with all covariates

# Split X and y into training and test data
train_idx <- sample(nrow(X), round(0.8*nrow(X)), replace = FALSE)
X_train <- X[train_idx, ]
X_test <- X[-train_idx, ]
y_train <- Balance[train_idx]
y_test <- Balance[-train_idx]


# Create a matrix to store the results
sqrt_pMSE_matrix <- matrix(NA, nrow = 2^6 - 1, ncol = 2)
colnames(sqrt_pMSE_matrix) <- c("sqrt_pMSE","model_names")

# Create a vector to store the model numbers for axis
model_numbers <- rep("", nrow(Models))

# Loop through all possible models
for (i in 1:(2^6 - 1)) {
  
  # Create the training design matrix for this model
  X_train_model <- X_train[,Models[i,]]
  
  # Compute beta-hat "by hand" for this model
  beta_hat <- solve(t(X_train_model) %*% X_train_model) %*% t(X_train_model) %*% y_train
  
  # Create the test design matrix for this model
  X_test_model <- X_test[,Models[i,]]
  
  # Compute the predicted values and pMSE for this model
  y_pred <- X_test_model %*% beta_hat
  pMSE <- mean((y_test - y_pred)^2) / var(y_test)
  sqrt_pMSE <- sqrt(pMSE)
  
  # Store the sqrt(pMSE) value and model name in the matrices
  sqrt_pMSE_matrix[i,1] <- sqrt_pMSE
  sqrt_pMSE_matrix[i,2]<- paste(colnames(X_train_model), collapse = "+")
  model_numbers[i] <- paste("[",i,"]_",ncol(X_train_model)-1) # indices of model and the number of covariates
}
  
# Plot the sqrt(pMSE) values for each model
plot(sqrt_pMSE_matrix[,1], type = "b", pch = 19, xaxt = "n", xlab = "", ylab = "sqrt(pMSE)")
axis(1, at = 1:length(model_numbers), labels = model_numbers, las = 2, cex.axis = 0.8)
