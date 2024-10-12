# Exercise 1
# *
Credit <- read.table("Credit.dat") # loads the Credit dataset
attach(Credit) # so you can access its variables by name
pairs(Credit) # a quick way to look at all pairwise relationships
# Yes, variables "Rating" and "Limit" seem to have a linear relation to the "Balance" variable.

#**
# To compute the estimated slope and intercept "by hand", first we should calculate the means of variables.
mean_balance <- mean(Balance)
mean_rate <- mean(Rating)
# E(balance) = β0 + β1rate
# Compute the estimated slope
estimated_slope <- sum((Rating - mean_rate) * (Balance - mean_balance))/(sum((Rating - mean_rate) ^ 2))
# Compute the estimated intercept
estimated_intercept <- mean_balance - estimated_slope * mean_rate
# Print the results
cat("Estimated slope:", estimated_slope, "\n")
cat("Estimated intercept:", estimated_intercept)

# To compare these results with what is returned by `lm` function":
# Fit linear model using lm
model <- lm(Balance ~ Rating)
# Extract the estimated slope and intercept from the model
lm_slope <- coef(model)[2]
lm_intercept <- coef(model)[1]
# Print the results
cat("lm slope:", lm_slope, "\n")
cat("lm intercept:", lm_intercept)
# By comparing the results from the "by hand" computation and `lm` function, 
# you should find that the estimated slope and intercept values are the same.

#***
#The slope β1 is the variation in the expected response when the independent variable increases by 1 unit.
#Therefore for this example, the estimate ˆ β1 = 2.395 means that we expect an increase in the balance 
#as 2.395 when the rate increases by 1 unit.
#######################################################################################################################
# Exercise 2
# *
# Compute predicted values
predicted <- estimated_intercept + estimated_slope * Rating
# Compute residuals
residuals <- Balance - predicted
# Compute residual sum of squares (RSS)
rss <- sum(residuals^2)
# Compute degrees of freedom
df <- length(Balance) - 2
# Compute residual standard error (s)
s <- sqrt(rss / df)
# Print the result
cat("Residual standard error (s):", s)

#**
#The residual standard error (s) is a measure of the variability of the residuals in a linear regression model.
#It represents the average distance that the observed data points fall from the regression line.
#In other words, s tells us how much the actual data points deviate from the predicted values of the model.
#A smaller value of s indicates that the model fits the data well, while a larger value of s indicates that
#the model does not fit the data well and that there is more variability in the residuals.
#The value of s is important because it is used to compute confidence intervals and hypothesis tests for
#the coefficients of the linear regression model. A smaller value of s leads to narrower confidence intervals
#and more significant hypothesis tests, indicating greater precision and accuracy of the model.
#######################################################################################################################
# Exercise 3
# *
# Compute t-value for 0.80 confidence level
t_value <- qt(0.80, df)

# Compute standard error of the slope and intercept
se_slope <- s / sqrt(sum((Rating - mean_rate)^2))
se_intercept <- s * sqrt((1 / length(Rating)) + (mean_rate^2 / sum((Rating - mean_rate)^2)))

# Compute confidence intervals for the slope and intercept
slope_lower <- estimated_slope - t_value * se_slope
slope_upper <- estimated_slope + t_value * se_slope
intercept_lower <- estimated_intercept - t_value * se_intercept
intercept_upper <- estimated_intercept + t_value * se_intercept

# Print the results
cat("Confidence interval for beta_0:", round(intercept_lower, 2), "to", round(intercept_upper, 2), "\n")
cat("Confidence interval for beta_1:", round(slope_lower, 2), "to", round(slope_upper, 2))
confint(model,level=0.80)
# As you can see the results are the same.

#**
#The 80% confidence interval for β1 is a range of values that is likely to contain the true population slope coefficient with 80% confidence, based on the observed sample data. 
#In other words, if we were to repeat the sampling process many times and calculate the 80% confidence interval each time, 
# we would expect that about 80% of these intervals would contain the true population slope coefficient.
#The interpretation of the confidence interval for β1 depends on its values. If the interval contains zero, 
# there is no significant linear relationship between the predictor variable (rate) and the response variable (balance), 
#and rate changes do not significantly affect the balance. If the interval does not contain zero, 
# it means that there is a significant linear relationship between rate and balance, and that rate changes have a significant effect on balance.
#For our case, the confidence interval for β1 is (2.26, 2.53), it means that we can be 80% confident that the true population slope coefficient falls within this range.
#This indicates that there is a significant positive linear relationship between rate and balance and that a rate increase is associated with an increase in balance.

##########################################################################################################################################################################################
# Exercise 4
# *
Credit <- read.table("Credit.dat") # loads the Credit dataset
attach(Credit) # so you can access its variables by name

n <- length(Balance)
# From previous exrecises
s <- 249.6045
B0_true <- -299.247
B1_true <- 2.395

# Initialize variables
B0_hat <- numeric(2000)
B1_hat <- numeric(2000)
CI_B0 <- matrix(0, nrow = 2000, ncol = 2)
CI_B1 <- matrix(0, nrow = 2000, ncol = 2)

# Perform simulation and estimation
for (j in 1:2000) {
  # Step 1: Simulate dataset Dj
  epsilon <- rnorm(n, mean = 0, sd = s)
  D_balance <- B0_true + B1_true * Rating + epsilon
  # Fit D1 via linear regression
  model <- lm(D_balance ~ Rating)
  # Store parameter estimates for Dj
  B0_hat[j] <- coef(model)[1]
  B1_hat[j] <- coef(model)[2]
  # Compute confidence intervals for Dj
  CI_B0[j, ] <- confint(model,level=0.80)[1, ]
  CI_B1[j, ] <- confint(model,level=0.80)[2, ]
}
# Compute proportions of intervals that include the true values
prop_B0_included <- mean(CI_B0[, 1] <= B0_true & CI_B0[, 2] >= B0_true)
prop_B1_included <- mean(CI_B1[, 1] <= B1_true & CI_B1[, 2] >= B1_true)
# Print the results
cat("Proportion of intervals including B0_true:", prop_B0_included, "\n")
cat("Proportion of intervals including B1_true:", prop_B1_included)

#################################################################################################################################################

# Exercise 5
#*
# Density plot for B0_hat
plot(density(B0_hat), main = "Density plot of B0_hat", xlab = "B0_hat", ylab = "Density")

# Density plot for B1_hat
plot(density(B1_hat), main = "Density plot of B1_hat", xlab = "B1_hat", ylab = "Density")

#The resulting density plots show the distribution of the estimated values of B0 and B1. 
#The density plot of B0_hat shows that the estimates are centered around the true value of B0, 
# with some variability due to the randomness in the simulation process. The density plot of B1_hat shows a similar pattern, 
# with estimates centered around the true value of B1 and some variability due to sampling.
# Overall, these density plots provide a visual representation of the uncertainty associated with the parameter estimates 
# and can be used to assess the quality of the simulation and estimation process.

#**
#To check if the distribution of the estimated β's is approximately Gaussian, we can use the qqnorm function and
# If the points on the plot fall close to a straight line, it suggests that the sample is approximately normally distributed.
# QQ plot for B0_hat
qqnorm(B0_hat, main = "QQ plot of B0_hat")
qqline(B0_hat)

# QQ plot for B1_hat
qqnorm(B1_hat, main = "QQ plot of B1_hat")
qqline(B1_hat)

#The resulting plots show that the points on the qq plots for B0_hat and B1_hat roughly follow a straight line, 
# indicating that the distributions of the estimated parameters are approximately normal. This suggests that our simulation 
# and estimation process is working well and that the confidence intervals we obtained are reliable.

###################################################################################################################################################

