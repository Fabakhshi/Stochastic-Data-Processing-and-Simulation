#Question1
import random

def f(x):
    return x**3

M = 10000  # choose the desired sample size

sum_f = 0 # Initialize a variable to store the sum of function evaluations

# Generate random numbers between 0 and 1, and evaluate the function f(x) for each random number:
for _ in range(M):
    x = random.uniform(0, 1)
    sum_f += f(x)

# Estimate θ using the Crude Monte Carlo method:    
theta_estimation_CRUDE = sum_f / M

print("Estimated value of θ (CrudeMC):", theta_estimation_CRUDE)
######################################################################
#Question2

# Initialize variables to store the counts of hits and total points:
hits = 0
total_points = 0

#Generate random points (x, y) within the unit square [0, 1] x [0, 1] and evaluate the function f(x) for each point:
for _ in range(M):
    x = random.uniform(0, 1)
    y = random.uniform(0, 1)
    
    if y <= f(x):
        hits += 1
    
    total_points += 1
    
# Estimate θ using the hit-or-miss Monte Carlo method:
theta_estimation_HOM= hits / total_points
print("Estimated value of θ(HOM):", theta_estimation_HOM)
##############################################################
#Question4

import numpy as np

# Define the theta from the question 1:
theta=1/4

# Define the number of simulations M and the sample sizes N:

M = 1000  # number of simulations
N_values = [10, 100, 1000, 10000]  # different sample sizes to try


# Initialize variables to store the RMSE values for each estimator and each sample size:

rmse_MC = []
rmse_HM = []


# For each sample size N, perform M Monte Carlo simulations and estimate θ using both the crude and hit-or-miss Monte Carlo methods:

for N in N_values:
    theta_MC = []
    theta_HM = []
    
    for _ in range(M):
        # Crude Monte Carlo method
        sum_f = 0
        for _ in range(N):
            x = random.uniform(0, 1)
            sum_f += f(x)
        theta_MC.append(sum_f / N)
        
        # Hit-or-miss Monte Carlo method
        hits = 0
        for _ in range(N):
            x = random.uniform(0, 1)
            y = random.uniform(0, 1)
            if y <= f(x):
                hits += 1
        theta_HM.append(hits / N)
    
    # Compute RMSE for each estimator and add to the corresponding list
    rmse_MC.append(np.sqrt(np.mean((np.array(theta_MC) - theta)**2)))
    rmse_HM.append(np.sqrt(np.mean((np.array(theta_HM) - theta)**2)))


# Print the RMSE values for each estimator and each sample size:

print("Sample size\tCrude MC\tHit-or-miss MC")
for i in range(len(N_values)):
    print("{}\t\t{:.4f}\t\t{:.4f}".format(N_values[i], rmse_MC[i], rmse_HM[i]))
####################################################################################
#Question5
import matplotlib.pyplot as plt


theta=1/4  # true value of θ
M_values = [2**i for i in range(1, 21)]

theta_MC = []
theta_HM = []
error_MC = []
error_HM = []

for M in M_values:
    # Crude Monte Carlo method
    sum_f = 0
    for _ in range(M):
        x = random.uniform(0, 1)
        sum_f += f(x)
    theta_MC.append(sum_f / M)
    error_MC.append(abs(theta_MC[-1] - theta))
    
    # Hit-or-miss Monte Carlo method
    hits = 0
    for _ in range(M):
        x = random.uniform(0, 1)
        y = random.uniform(0, 1)
        if y <= f(x):
            hits += 1
    theta_HM.append(hits / M)
    error_HM.append(abs(theta_HM[-1] - theta))

plt.loglog(M_values, error_MC, label="Crude MC")
plt.loglog(M_values, error_HM, label="Hit-or-miss MC")
plt.loglog(M_values, 1/np.sqrt(M_values), label='Theoretical reference slope $M^{-1/2}$', linestyle='--')
plt.xlabel("Sample size (M)")
plt.ylabel("Absolute error")
plt.legend()
plt.show()

##############################################################################################################
#Question6


import numpy as np
import matplotlib.pyplot as plt

# Define the function and the value of theta
def f(x):
    return x**3

theta=1/4  # true value of θ

# Define the number of experiments
N = 10

# Define the sequence of samples for Monte Carlo
M_values = [2**i for i in range(1, 21)]

# Initialize lists to store RMSE
crude_rmse_values = []
hit_or_miss_rmse_values = []

for M in M_values:
    # Initialize lists to store errors for each experiment
    crude_errors = []
    hit_or_miss_errors = []
    
    for _ in range(N):
        # Generate random samples
        U = np.random.uniform(0, 1, M)
        
        # Crude Monte Carlo
        crude_estimate = np.mean(f(U))
        crude_error = (crude_estimate - theta)**2
        crude_errors.append(crude_error)
        
        # Hit-or-Miss Monte Carlo
        hit_or_miss_estimate = np.mean(U < theta)
        hit_or_miss_error = (hit_or_miss_estimate - theta)**2
        hit_or_miss_errors.append(hit_or_miss_error)
    
    # Compute RMSE and add to the list
    crude_rmse = np.sqrt(np.mean(crude_errors))
    hit_or_miss_rmse = np.sqrt(np.mean(hit_or_miss_errors))
    
    crude_rmse_values.append(crude_rmse)
    hit_or_miss_rmse_values.append(hit_or_miss_rmse)

# Plot RMSE in a loglog plot
plt.loglog(M_values, crude_rmse_values, label='Crude Monte Carlo')
plt.loglog(M_values, hit_or_miss_rmse_values, label='Hit-or-Miss Monte Carlo')
plt.loglog(M_values, 1/np.sqrt(M_values), label='Theoretical reference slope $M^{-1/2}$', linestyle='--')

plt.xlabel('Number of samples (M)')
plt.ylabel('Root Mean Squared Error')
plt.legend()
plt.show()


