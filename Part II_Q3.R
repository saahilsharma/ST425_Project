source("mvrnorm.R")

## Question 3 ##
p = 3
mu = c(0,0,0)
var = 0.5
n = 100000
correlations = seq(-0.99, 0.99, by=0.01)
use_cholesky = F

##### PART a ##################################################################

compute_prob_part_a = function(XDF){
  # First, lets condition on whether X2 > 0, which then intuitively
  # (and numerically) changes the sample size
  X2GtThanZero = XDF[XDF$V2 > 0,]
  new_n = nrow(X2GtThanZero) # New sample size
  
  # Base case: If new sample is zero, prob = 0
  if(new_n == 0) {
    return (0)
  }
  else {
    # We now count each row that has X1 AND X3 > 0
    result = X2GtThanZero[X2GtThanZero$V1 > 0 & X2GtThanZero$V3 > 0,]
    
    # Prob is simply the count divided by the "new" (conditional) sample size
    return (nrow(result)/new_n)  
  }
}

# Run simulation using our function
prob_3_part_a = 
  run_simulation(n,p,mu,var,correlations,use_cholesky = use_cholesky,
                 use_external_package = F, FUN = compute_prob_part_a)

# Run simulation using external package
prob_3_part_a_external_package = 
  run_simulation(n,p,mu,var,correlations,use_cholesky = use_cholesky,
                 use_external_package = T, FUN = compute_prob_part_a)

## Plot ours vs external
par(mfrow = c(2,1))

plot(x = correlations, y = prob_3_part_a, type = 'l', xlab = 'Correlation', 
     ylab = 'Probability', 
     main = 'P(X1 > 0, X3 > 0 | X2 > 0) - Using Our Method', col = "blue")

plot(x = correlations, y = prob_3_part_a_external_package, type = 'l', 
     xlab = 'Correlation', ylab = 'Probability', 
     main = 'P(X1 > 0, X3 > 0 | X2 > 0) - Using External Package', col = "red")


##### PART b ##################################################################

compute_prob_part_b = function(XDF){
  # First, lets condition on whether X2 < 0, which then intuitively
  # (and numerically) changes the sample size
  X2LtThanZero = XDF[XDF$V2 < 0,]
  new_n = nrow(X2LtThanZero) # New sample size
  
  # Base case: If new sample is zero, prob = 0
  if(new_n == 0) {
    return (0)
  }
  else {
    # We now count each row that has X1 AND X3 > 0
    result = X2LtThanZero[X2LtThanZero$V1 > 0 & X2LtThanZero$V3 > 0,]
    
    # Prob is simply the count divided by the "new" (conditional) sample size
    return (nrow(result)/new_n)  
  } 
}

# Run simulation using our function
prob_3_part_b = 
  run_simulation(n,p,mu,var,correlations,use_cholesky = use_cholesky,
                 use_external_package = F, FUN = compute_prob_part_b)

# Run simulation using external package
prob_3_part_b_external_package = 
  run_simulation(n,p,mu,var,correlations,use_cholesky = use_cholesky,
                 use_external_package = T, FUN = compute_prob_part_b)

## Plot ours vs external
par(mfrow = c(2,1))

plot(x = correlations, y = prob_3_part_b, type = 'l', xlab = 'Correlation', 
     ylab = 'Probability', 
     main = 'P(X1 > 0, X3 > 0 | X2 < 0) - Using Our Method', col = "blue")

plot(x = correlations, y = prob_3_part_b_external_package, type = 'l', 
     xlab = 'Correlation', ylab = 'Probability', 
     main = 'P(X1 > 0, X3 > 0 | X2 < 0) - Using External Package', col = "red")
