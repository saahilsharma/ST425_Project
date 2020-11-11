source("mvrnorm.R")

## Question 3 ##
p = 3
mu = c(0,0,0)
var = 0.5
n = 100000
correlations = seq(-0.99, 0.99, by=0.01)

# part a
prob_3_part_a = rep(0,length(correlations))

run_simulation(n,p,mu,var,correlations,FUN = function(index, XDF){
  # First, lets condition on whether X2 > 0, which then intuitively
  # (and numerically) changes the sample size
  X2GtThanZero = XDF[XDF$V2 > 0,]
  new_n = nrow(X2GtThanZero) # New sample size
  
  # Base case: If new sample is zero, prob = 0
  if(new_n == 0) {
    prob[i] = 0
  }
  else {
    # We now count each row that has X1 AND X3 > 0
    result = X2GtThanZero[X2GtThanZero$V1 > 0 & X2GtThanZero$V3 > 0,]
    
    # Prob is simply the count divided by the "new" (conditional) sample size
    prob_3_part_a[index] <<- nrow(result)/new_n  
  } 
})

# part b
prob_3_part_b = rep(0,length(correlations))

run_simulation(n,p,mu,var,correlations,FUN = function(index, XDF){
  # First, lets condition on whether X2 < 0,which then intuitively
  # (and numerically) changes the sample size
  X2LtThanZero = XDF[XDF$V2 < 0,]
  new_n = nrow(X2LtThanZero) # New sample size
  
  # Base case: If new sample is zero, prob = 0
  if(new_n == 0) {
    prob[i] = 0
  }
  else {
    # We now count each row that has X1 AND X3 > 0
    result = X2LtThanZero[X2LtThanZero$V1 > 0 & X2LtThanZero$V3 > 0,]
    
    # Prob is simply the count divided by the "new" (conditional) sample size
    prob_3_part_b[index] <<- nrow(result)/new_n  
  } 
})

# Plotting the two together
par(mfrow = c(2,1))
plot(x = correlations, y = prob_3_part_a, type = 'l', xlab = 'Correlation', ylab = 'Probability', main = 'P(X1 > 0, X3 > 0 | X2 > 0)')
plot(x = correlations, y = prob_3_part_b, type = 'l', xlab = 'Correlation', ylab = 'Probability', main = 'P(X1 > 0, X3 > 0 | X2 < 0)')