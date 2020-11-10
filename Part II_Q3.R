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
  X2GtThanZero = XDF[XDF$V2 > 0,]
  new_n = nrow(X2GtThanZero)
  if(new_n == 0) {
    prob[i] = 0
  }
  else {
    X1GtThanZero = X2GtThanZero[X2GtThanZero$V1 > 0,]
    result = X1GtThanZero[X1GtThanZero$V3 > 0,]
    prob_3_part_a[index] <<- nrow(result)/new_n  
  } 
})

# part b
prob_3_part_b = rep(0,length(correlations))

run_simulation(n,p,mu,var,correlations,FUN = function(index, XDF){
  X2LtThanZero = XDF[XDF$V2 < 0,]
  new_n = nrow(X2LtThanZero)
  if(new_n == 0) {
    prob[i] = 0
  }
  else {
    X1GtThanZero = X2LtThanZero[X2LtThanZero$V1 > 0,]
    result = X1GtThanZero[X1GtThanZero$V3 > 0,]
    prob_3_part_b[index] <<- nrow(result)/new_n  
  } 
})

# Plotting the two together
par(mfrow = c(2,1))
plot(x = correlations, y = prob_3_part_a, type = 'l', xlab = 'Correlation', ylab = 'Probability', main = 'P(X1 > 0, X3 > 0|X2 > 0)')
plot(x = correlations, y = prob_3_part_b, type = 'l', xlab = 'Correlation', ylab = 'Probability', main = 'P(X1 > 0, X3 > 0|X2 < 0)')