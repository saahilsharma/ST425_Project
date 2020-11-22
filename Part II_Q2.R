source("mvrnorm.R")

## Question 2 ##
p = 8
mu = c(-1,1,-1,1,-1,1,-1,1)
var = 1.5
n = 100000
correlations = seq(-0.95, 0.95, by=0.01)
use_cholesky = F

##### PART a ##################################################################
compute_prob_part_a = function(XDF) {
  # Check if each r.v. is -1 < X < 4 
  t = XDF[XDF$V1 > -1 & XDF$V1 < 4 & XDF$V2 > -1 & XDF$V2 < 4 & 
            XDF$V3 > -1 & XDF$V3 < 4 & XDF$V4 > -1 & XDF$V4 < 4 & 
            XDF$V5 > -1 & XDF$V5 < 4 & XDF$V6 > -1 & XDF$V6 < 4 & 
            XDF$V7 > -1 & XDF$V7 < 4 & XDF$V8 > -1 & XDF$V8 < 4,]
  t = na.omit(t)
  
  # Probability is just the number of rows where the above condition was true
  # divided by the sample size (n)
  return (nrow(t)/n)
}

# Run the simulation using our multi-variate normal function
prob_2_part_a = 
  run_simulation(n,p,mu,var,correlations,use_cholesky = use_cholesky,
                 use_external_package = F, FUN = compute_prob_part_a)

# Run the simulation using ext package to generate multi-variate normal vectors
prob_2_part_a_external_package = 
  run_simulation(n,p,mu,var,correlations,use_cholesky = use_cholesky,
                 use_external_package = T, FUN = compute_prob_part_a)

## Plot ours vs external
par(mfrow = c(2,1))

plot(x = correlations, y = prob_2_part_a, type = 'l', xlab = 'Correlation', 
     ylab = 'Probability', 
     main = 'P(-1<Xj<4, j=1,...,p) - Using Our Method', col = "blue")

plot(x = correlations, y = prob_2_part_a_external_package, type = 'l', 
     xlab = 'Correlation', ylab = 'Probability', 
     main = 'P(-1<Xj<4, j=1,...,p) - Using External Package', col = "red")


##### PART b ##################################################################

compute_prob_part_b = function(XDF) {
  # Check if the absolute sum of X1...X8 <= 8
  t = rowSums(abs(XDF))
  return (length(t[t<=8])/n)
}

# Run the simulation using our multi-variate normal function
prob_2_part_b = 
  run_simulation(n,p,mu,var,correlations,use_cholesky = use_cholesky,
                 use_external_package = F,compute_prob_part_b)

# Run the simulation using ext package to generate multi-variate normal vectors
prob_2_part_b_external_package = 
  run_simulation(n,p,mu,var,correlations,use_cholesky = use_cholesky,
                 use_external_package = T,FUN = compute_prob_part_b)

## Plot ours vs external
par(mfrow = c(2,1))

plot(x = correlations, y = prob_2_part_b, type = 'l', xlab = 'Correlation', 
     ylab = 'Probability', 
     main = 'P(Sum(|Xj|)<=8) - Using our Method', col = "blue")

plot(x = correlations, y = prob_2_part_b_external_package, type = 'l', 
     xlab = 'Correlation', ylab = 'Probability', 
     main = 'P(Sum(|Xj|)<=8) - Using External Package', col = "red")


