source("mvrnorm.R")
library(mvtnorm)

## Question 2 ##
p = 8
mu = c(-1,1,-1,1,-1,1,-1,1)
var = 1.5
n = 100000
correlations = seq(-0.95, 0.95, by=0.01)

# part a
prob_2_part_a = rep(0,length(correlations))

run_simulation(n,p,mu,var,correlations,FUN = function(index, XDF){
  t = XDF[XDF$V1 > -1 & XDF$V1 < 4 & XDF$V2 > -1 & XDF$V2 < 4 & XDF$V3 > -1 & XDF$V3 < 4 & XDF$V4 > -1 & XDF$V4 < 4 & XDF$V5 > -1 & XDF$V5 < 4 & XDF$V6 > -1 & XDF$V6 < 4 & XDF$V7 > -1 & XDF$V7 < 4 & XDF$V8 > -1 & XDF$V8 < 4,]
  t = na.omit(t)
  prob_2_part_a[index] <<- nrow(t)/n
})
plot(correlations, prob_part_a, type = "l")


# part b
prob_2_part_b = rep(0,length(correlations))

run_simulation(n,p,mu,var,correlations,FUN = function(index, XDF){
  t = abs(XDF$V1) + abs(XDF$V2) + abs(XDF$V3) + abs(XDF$V4) + abs(XDF$V5) + abs(XDF$V6) + abs(XDF$V7) + abs(XDF$V8) 
  prob_2_part_b[index] <<- length(t[t<=8])/n
})

# Plotting the results
par(mfrow = c(2,1))
plot(x = correlations, y = prob_2_part_a, type = 'l', xlab = 'Correlation', ylab = 'Probability', main = 'P(-1<Xj<4, j=1,...,p) through simulation')
plot(x = correlations, y = prob_2_part_b, type = 'l', xlab = 'Correlation', ylab = 'Probability', main = 'P(Sum(|Xj|)<=8) through simulation')