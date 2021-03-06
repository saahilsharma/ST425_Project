#install.packages("matrixcalc")
library(matrixcalc)
# Part II - Question 1 ##

# This function will will generate the pseudo-random numbers from a
# multi-variate normal distribution
# Parameters:
#   n = Number of p-variate random vectors to generate
#   mu = Mean vector of random variables
#   sigma = Covariance matrix
#   use_cholesky = Boolean to signify whether we should use Cholesky or 
#                  Eigen Decomposition (Default is Eigen)
mvrnorm = function(n, mu, sigma, use_cholesky = F) {
  p = length(mu) # Dimensionality
  
  # Return error if dimensions don't line up
  if(p != ncol(sigma)) 
    stop("The dimensions of sigma and mean vector aren't the same") 
  
  # Return an error if sigma is not positive semi definite
  if(!is.positive.semi.definite(sigma)) 
    stop("Sigma is not positive semi definite")
  
  # First, lets generate the standard normal r.v.s., Z ~ N(0,1)
  Z = matrix(rnorm(n*p), byrow = T, ncol = p)
  
  if(use_cholesky) 
  {
    # Use R's in built Cholesky function to decompose sigma
    # Full Explanation is given in the report
    sigma_decom = chol(sigma)
  } 
  else 
  {
    # Use eigen decomposition to simulate the multivariate normal dist
    # Explanation of this process is given in the Report
    eigens = eigen(sigma, symmetric = T)
    evals = eigens$values
    evectors = eigens$vectors
    D = sqrt(diag(evals))
    sigma_decom = evectors %*% D %*% t(evectors)
  }
  
  # Use the decomposed Sigma to generate the p-variate normal r.v.s
  X = matrix(rep(mu, n), byrow=T, ncol=p) + Z %*% sigma_decom
  
  return (X)
}

# This function produces a symmetric covariance matrix using the correlation
# provided where the covariance of the i-th and j-th element is computed
# by corr^(|j-i|)
# Parameters:
#   p = Dimensionality
#   var = Variance of every r.v. (we assume all r.v.s have the same variance)
#   base_corr = Base correlation
build_sigma = function(p, var, base_corr) {
  sigma = matrix(0L, nrow = p, ncol = p) # Initialise sigma
  for(i in 1:p) {
    for(j in i:p) {
      corr = base_corr^(abs(j-i)) # Based on equation in problem set
      # As [correlation(x,y) = cov(x,y)/sqrt(var(x).var(y))] and var(x)=var(y).
      # Re-arranging, this gives us [cov(x,y) = var(x) * corr(x,y)
      cov = var * corr 
      sigma[i,j] = cov
      sigma[j,i] = cov
    }
  }
  return (sigma)
}

# Helper function to allow us to run simulations across various different 
# correlations
# Parameters:
#   n = Number of variables to generate
#   p = Dimensionality
#   var = Variance of every r.v. (we assume all r.v.s have the same variance)
#   correlations = List of correlations used to simulate different sigma's
#   use_cholesky = Boolean to specify whether we want to use 
#                  cholesky decomp or eigen (default)
#   use_external_package = Boolean to indicate if we want to use an external 
#                          package to generate the multi-variate vectors 
#                         (default is False)
#   FUN = This is a function that accepts the dataframe of n p-variate normal 
#         generated by mvrnorm (defined above) or the external package.
#         This allows the the caller to work with 
#         the output of each simulation. The function should return the prob.
run_simulation = function(n, p, mu, var, correlations, use_cholesky = F, 
                          use_external_package = F, FUN) {
  prob = rep(0,length(correlations))
  for(i in 1:length(correlations)) {
    sigma = build_sigma(p,var, correlations[i])
    
    if(use_external_package) {
      method = if(use_cholesky) "chol" else "eigen"
      X = mvtnorm::rmvnorm(n, mean = mu, sigma = sigma, method = method)
    } else {
      X = mvrnorm(n, mu, sigma, use_cholesky = use_cholesky)
    }
    XDF = as.data.frame(X)
    prob[i] = FUN(XDF)
  }
  
  return (prob)
}

