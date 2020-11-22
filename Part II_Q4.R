####### Question 4  ##############################

# Function to compute the sample covariance matrix
# Params:
#     data:- Dataset containing p-variate vectors
#     MLMean:- Vector of p means
compute_mle_cov = function(data, MLMean) {
  MLE_cov = matrix(0,p,p) # Initiliase Cov matrix
  for (i in 1:n) {
    # For each vector, subtract the mean
    x = as.matrix(data[i,]-MLMean)
    # Multiply the x with its transpose
    q = t(x)%*%x
    # Add to the previous value
    MLE_cov = MLE_cov + q
  }
  # Now just divide the whole thing by the sample size
  return (MLE_cov/n)
}

# Function to compute the Correlation matrix using
# corr(x,y) = cov(x,y)/sd(x)*sd(y)
# Params:
#   cov :- Covariance Matrix
compute_corr = function(cov) {
  p = nrow(cov)
  corr = matrix(0,p,p)
  for (i in seq(1,p,1)) {
    for (j in seq(1,p,1)) {
      corr[i,j] = cov[i,j]/(sqrt(cov[i,i] * cov[j,j]))
    }
  }
  return (corr)
}

######### Start of Script ##########################

# Read the data from the file
data=read.table("mnData.txt")

# P is the dimensionality
p=ncol(data)

# n is the number of p-variate normal vectors
n=nrow(data)

# The MLE of the population mean of each r.v. is just the sample mean
# This is explained further in the report
MLE_Mean = as.vector(colMeans(data))

# Compute the Covariance Matrix using the MLE approach
MLE_Cov = compute_mle_cov(data, MLE_Mean)

# Compute the Correlation Matrix using sample variance
corr = compute_corr(MLE_Cov)

# Display our Results to two decimal places
round(MLE_Mean,2)
round(MLE_Cov,2)
round(corr, 2)

# Display in built R package results
round(cov(data),2)
round(cor(data),2)



