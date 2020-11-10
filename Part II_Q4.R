data<-read.table("mnData.txt")
p<-dim(data)[2]
n<-dim(data)[1]

MLMean = as.vector(colMeans(data))
MLMean

covML = function(data, MLMean) {
  MLcov = matrix(0,p,p)
  for (i in seq(1,n,1)) {
    x = as.matrix(data[i,]-MLMean)
    q = t(x)%*%x
    MLcov = MLcov + q
  }
  return (MLcov/n)
}

MLcov<-covML(data, MLMean)
MLcov

cov(data)
cor(MLcov)

MLrho<- function(MLcov) {
  p = nrow(MLcov)
  MLcor<-matrix(0,p,p)
  for (i in seq(1,p,1)) {
    for (j in seq(1,p,1)) {
      MLcor[i,j]<-MLcov[i,j]/(sqrt(MLcov[i,i] * MLcov[j,j]))
    }
  }
  print(MLcor)
  return (MLcor[2,1])  #following the rule from 1
}
corr = MLrho(MLcov)
corr
